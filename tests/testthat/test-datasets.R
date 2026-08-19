# Tests for dataset-record parsing (binary.R) and the cran_datasets detail
# table (export.R). Dataset records are emitted by rpkg-analyzer for every file
# under data/ and R/sysdata.rda; the pipeline stamps them with package+version
# and stores one row per dataset per version.

test_that("parse_analyzer_records collects dataset records into a frame", {
  lines <- c(
    '{"rec":"summary","package":"p","version":"1.0"}',
    '{"rec":"dataset","name":"mtcars","file":"data/mtcars.rda","internal":false,"format":"rda","format_version":2,"compression":"gzip","class":"data.frame","kind":"data.frame","nrow":32,"ncol":11,"has_rownames":true,"n_missing_total":0,"schema_fp":"aaa","shape_fp":"bbb","content_fp":"ccc","columns":[{"name":"mpg","type":"numeric","is_factor":false,"n_missing":0,"n_unique":25}],"row_sketch":["0001","0002"],"confidence":"exact"}',
    '{"rec":"dataset","name":"internal_df","file":"R/sysdata.rda","internal":true,"format":"rda","format_version":3,"compression":"xz","class":"S4:RangedSummarizedExperiment","s4_package":"SummarizedExperiment","kind":"RangedSummarizedExperiment","nrow":100,"ncol":8,"confidence":"degraded","notes":"s4-assay-dims"}'
  )
  ds <- parse_analyzer_records(lines)$datasets

  expect_equal(nrow(ds), 2L)
  expect_true(all(c("name", "file", "internal", "format", "format_version",
                    "compression", "class", "kind", "nrow", "ncol", "length",
                    "n_cols", "n_missing_total", "schema_fp", "shape_fp",
                    "content_fp", "s4_package", "confidence", "notes",
                    "columns", "row_sketch") %in% names(ds)))

  mt <- ds[ds$name == "mtcars", ]
  expect_equal(mt$nrow, 32L)
  expect_equal(mt$ncol, 11L)
  expect_equal(mt$content_fp, "ccc")
  expect_equal(mt$n_cols, 1L)          # derived from the columns array length
  expect_false(mt$internal)
  expect_true(grepl("mpg", mt$columns))       # nested columns kept as JSON
  expect_true(grepl("0001", mt$row_sketch))   # nested row_sketch kept as JSON

  sd <- ds[ds$name == "internal_df", ]
  expect_true(sd$internal)
  expect_equal(sd$s4_package, "SummarizedExperiment")
  expect_equal(sd$nrow, 100L)
  expect_equal(sd$confidence, "degraded")
})

test_that("a stream with no dataset records yields a zero-row frame", {
  ds <- parse_analyzer_records('{"rec":"summary","package":"p","version":"1.0"}')$datasets
  expect_equal(nrow(ds), 0L)
  expect_true("content_fp" %in% names(ds))
})

test_that(".empty_datasets_df matches the stamped dataset row shape", {
  empty <- .empty_datasets_df()
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("package", "version") == names(empty)[1:2]))
})

# One per-version dataset row, as analyze.R produces it (binary frame + stamps).
.mk_ds_row <- function(package, version, is_current, content_fp,
                       name = "d", schema_fp = "S1", internal = 0L) {
  data.frame(
    package = package, version = version,
    is_current = as.integer(is_current), fp_algo_version = 1L,
    name = name,
    file = if (internal) "R/sysdata.rda" else paste0("data/", name, ".rda"),
    internal = as.integer(internal),
    format = "rda", format_version = 2L, compression = "gzip",
    class = "data.frame", kind = "data.frame", nrow = 3L, ncol = 2L,
    length = NA_integer_, n_cols = 2L, n_missing_total = 0L,
    schema_fp = schema_fp, shape_fp = "SH", content_fp = content_fp,
    s4_package = NA_character_, confidence = "exact", notes = NA_character_,
    columns = '[{"name":"a","type":"integer"}]', row_sketch = '["0001","0002"]',
    stringsAsFactors = FALSE
  )
}

test_that(".write_datasets_normalized splits into four tables and dedups content across versions", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # Two versions of the same dataset (same content_fp).
  df <- rbind(.mk_ds_row("p", "1.0", FALSE, "C1"),
              .mk_ds_row("p", "1.1", TRUE,  "C1"))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, df, "p"))

  expect_setequal(
    DBI::dbListTables(con),
    c("cran_datasets", "cran_dataset_versions", "cran_dataset_contents", "cran_dataset_sketches"))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n
  expect_equal(count("cran_dataset_versions"), 2L)   # one link per version
  expect_equal(count("cran_dataset_contents"), 1L)   # content deduped across the two versions
  expect_equal(count("cran_datasets"),         1L)   # one identity row
  expect_equal(count("cran_dataset_sketches"), 1L)   # one sketch per distinct content
  expect_equal(DBI::dbGetQuery(con, "SELECT current_version FROM cran_datasets")$current_version, "1.1")
  # both version rows reconstruct to the same content
  cids <- DBI::dbGetQuery(con, "SELECT DISTINCT content_id FROM cran_dataset_versions")$content_id
  expect_length(cids, 1L)
})

test_that(".write_datasets_normalized collapses a dataset name colliding within one version", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n

  # A single package version can surface one dataset name twice: an exported
  # data/ object and an internal sysdata object of the same name. (package, name,
  # version) is unique in cran_dataset_versions, so the writer must collapse to
  # one row rather than fail the PK, keeping the exported copy.
  df <- rbind(
    .mk_ds_row("p", "1.0", TRUE, "CE", name = "d", internal = 0L),
    .mk_ds_row("p", "1.0", TRUE, "CI", name = "d", internal = 1L))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, df, "p"))

  expect_equal(count("cran_dataset_versions"), 1L)   # collapsed, no PK violation
  expect_equal(count("cran_datasets"),         1L)
  cid <- DBI::dbGetQuery(con, "SELECT content_id FROM cran_dataset_versions")$content_id
  fp  <- DBI::dbGetQuery(con,
    sprintf("SELECT content_fp FROM cran_dataset_contents WHERE content_id = %d", cid))$content_fp
  expect_equal(fp, "CE")                              # exported copy wins
  expect_equal(DBI::dbGetQuery(con, "SELECT internal FROM cran_datasets")$internal, 0L)
})

test_that(".write_datasets_normalized migrates away from the legacy flat table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # Legacy flat schema (pre-normalization): one row per dataset per version, no
  # current_version column. A database that still holds it must be migrated, not
  # appended to, or the identity write fails with "no column named current_version".
  DBI::dbExecute(con, "CREATE TABLE cran_datasets
    (package TEXT, version TEXT, name TEXT, file TEXT, internal INTEGER,
     columns TEXT, row_sketch TEXT)")
  DBI::dbExecute(con, "INSERT INTO cran_datasets (package, name, version)
                       VALUES ('old', 'd', '0.9')")
  # Summary carries the datasets_scanned marker set under the old design. In a
  # real shard upsert_shard has already written the current package's summary
  # (marker set) before the dataset write, so pre-set p as scanned here too.
  DBI::dbExecute(con, "CREATE TABLE cran_code_summary
    (package TEXT, version TEXT, datasets_scanned INTEGER)")
  DBI::dbExecute(con, "INSERT INTO cran_code_summary VALUES ('old','0.9',1), ('p','1.0',1)")

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.0", TRUE, "C1"), "p"))

  # Flat table replaced by the normalized identity + link tables.
  expect_true("current_version" %in% DBI::dbListFields(con, "cran_datasets"))
  expect_true(all(c("cran_dataset_versions", "cran_dataset_contents") %in% DBI::dbListTables(con)))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT current_version FROM cran_datasets WHERE package='p'")$current_version,
    "1.0")
  # A package scanned only under the old design is un-marked so it re-scans.
  expect_true(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary WHERE package='old'")$datasets_scanned))
  # The current shard's freshly written marker is preserved.
  expect_equal(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary WHERE package='p'")$datasets_scanned,
    1L)
})

test_that("re-analysis is idempotent and content dedups across packages", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n

  # Packages p and q ship the identical dataset (content C1).
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, rbind(.mk_ds_row("p", "1.0", TRUE, "C1"), .mk_ds_row("q", "1.0", TRUE, "C1")), c("p", "q")))
  expect_equal(count("cran_dataset_contents"), 1L)   # shared across packages
  expect_equal(count("cran_dataset_versions"), 2L)

  # Re-analyze p with the same data: no duplicate version or content rows.
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.0", TRUE, "C1"), "p"))
  expect_equal(count("cran_dataset_versions"), 2L)
  expect_equal(count("cran_dataset_contents"), 1L)
})

test_that(".gc_dataset_contents reclaims content orphaned by a data change", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.0", TRUE, "C1"), "p"))
  # Data changed on re-analysis: new content C2 written, C1 no longer referenced.
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.1", TRUE, "C2"), "p"))
  expect_equal(count("cran_dataset_contents"), 2L)   # C1 orphan + C2

  .gc_dataset_contents(con)
  expect_equal(count("cran_dataset_contents"), 1L)   # C1 reclaimed
  expect_equal(count("cran_dataset_sketches"), 1L)   # its sketch reclaimed too
})

# --- carrying what a newer analyzer describes --------------------------------
# A scan of the whole archive is expensive, and every one of these is a way for
# it to cost that and change nothing in the database.

.mk_wide_row <- function(package = "p", version = "1.0", content_fp = "C1",
                         origin_dir = "data", name = "d") {
  row <- .mk_ds_row(package, version, TRUE, content_fp, name = name)
  row$fp_algo_version <- 2L
  # Fields the analyzer describes that the tables have never seen.
  row$matrix_shape  <- "symmetric"
  row$matrix_uplo   <- "L"
  row$density       <- 0.125
  row$n_stored      <- 3L
  row$object_system <- "S4"
  row$is_spatial    <- TRUE
  row$origin_dir    <- origin_dir
  row
}

test_that("fields a newer analyzer describes reach the contents table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))

  got <- DBI::dbGetQuery(con, "SELECT * FROM cran_dataset_contents")
  expect_equal(got$matrix_shape, "symmetric")
  expect_equal(got$matrix_uplo, "L")
  expect_equal(got$density, 0.125)
  expect_equal(got$n_stored, 3L)
  expect_equal(got$object_system, "S4")
  expect_equal(got$is_spatial, 1L)         # logicals store as integers
})

test_that("where a dataset was found is identity, not content", {
  # origin_dir differs between two files holding the same bytes, so putting it
  # on the content row would give them two rows and break the dedup.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_wide_row(package = "q", origin_dir = "extdata"), "q"))

  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_dataset_contents")$n, 1L)
  expect_false("origin_dir" %in% DBI::dbListFields(con, "cran_dataset_contents"))
  ids <- DBI::dbGetQuery(con, "SELECT package, origin_dir FROM cran_datasets ORDER BY package")
  expect_equal(ids$origin_dir, c("data", "extdata"))
})

test_that("a table created before these fields existed is widened, not skipped", {
  # The incremental path runs against a database downloaded from the last
  # release, so a widened CREATE never applies to it. Without an ALTER the new
  # columns are dropped in silence and the scan that produced them is wasted.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE cran_dataset_contents (
    content_id INTEGER PRIMARY KEY,
    content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
    class TEXT, kind TEXT, nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
    UNIQUE (content_fp, schema_fp, fp_algo_version))")
  expect_false("matrix_shape" %in% DBI::dbListFields(con, "cran_dataset_contents"))

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))

  expect_true("matrix_shape" %in% DBI::dbListFields(con, "cran_dataset_contents"))
  expect_equal(DBI::dbGetQuery(con, "SELECT matrix_shape FROM cran_dataset_contents")$matrix_shape,
               "symmetric")
})

test_that("a re-scan under a new generation is stored rather than ignored", {
  # Same bytes, so the same content_fp: INSERT OR IGNORE drops the row unless
  # the generation is part of what makes it distinct. That is the whole reason
  # re-scanning with a better reader can reach the table at all.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  old <- .mk_ds_row("p", "1.0", TRUE, "C1")          # fp_algo_version 1
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, old, "p"))
  expect_true(is.na(DBI::dbGetQuery(con, "SELECT matrix_shape FROM cran_dataset_contents")$matrix_shape[[1]]) ||
              !("matrix_shape" %in% DBI::dbListFields(con, "cran_dataset_contents")))

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))
  got <- DBI::dbGetQuery(con,
    "SELECT fp_algo_version, matrix_shape FROM cran_dataset_contents ORDER BY fp_algo_version")
  expect_equal(got$fp_algo_version, c(1L, 2L))
  expect_equal(got$matrix_shape[[2]], "symmetric")
  # The version link points at the new generation, so the old row is
  # unreferenced and the contents GC reclaims it.
  .gc_dataset_contents(con)
  left <- DBI::dbGetQuery(con, "SELECT fp_algo_version FROM cran_dataset_contents")$fp_algo_version
  expect_equal(left, 2L)
})

# --- noticing that a scan is out of date -------------------------------------

.mk_summary_tbl <- function(con, rows) {
  DBI::dbWriteTable(con, "cran_code_summary", rows)
}

test_that("an analyzer upgrade puts the packages it already scanned back in the queue", {
  # The marker records that a package was scanned, not what scanned it, so
  # without this every package looks done after an upgrade and nothing re-runs.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = c("current", "older", "unknown"),
    datasets_scanned = c(TRUE, TRUE, TRUE),
    analyzer_version = c("0.3.1", "0.2.0", NA_character_),
    stringsAsFactors = FALSE))

  n <- .invalidate_stale_dataset_scans(con, "0.3.1")
  expect_equal(n, 2L)
  got <- DBI::dbGetQuery(con,
    "SELECT package, datasets_scanned FROM cran_code_summary ORDER BY package")
  # Only the row produced by the running build keeps its marker.
  expect_equal(got$package[!is.na(got$datasets_scanned)], "current")
})

test_that("nothing is invalidated when the running version cannot be determined", {
  # Clearing on a guess would re-scan the archive every run and never settle.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = "p", datasets_scanned = TRUE, analyzer_version = "0.2.0",
    stringsAsFactors = FALSE))

  expect_equal(.invalidate_stale_dataset_scans(con, NA_character_), 0L)
  expect_equal(.invalidate_stale_dataset_scans(con, ""), 0L)
  expect_true(DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1]][[1]] == 1L)
})

test_that("rows from before the version was recorded are all invalidated once", {
  # Nothing on them says which build produced them, so none can be shown to
  # match. The column appears on this run's write, so the branch is taken once.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = c("a", "b"), datasets_scanned = c(TRUE, NA),
    stringsAsFactors = FALSE))

  expect_equal(.invalidate_stale_dataset_scans(con, "0.3.1"), 1L)  # only the marked one
  left <- DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1]]
  expect_true(all(is.na(left)))
})

test_that("a settled archive is not re-queued on every run", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = c("a", "b"), datasets_scanned = c(TRUE, TRUE),
    analyzer_version = c("0.3.1", "0.3.1"), stringsAsFactors = FALSE))

  expect_equal(.invalidate_stale_dataset_scans(con, "0.3.1"), 0L)
  expect_equal(.invalidate_stale_dataset_scans(con, "0.3.1"), 0L)
})

test_that("versions describing different things still bind into one frame", {
  # .datasets_frame carries the fields its records actually had, so two versions
  # of one package differ in width as soon as they differ in what they hold.
  # Plain rbind stops on that, and the caller reads the error as the whole
  # package failing: it loses its summary, functions and edges too, and five
  # consecutive failures exclude it from the pipeline for good.
  v1 <- .datasets_frame(list(list(rec = "dataset", name = "s", class = "S4:X", nrow = 1L)))
  v2 <- .datasets_frame(list(list(rec = "dataset", name = "d", class = "data.frame",
                                  nrow = 3L, has_rownames = TRUE)))
  expect_false(ncol(v1) == ncol(v2))
  bound <- .rbind_datasets(list(v1, v2))
  expect_equal(nrow(bound), 2L)
  expect_true("has_rownames" %in% names(bound))
  expect_true(is.na(bound$has_rownames[bound$name == "s"]))
  expect_null(.rbind_datasets(list()))
})

test_that("the re-scan queue settles instead of clearing every marker forever", {
  # If the version column never appears, the column-absent branch fires on every
  # run: the whole archive is queued, the shard truncates to its alphabetical
  # prefix, and packages later in the alphabet are never reached again.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = c("a", "b"), datasets_scanned = c(TRUE, TRUE),
    stringsAsFactors = FALSE))

  # First run: nothing records which build produced these, so both are queued.
  expect_equal(.invalidate_stale_dataset_scans(con, "0.3.1"), 2L)
  # That run re-analyses them, and the write leaves the version behind.
  DBI::dbExecute(con, "ALTER TABLE cran_code_summary ADD COLUMN analyzer_version TEXT")
  DBI::dbExecute(con, "UPDATE cran_code_summary SET datasets_scanned = 1, analyzer_version = '0.3.1'")
  # Every run after that clears nothing.
  expect_equal(.invalidate_stale_dataset_scans(con, "0.3.1"), 0L)
  expect_equal(.invalidate_stale_dataset_scans(con, "0.3.1"), 0L)
  expect_equal(.invalidate_stale_dataset_scans(con, "0.3.1"), 0L)
})

test_that("how a file stores its data is recorded, not just what it holds", {
  # R's serialization format has versions, and a version 3 file cannot be read
  # by R before 3.5.0, so this is the difference between a dataset a reader can
  # open and one they cannot. It was being parsed and then dropped, along with
  # the on-disk size and the note saying how the file was read.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_wide_row()
  row$format_version   <- 3L
  row$compressed_bytes <- 4096L
  row$notes            <- "s4-dim-slot"
  row$shape_fp         <- "SHP1"
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  v <- DBI::dbGetQuery(con, "SELECT format_version, compressed_bytes, notes FROM cran_dataset_versions")
  expect_equal(v$format_version, 3L)
  expect_equal(v$compressed_bytes, 4096L)
  expect_equal(v$notes, "s4-dim-slot")
  # The shape fingerprint describes the data, so it sits with the data.
  expect_equal(DBI::dbGetQuery(con, "SELECT shape_fp FROM cran_dataset_contents")$shape_fp, "SHP1")
  expect_false("format_version" %in% DBI::dbListFields(con, "cran_dataset_contents"))
})

test_that("two versions of one dataset can differ in how they were stored", {
  # The same data saved twice under different serialization versions is one
  # content row and two version rows, so this has to live on the version.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  a <- .mk_ds_row("p", "1.0", FALSE, "C1"); a$format_version <- 2L; a$fp_algo_version <- 2L
  b <- .mk_ds_row("p", "1.1", TRUE,  "C1"); b$format_version <- 3L; b$fp_algo_version <- 2L
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, rbind(a, b), "p"))

  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_dataset_contents")$n, 1L)
  got <- DBI::dbGetQuery(con,
    "SELECT version, format_version FROM cran_dataset_versions ORDER BY version")
  expect_equal(got$format_version, c(2L, 3L))
})
