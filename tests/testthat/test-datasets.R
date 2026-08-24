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

# The generation the published database was written under. Every content row in
# it carries this number, so a re-scan that stamps the same number can never
# reach the table for data whose bytes have not changed.
.PUBLISHED_FP_ALGO_VERSION <- 2L

# The narrow profile an older reader produced, as the published rows hold it.
.mk_published_row <- function(package = "p", version = "1.0", content_fp = "C1") {
  row <- .mk_ds_row(package, version, TRUE, content_fp)
  row$fp_algo_version <- .PUBLISHED_FP_ALGO_VERSION
  row
}

# The same bytes read again by a reader that describes more of them, stamped the
# way analyze.R stamps a real scan. Every field here is one the published rows
# left NULL.
.mk_rescanned_row <- function(package = "p", version = "1.0", content_fp = "C1") {
  row <- .mk_ds_row(package, version, TRUE, content_fp)
  row$fp_algo_version <- FP_ALGO_VERSION
  row$matrix_diag     <- "unit"
  row$frequency       <- 12
  row$row_mean_mean   <- 1.5
  row$col_mean_mean   <- 2.5
  row$n_nan           <- 4L
  row$n_infinite_pos  <- 7L
  row$is_rowwise      <- TRUE
  row
}

test_that("the generation in the constant is ahead of the one already published", {
  # If it is not, the re-scan below is a no-op and roughly a hundred columns the
  # reader now fills stay NULL for as long as the bytes stay unchanged, which for
  # an archived package is forever.
  expect_gt(FP_ALGO_VERSION, .PUBLISHED_FP_ALGO_VERSION)
})

test_that("a published profile does not suppress the re-scan that widens it", {
  # The failure this guards is silent. INSERT OR IGNORE against
  # UNIQUE(content_fp, schema_fp, fp_algo_version) drops the second write when
  # the generation matches, so the fields the newer reader computed are
  # discarded without an error and the run reports success.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_published_row(), "p"))
  published <- DBI::dbGetQuery(con, "SELECT * FROM cran_dataset_contents")
  expect_equal(nrow(published), 1L)
  expect_true(is.na(published$matrix_diag[[1]]))
  expect_true(is.na(published$row_mean_mean[[1]]))

  # Same package, same dataset, same bytes: only the reader changed.
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_rescanned_row(), "p"))

  # Two rows now: the profile that was published and the one the re-scan wrote.
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_dataset_contents")$n, 2L)

  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM cran_dataset_contents WHERE fp_algo_version = %d", FP_ALGO_VERSION))
  expect_equal(nrow(got), 1L)
  expect_equal(got$matrix_diag[[1]], "unit")
  expect_equal(got$frequency[[1]], 12)
  expect_equal(got$row_mean_mean[[1]], 1.5)
  expect_equal(got$col_mean_mean[[1]], 2.5)
  expect_equal(got$n_nan[[1]], 4L)
  expect_equal(got$n_infinite_pos[[1]], 7L)
  expect_equal(got$is_rowwise[[1]], 1L)          # logicals store as integers

  # A profile nobody can read is not a fix: the version link has to move onto the
  # new row, or readers keep seeing the narrow one.
  linked <- DBI::dbGetQuery(con,
    "SELECT c.fp_algo_version fp, c.matrix_diag md
       FROM cran_dataset_versions v JOIN cran_dataset_contents c USING (content_id)")
  expect_equal(linked$fp, FP_ALGO_VERSION)
  expect_equal(linked$md, "unit")

  # The published row is now unreferenced, so the GC reclaims the space it held.
  .gc_dataset_contents(con)
  left <- DBI::dbGetQuery(con, "SELECT fp_algo_version FROM cran_dataset_contents")$fp_algo_version
  expect_equal(left, FP_ALGO_VERSION)
})

test_that("two packages shipping the same bytes still share one profile", {
  # The generation bump must not cost the dedup the content table exists for:
  # one content row for both packages, not one each.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_rescanned_row(), "p"))
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_rescanned_row(package = "q"), "q"))

  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_dataset_contents")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_dataset_versions")$n, 2L)
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

# --- how deeply a dataset's columns were read --------------------------------
# Past 512 columns the reader stops describing them one by one, and past its
# cell cap it stops reading their values at all. Which of the four it did is
# the legend for everything else on the record: at `none` there is no columns
# array and the whole-object figures stand in for it, at `reduced` each entry
# carries four fields and no col_fp, and at `structural` an entry is a name and
# a type because no value was read. A reader holding the array without the
# legend cannot tell an object with nothing to say from one that was not asked.
#
# The legend belongs beside the array it explains. Which depth a record reaches
# follows from the data: `full`, `reduced` and `none` from its width, the mix
# of its column types and how many cells they lay out to, and `structural` from
# the length of one column. Two files holding identical bytes are read to the
# same depth, so the answer is one the profile can hold once.

test_that("a profile says at what depth its columns were read", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_ds_row("p", "1.0", TRUE, "C1")
  row$column_detail <- "reduced"
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  expect_equal(
    DBI::dbGetQuery(con, "SELECT column_detail FROM cran_dataset_contents")$column_detail,
    "reduced")
  # Not on the version link. Repeating it there once per package that ships the
  # data says the same thing several times and invites the copies to disagree.
  expect_false("column_detail" %in% DBI::dbListFields(con, "cran_dataset_versions"))
})

test_that("a database that kept the depth on the version link is given it on the profile", {
  # These tables only ever gain columns, so a field that has changed table
  # leaves a copy behind on the old one that nothing will write to again. It
  # keeps whatever it held on the day it stopped being written, and every
  # reader that finds it there believes it.
  path <- withr::local_tempfile(fileext = ".db")
  con  <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE cran_dataset_versions (
      package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
      content_id INTEGER, format TEXT, compression TEXT, confidence TEXT,
      is_current INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (package, name, version))")
  DBI::dbExecute(con, "ALTER TABLE cran_dataset_versions ADD COLUMN notes TEXT")
  DBI::dbExecute(con, "ALTER TABLE cran_dataset_versions ADD COLUMN column_detail TEXT")
  DBI::dbExecute(con, "INSERT INTO cran_dataset_versions
      (package, name, version, content_id, format, is_current, notes, column_detail)
      VALUES ('p', 'kept', '1.0', 7, 'rda', 1, 'from before', 'reduced')")
  DBI::dbExecute(con, "CREATE TABLE cran_dataset_contents (
      content_id INTEGER PRIMARY KEY,
      content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
      class TEXT, kind TEXT, nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
      UNIQUE (content_fp, schema_fp, fp_algo_version))")
  DBI::dbDisconnect(con)

  con <- open_or_init_data_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("column_detail" %in% DBI::dbListFields(con, "cran_dataset_versions"))
  expect_true("column_detail" %in% DBI::dbListFields(con, "cran_dataset_contents"))
  # And the row it was carrying is otherwise untouched.
  kept <- DBI::dbGetQuery(con, "SELECT * FROM cran_dataset_versions")
  expect_equal(kept$name, "kept")
  expect_equal(kept$content_id, 7L)
  expect_equal(kept$notes, "from before")
})

test_that("a dataset the reader could not fingerprint keeps its place in the catalog", {
  # A frame with a column past the cell cap has its values skipped, so it comes
  # back with its shape, its column names and no fingerprints at all. The
  # writer dropped every record with no content fingerprint, so the whole
  # dataset left the catalog: no identity row, no version link, nothing saying
  # the package ships it.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  read   <- .mk_ds_row("p", "1.0", TRUE, "C1", name = "small")
  read$column_detail   <- "full"
  unread <- .mk_ds_row("p", "1.0", TRUE, NA_character_, name = "huge")
  unread$schema_fp     <- NA_character_
  unread$shape_fp      <- NA_character_
  unread$row_sketch    <- NA_character_
  unread$nrow          <- 9000000L
  unread$confidence    <- "degraded"
  unread$notes         <- "value scan skipped (size cap)"
  unread$column_detail <- "structural"
  # And the run says how many it kept that way, because a catalog entry with
  # nothing behind it is a coverage figure and a shard where the number climbs
  # is the reader losing objects it used to measure.
  expect_output(
    DBI::dbWithTransaction(
      con, .write_datasets_normalized(con, rbind(read, unread), "p")),
    "kept 1 dataset with no profile")

  ident <- DBI::dbGetQuery(con, "SELECT name FROM cran_datasets ORDER BY name")
  expect_equal(ident$name, c("huge", "small"))

  got <- DBI::dbGetQuery(con,
    "SELECT name, content_id, confidence, notes
       FROM cran_dataset_versions ORDER BY name")
  expect_equal(got$name, c("huge", "small"))
  # No profile to point at, and the row says why rather than pointing at
  # somebody else's: a fingerprint the reader did not take cannot be invented
  # without telling two objects it never compared that they are the same data.
  expect_true(is.na(got$content_id[[1L]]))
  expect_false(is.na(got$content_id[[2L]]))
  expect_equal(got$confidence[[1L]], "degraded")
  expect_equal(got$notes[[1L]], "value scan skipped (size cap)")

  # And only the fingerprinted one has a profile, so only it carries a depth.
  # A record with nothing behind it carries no depth either: confidence and
  # notes are the whole of what the catalog can say about it, which is the
  # honest answer and the one the run counts out loud.
  cts <- DBI::dbGetQuery(con,
    "SELECT column_detail FROM cran_dataset_contents")
  expect_equal(nrow(cts), 1L)
  expect_equal(cts$column_detail, "full")
})

test_that("the profile GC is not stopped by a version link with no profile", {
  # NOT IN over a column holding a NULL is NULL for every row, so one
  # unfingerprinted dataset anywhere in the table would quietly retire the GC
  # and the contents table would grow without bound.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  unread <- .mk_ds_row("q", "1.0", TRUE, NA_character_, name = "huge")
  unread$schema_fp <- NA_character_
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, rbind(.mk_ds_row("p", "1.0", TRUE, "C1"), unread), c("p", "q")))
  # p's data changes, orphaning the profile it used to point at.
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_ds_row("p", "1.1", TRUE, "C2"), "p"))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_dataset_contents")$n, 2L)

  .gc_dataset_contents(con)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT content_fp FROM cran_dataset_contents")$content_fp, "C2")
})

test_that("a database whose version links must name a profile is given one that need not", {
  # The published database carries the NOT NULL, so the record that has no
  # profile to name has to become writable under a run that opens an older
  # release rather than only in one built from nothing.
  path <- withr::local_tempfile(fileext = ".db")
  con  <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE cran_dataset_versions (
      package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
      content_id INTEGER NOT NULL, format TEXT, compression TEXT, confidence TEXT,
      is_current INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (package, name, version))")
  DBI::dbExecute(con, "ALTER TABLE cran_dataset_versions ADD COLUMN notes TEXT")
  DBI::dbExecute(con, "INSERT INTO cran_dataset_versions
      (package, name, version, content_id, format, is_current, notes)
      VALUES ('p', 'kept', '1.0', 7, 'rda', 1, 'from before')")
  DBI::dbDisconnect(con)

  con <- open_or_init_data_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # The rows that were there are still there, columns and all.
  kept <- DBI::dbGetQuery(con, "SELECT * FROM cran_dataset_versions")
  expect_equal(kept$name, "kept")
  expect_equal(kept$content_id, 7L)
  expect_equal(kept$notes, "from before")
  # And a link with nothing behind it is now writable.
  expect_silent(DBI::dbExecute(con, "INSERT INTO cran_dataset_versions
      (package, name, version, content_id, format, is_current)
      VALUES ('p', 'huge', '1.0', NULL, 'rda', 1)"))
  # The rebuild takes the table's indexes down with it, so they have to come
  # back or every join onto a profile turns into a scan.
  expect_true("idx_cran_dsv_content" %in%
    DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'index'")$name)
})

test_that("what the analyzer describes reaches the tables that hold it", {
  # Every one of these was read, carried through the frame, and then dropped at
  # the write because the column list had not heard of it. A scan that costs
  # eleven hours should not arrive and be discarded.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_wide_row()
  row$mean <- 2.5; row$sd <- 1.25; row$q1 <- 1.5; row$q3 <- 3.5
  row$sort_order <- "ascending"; row$n_zero <- 0L; row$p_zero <- 0
  row$levels <- '["a","b"]'; row$is_ordered <- TRUE
  row$frame_class <- "tibble"; row$dt_key <- '["id"]'
  row$inner_nrow_total <- 2000L; row$element_names <- '["train","test"]'
  row$dimnames <- '[{"margin":1,"labels":["A","B"]}]'
  row$index_delta <- 1; row$index_regular <- TRUE; row$ts_span <- 3.5
  row$resolution <- "[0.5,0.5]"; row$nodata_value <- -9999; row$in_memory <- TRUE
  row$n_nonzero <- 6L; row$skewness <- 1.5; row$n_outliers <- 2L
  row$title <- "Readings from an instrument"
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  got <- DBI::dbGetQuery(con, "SELECT * FROM cran_dataset_contents")
  expect_equal(got$mean, 2.5)
  expect_equal(got$sd, 1.25)
  expect_equal(got$sort_order, "ascending")
  expect_equal(got$inner_nrow_total, 2000L)
  expect_equal(got$n_nonzero, 6L)
  expect_equal(got$nodata_value, -9999)
  expect_equal(got$skewness, 1.5)
  expect_true(all(c("levels", "dimnames", "dt_key", "element_names", "resolution",
                    "index_delta", "ts_span", "n_outliers", "frame_class")
                  %in% names(got)))

  # A title belongs to the package's documentation, not to the bytes: two
  # packages carrying identical data may describe it differently.
  ident <- DBI::dbGetQuery(con, "SELECT title FROM cran_datasets")
  expect_equal(ident$title, "Readings from an instrument")
  expect_false("title" %in% DBI::dbListFields(con, "cran_dataset_contents"))
})

test_that("a columns profile too large to serve is refused, and the row says so", {
  # A single value over MySQL's max_allowed_packet cannot be loaded at all, and
  # that ceiling is not raisable: a misparsed file has already produced a
  # 306 MB profile here. The profile is what gets dropped, never the row.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  orig <- MAX_DATASET_COLUMNS_BYTES
  MAX_DATASET_COLUMNS_BYTES <<- 64L
  on.exit(MAX_DATASET_COLUMNS_BYTES <<- orig, add = TRUE)

  big <- .mk_ds_row("p", "1.0", TRUE, "C1")
  big$columns <- paste0('[{"name":"', strrep("x", 500L), '"}]')
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, big, "p"))

  got <- DBI::dbGetQuery(con,
    "SELECT class, nrow, columns, columns_refused_bytes FROM cran_dataset_contents")
  expect_equal(nrow(got), 1L)
  expect_true(is.na(got$columns))
  expect_equal(got$columns_refused_bytes, nchar(big$columns, type = "bytes"))
  # The rest of the profile is still true and still stored.
  expect_equal(got$class, "data.frame")
  expect_equal(got$nrow, 3L)
})

test_that("a columns profile within the bound is stored untouched", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_ds_row("p", "1.0", TRUE, "C1")
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  got <- DBI::dbGetQuery(con,
    "SELECT columns, columns_refused_bytes FROM cran_dataset_contents")
  expect_equal(got$columns, row$columns)
  # Zero rather than NULL: nothing was refused is a measurement, and a column
  # that is NULL for every healthy row reads to the coverage canary as dead.
  expect_equal(got$columns_refused_bytes, 0L)
})


# --- the build a row was collected under, on the rows the analyzer produced ---
# The re-scan queue clears the dataset marker on rows produced by a build other
# than the running one, and reads a row that names no build as one it cannot
# show to be current, so a scanned row has to name the build that scanned it or
# the queue never settles.
#
# It has to name it only when the analyzer really produced it. Writing the
# running build onto a row the pure-R fallback produced claims a producer that
# produced nothing, which is the same false claim datasets_scanned is withheld
# to avoid, and the two would then disagree about the same row.

.ds_run_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

# What build the stored rows name, if the column is there to name one at all.
# A database whose every row came from the fallback never grows the column,
# which is the same answer as a column full of NULLs and has to read as one.
.ds_named_builds <- function(con) {
  if (!"analyzer_version" %in% DBI::dbListFields(con, "cran_code_summary")) {
    return(NA_character_)
  }
  as.character(
    DBI::dbGetQuery(con, "SELECT analyzer_version FROM cran_code_summary")[[1L]])
}

# A package the analyzer read: the dataset marker and the build that earned it
# arrive together, because the reader that sets one is the producer that names
# the other. This is the shape analyze_package can actually return.
.ds_stub_analyze <- function(version = "0.4.0-test") {
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = TRUE, detail_scanned = TRUE,
      analyzer_version = version, stringsAsFactors = FALSE),
    api = data.frame(package = pkg, version = "1.0", exports_added = "[]",
      exports_removed = "[]", n_exports = 1L, stringsAsFactors = FALSE),
    churn = NULL, functions = NULL, edges = NULL, datasets = NULL,
    binary_versions = "1.0"), envir = env)
  old
}

# The other shape: the pure-R fallback ran, so there is no scan and no build.
.ds_stub_fallback <- function() {
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L,
      latest_release_date = "2026-01-01", datasets_scanned = NA, detail_scanned = TRUE,
      stringsAsFactors = FALSE),
    api = data.frame(package = pkg, version = "1.0", exports_added = "[]",
      exports_removed = "[]", n_exports = 1L, stringsAsFactors = FALSE),
    churn = NULL, functions = NULL, edges = NULL, datasets = NULL,
    binary_versions = character(0L)), envir = env)
  old
}

test_that("the run fills the build in on a row the analyzer produced without one", {
  df <- data.frame(package = c("a", "b"), version = c("1.0", "1.0"),
                   analyzer_version = c(NA_character_, NA_character_),
                   stringsAsFactors = FALSE)
  got <- .stamp_analyzer_version(df, "0.4.0-test", .analyzer_row_keys("a", "1.0"))
  expect_equal(got$analyzer_version, c("0.4.0-test", NA_character_))
})

test_that("a row the analyzer did not produce is left naming nobody", {
  # The pure-R fallback wrote this row. Stamping the running build on it would
  # say the analyzer collected data the analyzer never saw, and the re-scan
  # queue would then read a fallback row as one it has no reason to re-scan.
  df <- data.frame(package = "a", version = "1.0",
                   analyzer_version = NA_character_, stringsAsFactors = FALSE)
  got <- .stamp_analyzer_version(df, "0.4.0-test")
  expect_true(is.na(got$analyzer_version))
  expect_true(is.na(.stamp_analyzer_version(
    df, "0.4.0-test", .analyzer_row_keys("a", character(0L)))$analyzer_version))
})

test_that("a row the analyzer stamped keeps the build it names", {
  # The run fills a gap; it does not restate what the analyzer already said.
  # Overwriting would erase the one signal that tells a row collected by an
  # older build from one collected by this one.
  df <- data.frame(package = c("a", "b"), version = c("1.0", "1.0"),
                   analyzer_version = c("0.2.0", NA_character_),
                   stringsAsFactors = FALSE)
  got <- .stamp_analyzer_version(df, "0.4.0-test",
                                 .analyzer_row_keys("a", "1.0"))
  expect_equal(got$analyzer_version, c("0.2.0", NA_character_))
})

test_that("a run that cannot name its analyzer stamps nothing", {
  # Writing a guess would make every row look current and stop the re-scan
  # queue from ever noticing an upgrade.
  df <- data.frame(package = "a", version = "1.0",
                   analyzer_version = NA_character_, stringsAsFactors = FALSE)
  expect_true(is.na(.stamp_analyzer_version(
    df, NA_character_, .analyzer_row_keys("a", "1.0"))$analyzer_version))
  expect_false("analyzer_version" %in%
                 names(.stamp_analyzer_version(
                   data.frame(package = "a", stringsAsFactors = FALSE),
                   NA_character_)))
})

test_that("a run records the analyzer build on the rows the analyzer produced", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  old <- .ds_stub_analyze(version = NA_character_)
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.ds_run_io(), out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT analyzer_version FROM cran_code_summary")[[1L]],
    "0.4.0-test")
})

test_that("the scan marker survives a second run over the same universe", {
  # The failure this pins down is not a lost marker, it is a pipeline that
  # never reports itself finished: the package returns to the queue every run,
  # is re-analysed, and the run publishes a dated release for nothing.
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  old <- .ds_stub_analyze()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  io  <- .ds_run_io()
  run_update(io, out, shard_size = 10L)
  m2 <- run_update(io, out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1L]], 1L)
  expect_equal(m2$n_fresh, 0L)
  expect_false(m2$changed)
})

test_that("a fallback row reaches the database naming no build at all", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  old <- .ds_stub_fallback()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.ds_run_io(), out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Neither half of the claim. A row with no scan and a build against it says
  # the analyzer was here and read nothing, which is the state this pipeline
  # cannot tell from a package that ships no data.
  expect_true(all(is.na(.ds_named_builds(con))))
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1L]])))
})

test_that("a package the installed analyzer cannot read names no build either", {
  # The same thing without a stub in the way: a real analyze_package, a real
  # binary that answers --version and fails on the package.
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  out <- withr::local_tempdir()
  io  <- list(
    package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
      system2("git", c("-C", dest, "config", "user.email", "t@example.com"),
              stdout = FALSE, stderr = FALSE)
      system2("git", c("-C", dest, "config", "user.name", "T"),
              stdout = FALSE, stderr = FALSE)
      writeLines(c("Package: pkgA", "Version: 1.0", "Title: T",
                   "Description: A package for the fallback test.", "Author: T",
                   "Maintainer: T <t@example.com>", "License: MIT"),
                 file.path(dest, "DESCRIPTION"))
      writeLines("export(hello)", file.path(dest, "NAMESPACE"))
      dir.create(file.path(dest, "R"), showWarnings = FALSE)
      writeLines("hello <- function() 'hello'", file.path(dest, "R", "hello.R"))
      system2("git", c("-C", dest, "add", "-A"), stdout = FALSE, stderr = FALSE)
      system2("git", c("-C", dest, "commit", "-m", "1.0"), stdout = FALSE, stderr = FALSE)
      system2("git", c("-C", dest, "tag", "1.0"), stdout = FALSE, stderr = FALSE)
      TRUE
    })
  run_update(io, out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(all(is.na(.ds_named_builds(con))))
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1L]])))
})
