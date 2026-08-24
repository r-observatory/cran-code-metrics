# scripts/export.R: SQLite export, manifest, and fingerprint helpers.
#
# Load order: config.R -> analyze.R -> export.R
# (analyze.R supplies the author-identity parser the span projection reuses.)
# Does NOT auto-source dependencies; caller controls load order.

#' Coerce logical columns in a data.frame to 0/1 INTEGER.
#'
#' SQLite has no native boolean type. This helper converts every logical
#' column to integer (TRUE -> 1L, FALSE -> 0L, NA -> NA_integer_) so that
#' downstream reads are stable regardless of driver type inference.
#'
#' @param df A data.frame. Non-logical columns are unchanged.
#' @return A copy of df with all logical columns replaced by integer.
.coerce_logicals <- function(df) {
  for (col in names(df)) {
    if (is.logical(df[[col]])) {
      df[[col]] <- as.integer(df[[col]])
    }
  }
  df
}

#' Export code-metrics tables to a fresh SQLite database.
#'
#' Creates (or replaces) the file at `path` with three tables:
#'   cran_code_summary  -- one row per package-version, all metric columns.
#'   cran_code_churn    -- one row per file per version (added/deleted lines).
#'   cran_api_history   -- one row per version (export diffs as JSON arrays).
#'
#' The schema for cran_code_summary is derived entirely from `summary_df`
#' (schema-flexible). Logical columns in any input frame are coerced to 0/1
#' INTEGER before writing. NA values are preserved.
#'
#' @param path       File path for the output .db file.
#' @param summary_df data.frame with columns package, version, date, and any
#'   number of metric columns (integer/numeric/logical/character).
#'   If empty (0 rows), the table is still created with at least package and
#'   version TEXT columns.
#' @param churn_df   data.frame with columns package, version, file, added,
#'   deleted. added/deleted may be NA for binary files.
#' @param api_df     data.frame with columns package, version, exports_added,
#'   exports_removed (JSON array strings), n_exports (integer), and optionally
#'   cold_removals.
export_metrics <- function(path, summary_df, churn_df, api_df, vignettes_df = NULL) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # ---- cran_code_summary -----------------------------------------------------
  write_summary <- .coerce_logicals(summary_df)
  # Guarantee at least package and version columns for schema stability.
  if (!"package" %in% names(write_summary)) {
    write_summary[["package"]] <- rep(NA_character_, nrow(write_summary))
  }
  if (!"version" %in% names(write_summary)) {
    write_summary[["version"]] <- rep(NA_character_, nrow(write_summary))
  }
  DBI::dbWriteTable(con, "cran_code_summary", write_summary, row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE UNIQUE INDEX idx_summary_pkg_ver ON cran_code_summary(package, version)")

  # Per-metric coverage for this run, so the next one can compare against it and
  # so a reader can see what each metric actually reports before trusting a rate.
  # ---- cran_vignettes --------------------------------------------------------
  # One row per vignette per version. The summary says how many; this says which
  # ones, what they are written in, and what they are called in the index, which
  # is the string a reader actually browses and lives nowhere else.
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS cran_vignettes (
    package TEXT NOT NULL, version TEXT NOT NULL, file TEXT NOT NULL,
    is_current INTEGER NOT NULL DEFAULT 0,
    name TEXT, format TEXT, engine TEXT, output TEXT,
    title TEXT, author TEXT, n_authors INTEGER, author_stated INTEGER,
    builder TEXT,
    prebuilt INTEGER, precomputed INTEGER,
    lines INTEGER, has_code INTEGER,
    PRIMARY KEY (package, version, file))")
  if (!is.null(vignettes_df) && nrow(vignettes_df) > 0L) {
    DBI::dbWriteTable(con, "cran_vignettes", vignettes_df, append = TRUE)
  }
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_vignettes_pkg ON cran_vignettes(package, is_current)")

  cov <- metric_coverage(write_summary)
  DBI::dbExecute(con, "CREATE TABLE metric_coverage (
    metric TEXT NOT NULL, n_rows INTEGER, measured INTEGER, positive INTEGER,
    kind TEXT, PRIMARY KEY (metric))")
  if (nrow(cov) > 0) DBI::dbWriteTable(con, "metric_coverage", cov, append = TRUE)

  # ---- cran_code_churn -------------------------------------------------------
  DBI::dbWriteTable(con, "cran_code_churn", .coerce_logicals(churn_df), row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE INDEX idx_churn_pkg_ver ON cran_code_churn(package, version)")
  DBI::dbExecute(con,
    "CREATE INDEX idx_churn_pkg ON cran_code_churn(package)")

  # ---- cran_api_history ------------------------------------------------------
  DBI::dbWriteTable(con, "cran_api_history", .coerce_logicals(api_df), row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE INDEX idx_api_pkg_ver ON cran_api_history(package, version)")

  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Write an R list as pretty-printed JSON.
#'
#' @param path File path for the output .json file.
#' @param obj  R list to serialise.
write_manifest <- function(path, obj) {
  jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(NULL)
}

#' Compute a stable SHA-256 fingerprint over the set of package-version pairs.
#'
#' Derives a 64-character hex string from the sorted vector of
#' "package:version" keys in summary_df.  Adding a new version for any
#' package changes the key set and therefore changes the fingerprint.
#' Identical inputs in the same R session always produce the same hash.
#'
#' @param summary_df data.frame with at least columns package and version.
#' @return 64-character lower-case hex string (SHA-256).
metrics_fingerprint <- function(summary_df) {
  if (nrow(summary_df) == 0L) {
    keys <- character(0L)
  } else {
    keys <- sort(paste(summary_df$package, summary_df$version, sep = ":"))
  }
  digest::digest(paste(keys, collapse = ","), algo = "sha256", serialize = FALSE)
}

# ---------------------------------------------------------------------------
# In-place DB helpers (used by run_update for O(shard) memory writes)
# ---------------------------------------------------------------------------

# Delete rows for a set of packages from one table, chunking IN lists to <= 900.
# Silently no-ops if the table does not exist or pkgs is empty.
.delete_by_package <- function(con, table, pkgs) {
  tables <- DBI::dbListTables(con)
  if (!table %in% tables || length(pkgs) == 0L) return(invisible(NULL))
  chunk_size <- 900L
  for (i in seq(1L, length(pkgs), by = chunk_size)) {
    chunk <- pkgs[i:min(i + chunk_size - 1L, length(pkgs))]
    ph    <- paste(rep("?", length(chunk)), collapse = ", ")
    DBI::dbExecute(
      con,
      sprintf("DELETE FROM %s WHERE package IN (%s)", table, ph),
      params = as.list(chunk)
    )
  }
  invisible(NULL)
}

# Append rows to a detail table, creating it (schema derived from the frame) on
# first write and tolerating new columns via ALTER TABLE ... ADD COLUMN. Mirrors
# the schema-flexible cran_code_summary path but without a UNIQUE index (detail
# tables carry many rows per package/version). A zero-row frame still creates the
# table with the correct column types. Logical columns are coerced to 0/1.
.append_detail_table <- function(con, table, df) {
  if (is.null(df)) return(invisible(NULL))
  df     <- .coerce_logicals(df)
  tables <- DBI::dbListTables(con)

  if (!table %in% tables) {
    DBI::dbWriteTable(con, table, df, row.names = FALSE,
                      overwrite = FALSE, append = FALSE)
  } else {
    existing_cols <- DBI::dbListFields(con, table)
    for (col in setdiff(names(df), existing_cols)) {
      col_type <- if (is.integer(df[[col]])) "INTEGER"
                  else if (is.numeric(df[[col]])) "REAL"
                  else "TEXT"
      DBI::dbExecute(con,
        sprintf("ALTER TABLE %s ADD COLUMN \"%s\" %s", table, col, col_type))
    }
    if (nrow(df) > 0L) DBI::dbAppendTable(con, table, df)
  }
  DBI::dbExecute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS idx_%s_pkg_ver ON %s(package, version)",
    table, table))
  DBI::dbExecute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS idx_%s_pkg ON %s(package)",
    table, table))
  invisible(NULL)
}

# ---- dataset tables (normalized, content-addressed) -------------------------
# Datasets are split three ways so an identical profile is stored once, not per
# version: an identity row per (package, name), a per-version link that carries
# only a small integer content_id, and a shared profile keyed by a digest over
# everything that profile records, shared across versions AND packages. The
# heavy row_sketch lives in its own table (kept out of the merge allowlist).
#
# The key used to be (content_fp, schema_fp, fp_algo_version), and that was a
# narrower question than the row answers. The analyzer takes both of those
# digests over the column values alone: content_fp over each column's base type
# and its cell bytes, schema_fp over each column's name and base type. A
# factor's labels stand in for its codes, and an attribute written beside the
# values reaches neither. So two records could agree on the key and disagree
# about what the reader went on to record, the row could hold only one answer,
# and the one it held was whichever record the shard reached first. A package
# was being handed another package's measurement.
#
# profile_fp closes that. It is taken in R, here, over every field this row
# stores, so two profiles that differ in any recorded way are two rows and no
# dataset can be given a measurement that was taken of another one.
#
# content_fp is unchanged and stays a column. It is the user-facing "the same
# data in N packages" signal and the thing the discovery feature groups on;
# widening it would change what it means. It is a fingerprint, not the key.

# Columns of a dataset record that describe the data itself, and so belong on
# the shared profile rather than on the per-version link. Anything that can
# differ between two files holding identical bytes is deliberately absent:
# which file it came from, how it was compressed, which directory it sat in.
# Putting one of those here would give two identical datasets two profile rows
# and cost the dedup this table exists for.
#
# Everything else the reader records about the object is here, whether or not
# content_fp covers it, because profile_fp does. That includes the fields lifted
# off attributes rather than off the values: the class chain, the time zone an
# instant is stored in, the calendar a series is placed on, the projection its
# coordinates are declared in. Two records that disagree about any of them now
# take a row each.
#
# Types are declared rather than inferred from whatever a shard happens to
# carry. A shard whose every density is missing would otherwise fix that column
# as text for good, and the column would then read back as text forever.
.DATASET_CONTENT_COLS <- c(
  class = "TEXT", kind = "TEXT", nrow = "INTEGER", ncol = "INTEGER",
  length = "INTEGER", n_cols = "INTEGER", n_unique = "INTEGER",
  n_missing_total = "INTEGER", columns = "TEXT", has_rownames = "INTEGER",
  shape_fp = "TEXT",
  dim = "TEXT", n_dim = "INTEGER", has_dimnames = "INTEGER",
  n_stored = "INTEGER", n_cells = "INTEGER", density = "REAL",
  matrix_value_type = "TEXT", matrix_shape = "TEXT", matrix_storage = "TEXT",
  matrix_uplo = "TEXT", matrix_diag = "TEXT",
  ts_start = "REAL", ts_end = "REAL", ts_frequency = "REAL", frequency = "REAL",
  index_start = "TEXT", index_end = "TEXT", index_n = "INTEGER", index_class = "TEXT",
  geom_type = "TEXT", is_geometry = "INTEGER", n_geometries = "INTEGER",
  is_spatial = "INTEGER",
  crs_input = "TEXT", crs_epsg = "INTEGER", crs_wkt = "TEXT", bbox = "TEXT",
  n_layers = "INTEGER", object_system = "TEXT", s4_package = "TEXT",
  label = "TEXT", comment = "TEXT", units = "TEXT", attrs_other = "TEXT",

  # What a column or a grid holds, on the same terms summary() reports it.
  type = "TEXT", mean = "REAL", median = "REAL", q1 = "REAL", q3 = "REAL",
  sd = "REAL", col_min = "REAL", col_max = "REAL",
  skewness = "REAL", kurtosis = "REAL",
  n_outliers = "INTEGER", n_outliers_low = "INTEGER", n_outliers_high = "INTEGER",
  mode_value = "REAL", mode_share = "REAL",
  n_true = "INTEGER", n_false = "INTEGER",
  min_nchar = "INTEGER", max_nchar = "INTEGER", n_blank = "INTEGER",
  n_zero = "INTEGER", p_zero = "REAL",
  n_infinite = "INTEGER", max_infinite = "INTEGER", min_infinite = "INTEGER",
  is_integer_valued = "INTEGER", sort_order = "TEXT",
  n_missing_leading = "INTEGER", n_missing_trailing = "INTEGER",
  max_missing_run = "INTEGER",
  summary_over = "TEXT",

  # Written down beside the values rather than computed from them.
  levels = "TEXT", n_levels = "INTEGER", level_counts = "TEXT",
  is_factor = "INTEGER", is_ordered = "INTEGER",

  # Which kind of table, and how it is keyed and grouped.
  frame_class = "TEXT", is_grouped = "INTEGER",
  dt_key = "TEXT", dt_indices = "TEXT",
  group_vars = "TEXT", n_groups = "INTEGER",

  # What a list holds. The inner row count is the one that matters: a nested
  # table reports its group count as its rows.
  element_names = "TEXT", element_class = "TEXT", element_classes = "TEXT",
  element_len_min = "INTEGER", element_len_max = "INTEGER",
  element_len_total = "INTEGER", max_depth = "INTEGER",
  inner_nrow_total = "INTEGER", inner_ncol = "INTEGER", inner_names = "TEXT",
  inner_schema_varies = "INTEGER",

  # The labels along the margins of a grid, without which a table of counts
  # cannot be read.
  dimnames = "TEXT",

  # How evenly a series is observed.
  ts_span = "REAL", index_span = "REAL", index_tz = "TEXT",
  index_delta = "REAL", index_regular = "INTEGER",
  index_n_gaps = "INTEGER", index_max_gap = "REAL",
  index_sorted = "INTEGER", index_has_duplicates = "INTEGER",

  # Spatial and raster detail.
  geom_dimension = "TEXT", n_empty = "INTEGER",
  resolution = "TEXT", nodata_value = "REAL", in_memory = "INTEGER",
  layer_names = "TEXT", layer_min = "TEXT", layer_max = "TEXT",

  # Which kind of missing, and which end an infinity runs to. Both are
  # column-level too and ride in the columns JSON; these are for the objects
  # that are one vector rather than a table.
  n_nan = "INTEGER", n_infinite_pos = "INTEGER", n_infinite_neg = "INTEGER",

  # A broken-down time: how many fields it is stored in, and the years it
  # covers, which is what is recoverable without rebuilding the instants.
  n_fields = "INTEGER", year_min = "INTEGER", year_max = "INTEGER",

  # Sparse and graph.
  n_nonzero = "INTEGER", n_vertices = "INTEGER", n_edges = "INTEGER",
  directed = "INTEGER",

  # A list's elements profiled the way a frame's columns are, in the same
  # shape, so one renderer serves both.
  elements = "TEXT",
  # dplyr's rowwise state, which was in the class chain and never recorded.
  is_rowwise = "INTEGER",

  # Per element rather than reduced across the list. The aggregates cannot say
  # how big any one slot was, which is the question a list of folds raises.
  element_lens = "TEXT", element_inner_nrow = "TEXT",

  # Where a grid's variation runs. A summary over every cell reads a matrix and
  # its transpose identically; the means along each margin do not. Fourteen
  # columns whatever the size of the matrix, because the margins are summarised
  # rather than stored.
  row_mean_min = "REAL", row_mean_q1 = "REAL", row_mean_median = "REAL",
  row_mean_mean = "REAL", row_mean_q3 = "REAL", row_mean_max = "REAL",
  row_mean_sd = "REAL",
  col_mean_min = "REAL", col_mean_q1 = "REAL", col_mean_median = "REAL",
  col_mean_mean = "REAL", col_mean_q3 = "REAL", col_mean_max = "REAL",
  col_mean_sd = "REAL",

  # Slots of a list that hold nothing at all. They count towards its length and
  # they draw as nothing, so a list of ten with four of them empty is not the
  # list its length says it is.
  n_empty_slots = "INTEGER",
  # The time zone an instant is stored in. It belongs to the object rather than
  # to the file: the same moment written in two zones reads as two different
  # local times. index_tz above it is the zone of a series' index, which is a
  # different field on a different kind of object, and declaring one was not
  # declaring the other.
  tz = "TEXT",

  # How deeply this profile's columns were read: full, reduced, none or
  # structural. It is the legend for the rest of the row. At `none` there is no
  # columns array at all and the whole-object figures stand in its place; at
  # `reduced` an entry carries a name, a type and two counts and no col_fp; at
  # `structural` an entry is a name and a type, because no value was read.
  # Without it, an entry with no statistics reads the same as an object that
  # had none to give.
  #
  # Content-determined, so it belongs beside the array it explains. Width, the
  # mix of column types and the number of cells decide between the first three,
  # and the length of one column decides the fourth; all of them are properties
  # of the data, so two files holding identical bytes are read to the same
  # depth and there is one answer to store rather than one per package.
  column_detail = "TEXT",

  # How many bytes of column profile this row does NOT carry. Zero on a row
  # that carries all of it, which is every honest row; a count says the profile
  # was over MAX_DATASET_COLUMNS_BYTES and was refused rather than stored. It
  # is written by the pipeline rather than read from the analyzer, so that a
  # reader can tell an object with no columns from one whose columns would not
  # fit through the load.
  columns_refused_bytes = "INTEGER"
)

# How one file happened to store the data, which is not a property of the data.
# The same table saved twice can differ in all of these: R's serialization
# format has versions, and version 3 cannot be read by R before 3.5.0, so this
# is the difference between a dataset a reader can open and one they cannot.
#
# Nothing else belongs here. A field the reader lifts off an attribute rather
# than off the values reaches neither fingerprint, but the digest the profile
# is keyed on covers it, so it sits on the profile with the rest of what the
# reader recorded and two records that disagree about it take a row each.
#
# One consequence worth knowing. These rows are deleted and rewritten for every
# package in a shard, so a field here arrives on every scan. The profile is
# written with INSERT OR IGNORE, so a field there arrives when anything the
# digest covers moves, which is what a reader recording something new does.
.DATASET_VERSION_COLS <- c(
  format_version = "INTEGER", compressed_bytes = "INTEGER", notes = "TEXT",
  # A file that is not what its name says: which separator would work, and how
  # many columns it would give. A property of this file, not of the data.
  delimiter_looks_like = "TEXT", delimiter_would_give_ncol = "INTEGER"
)

# Where a dataset was found. Not a property of its contents: the same data can
# sit under data/ in one package and inst/extdata in another, and only the first
# is loadable by name.
.DATASET_IDENTITY_COLS <- c(
  origin_dir = "TEXT",
  # The title of the help page documenting this dataset. Not a property of the
  # data: two packages carrying identical bytes may document them differently,
  # or one may not document them at all.
  title = "TEXT"
)

# The three key fields a profile carries besides its measurements. They are
# named one by one by the writer rather than declared in .DATASET_CONTENT_COLS,
# because they are in the table's own CREATE and never arrive by ALTER, but the
# digest has to cover them: two records with different content_fp must never
# share a row, which is the guarantee the old key gave and this one keeps.
.DATASET_CONTENT_KEY_COLS <- c("content_fp", "schema_fp", "fp_algo_version")

#' The digest a profile row is keyed by: one value per record, over every field
#' that record stores on the profile.
#'
#' Taken over the fixed field list rather than over whatever columns the frame
#' happens to carry. A shard is one analyzer invocation per package and the
#' frame it produces holds only the fields that package's records mentioned, so
#' a raster field is a column in a shard that read a raster and absent in one
#' that did not. Digesting `intersect(spec, names(df))` would give the same
#' record two different digests in two shards and split the dedup down the
#' middle. An absent column is read as missing, which is what it stores.
#'
#' Encoding, chosen so that a value can only ever hash to itself:
#'   - missing is `~`, and every present value is its byte length, a colon, and
#'     its bytes, so no value can impersonate the separator or another field;
#'   - logicals become integers first, because that is what SQLite stores and
#'     what the writer converts them to, and a field can arrive from the parser
#'     as either depending on whether one package's records left it empty;
#'   - doubles are written to 17 significant digits, which round-trips an IEEE
#'     double exactly, so two distinct values cannot share a rendering;
#'   - NaN counts as missing, matching SQLite, which stores it as NULL.
#'
#' @param df One row per dataset record, as the writer holds it: after the
#'   column-profile refusal, so the digest describes what is stored rather than
#'   what arrived.
#' @return Character vector of 64-character hex digests, one per row.
.dataset_profile_fp <- function(df) {
  n <- nrow(df)
  if (n == 0L) return(character(0L))
  fields <- c(.DATASET_CONTENT_KEY_COLS, names(.DATASET_CONTENT_COLS))
  parts <- vector("list", length(fields))
  for (i in seq_along(fields)) {
    v <- df[[fields[i]]]
    if (is.null(v)) v <- rep(NA, n)
    if (is.logical(v)) v <- as.integer(v)
    enc <- if (is.double(v)) {
      ifelse(is.na(v), NA_character_, sprintf("%.17g", v))
    } else if (is.character(v)) {
      enc2utf8(v)
    } else {
      as.character(v)
    }
    parts[[i]] <- ifelse(is.na(enc), "~",
                         paste0(nchar(enc, type = "bytes"), ":", enc))
  }
  # One row's string at a time. A single column profile runs to
  # MAX_DATASET_COLUMNS_BYTES, so pasting the whole frame into one vector would
  # hold a second copy of the heaviest thing in it.
  vapply(seq_len(n), function(r) {
    digest::digest(paste0(vapply(parts, function(p) p[[r]], character(1L)),
                          collapse = ""),
                   algo = "sha256", serialize = FALSE)
  }, character(1L))
}

# Dataset columns that have changed table, named by the table they left.
#
# These tables only ever gain columns. A field that moves is added to its new
# home by .ensure_dataset_columns and then written there, and the copy on the
# old table is never written again: it keeps whatever it held on the day the
# move landed, for good, and every reader that finds it there believes it. So
# the move has two halves, and this is the second.
#
# Held as an explicit list rather than derived from the specs, because "a
# column this table's spec does not declare" is also true of package, name,
# version, content_id and every other field the writer names one by one, and a
# migration that dropped those would empty the database.
.DATASET_COLS_THAT_MOVED <- list(
  cran_dataset_versions = c(
    # The reading depth, which is content-determined and now sits beside the
    # array it explains.
    "column_detail",
    # And everything a database written between the key change and this one was
    # given on the link: the fields content_fp does not cover, which were put
    # there while the profile key was too narrow to hold them safely. The key
    # covers them now, so they are back on the profile, where the viewer reads
    # them, and this copy has to go or it stands for ever holding whatever it
    # held on the day it stopped being written.
    #
    # The values are not carried across. A profile several links point at is
    # exactly the collapsed answer this undoes, and picking one link's answer
    # for it would be the substitution again. They come back from the reader on
    # the next scan, which a new analyzer build asks for on every package in
    # the archive.
    "class", "kind", "frame_class", "object_system", "s4_package",
    "has_rownames", "has_dimnames", "dimnames",
    "matrix_value_type", "matrix_shape", "matrix_storage", "matrix_uplo",
    "matrix_diag",
    "ts_start", "ts_end", "ts_frequency", "ts_span", "frequency",
    "index_start", "index_end", "index_n", "index_class", "index_span",
    "index_tz", "index_delta", "index_regular", "index_n_gaps",
    "index_max_gap", "index_sorted", "index_has_duplicates",
    "crs_input", "crs_epsg", "crs_wkt",
    "n_layers", "resolution", "nodata_value", "in_memory", "layer_names",
    "layer_min", "layer_max",
    "label", "comment", "units", "attrs_other", "tz",
    "is_ordered",
    "is_grouped", "group_vars", "n_groups", "is_rowwise",
    "dt_key", "dt_indices",
    "element_names", "inner_names")
)

#' Bring the reading depth across to the profiles that already exist.
#'
#' A profile is written with INSERT OR IGNORE against its generation key, so a
#' re-scan of data whose bytes have not moved does not reach the table and the
#' depth would read NULL on every row that was already published. The version
#' links about to lose the column are holding the answer, and it is the same
#' answer on every link pointing at one profile, because which depth a record
#' is read at follows from the data. So any one of them will do.
#'
#' Only this direction, and only this field. A database written between the
#' key change and the one that undid it also has fields to bring back off the
#' links, and those cannot be carried: a profile that several links point at is
#' the collapsed answer the key change undoes, and there is no way to say whose
#' answer it was. They come back from the reader on the next scan instead,
#' which a new analyzer build already asks for on every package in the
#' archive.
.carry_reading_depth_to_profiles <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!all(c("cran_dataset_contents", "cran_dataset_versions") %in% tables)) {
    return(invisible(NULL))
  }
  if (!"column_detail" %in% DBI::dbListFields(con, "cran_dataset_contents")) {
    return(invisible(NULL))
  }
  if (!"column_detail" %in% DBI::dbListFields(con, "cran_dataset_versions")) {
    return(invisible(NULL))
  }
  DBI::dbExecute(con, "
    UPDATE cran_dataset_contents
       SET column_detail = (
             SELECT v.column_detail FROM cran_dataset_versions v
              WHERE v.content_id = cran_dataset_contents.content_id
                AND v.column_detail IS NOT NULL
              LIMIT 1)
     WHERE column_detail IS NULL")
  invisible(NULL)
}

#' Drop the copy a moved dataset column left behind on the table it came from.
#'
#' A no-op on a database that never had it, and a no-op for good once it has
#' run. Refuses to touch a column the table's own spec still declares, so a
#' name left in the list by mistake cannot delete live data.
.retire_moved_dataset_columns <- function(con) {
  specs <- list(cran_dataset_contents = .DATASET_CONTENT_COLS,
                cran_dataset_versions = .DATASET_VERSION_COLS,
                cran_datasets         = .DATASET_IDENTITY_COLS)
  present <- DBI::dbListTables(con)
  for (tbl in names(.DATASET_COLS_THAT_MOVED)) {
    if (!tbl %in% present) next
    gone <- setdiff(.DATASET_COLS_THAT_MOVED[[tbl]], names(specs[[tbl]]))
    drop <- intersect(gone, DBI::dbListFields(con, tbl))
    if (!length(drop)) next
    # SQLite rewrites every row once per dropped column, so on a published
    # database this is minutes rather than milliseconds, once. Said out loud
    # because a run that stops here otherwise looks like a run that hung.
    cat(sprintf("moving %d column%s off %s: %s\n", length(drop),
                if (length(drop) == 1L) "" else "s", tbl,
                paste(drop, collapse = ", ")), file = stdout())
    flush(stdout())
    for (col in drop) {
      DBI::dbExecute(con, sprintf('ALTER TABLE "%s" DROP COLUMN "%s"', tbl, col))
    }
  }
  invisible(NULL)
}

#' Add any dataset column the analyzer now emits that the table has not seen.
#' Mirrors what cran_code_summary already does for its own new columns; without
#' it the widened CREATE only ever applies to a database built from nothing.
.ensure_dataset_columns <- function(con) {
  add <- function(table, spec) {
    if (!table %in% DBI::dbListTables(con)) return(invisible(NULL))
    existing <- DBI::dbListFields(con, table)
    for (col in setdiff(names(spec), existing)) {
      DBI::dbExecute(con, sprintf('ALTER TABLE %s ADD COLUMN "%s" %s',
                                  table, col, spec[[col]]))
    }
    invisible(NULL)
  }
  add("cran_dataset_contents", .DATASET_CONTENT_COLS)
  add("cran_dataset_versions", .DATASET_VERSION_COLS)
  add("cran_datasets", .DATASET_IDENTITY_COLS)
  invisible(NULL)
}

#' Put a profile table that was keyed on the fingerprints onto the digest.
#'
#' The old key, UNIQUE (content_fp, schema_fp, fp_algo_version), is declared
#' inline, so SQLite holds it in an automatic index that cannot be dropped. The
#' table is rebuilt instead: every column it has picked up since, with its
#' declared type and its NOT NULL, plus profile_fp and the one unique
#' constraint that now decides what shares a row. content_id is copied as it
#' stands, because the version links and the sketches name it.
#'
#' Rows that predate the digest cannot have one computed for them here: the
#' fields it is taken over are on the row, but the row is exactly the collapsed
#' answer the digest exists to stop, so a digest taken over it would be a claim
#' about which record it came from that nobody can check. They are seeded with
#' the key they were stored under instead, prefixed so it can never be mistaken
#' for a digest. Nothing matches them again, and they leave on the first GC
#' after the links that name them are rewritten, which the generation bump asks
#' for on every package in the archive.
#'
#' A one-time no-op once the column is there.
.rekey_dataset_contents <- function(con) {
  if (!"cran_dataset_contents" %in% DBI::dbListTables(con)) return(invisible(NULL))
  info <- DBI::dbGetQuery(con, "PRAGMA table_info(cran_dataset_contents)")
  if ("profile_fp" %in% info$name) return(invisible(NULL))
  # Rebuilt from what the table declares rather than from what this file's
  # CREATE says, so every column it has picked up by ALTER since keeps its type
  # and its NOT NULL, and content_id keeps being the rowid the version links
  # and the sketches name.
  pk <- info$name[info$pk == 1L]
  defs <- vapply(seq_len(nrow(info)), function(i) {
    nm <- info$name[[i]]
    ty <- if (nzchar(info$type[[i]] %||% "")) info$type[[i]] else "TEXT"
    sprintf('"%s" %s%s%s', nm, ty,
            if (isTRUE(info$notnull[[i]] == 1L)) " NOT NULL" else "",
            if (length(pk) == 1L && identical(nm, pk)) " PRIMARY KEY" else "")
  }, character(1L))
  if (length(pk) > 1L) {
    defs <- c(defs, sprintf("PRIMARY KEY (%s)",
                            paste(sprintf('"%s"', pk), collapse = ", ")))
  }
  cols <- paste(sprintf('"%s"', info$name), collapse = ", ")
  # A digest is 64 hex characters and this is not one, so a seeded row cannot
  # collide with a real profile however the fingerprints read.
  seeded <- if (all(.DATASET_CONTENT_KEY_COLS %in% info$name)) {
    "'kept from the fingerprint key:' || content_fp || char(31) || schema_fp ||
     char(31) || fp_algo_version"
  } else {
    "'kept from the fingerprint key:' || content_id"
  }
  DBI::dbExecute(con, sprintf(
    'CREATE TABLE cran_dataset_contents_new (%s, "profile_fp" TEXT NOT NULL,
       UNIQUE ("profile_fp"))', paste(defs, collapse = ", ")))
  DBI::dbExecute(con, sprintf(
    'INSERT INTO cran_dataset_contents_new (%s, "profile_fp")
       SELECT %s, %s FROM cran_dataset_contents', cols, cols, seeded))
  DBI::dbExecute(con, "DROP TABLE cran_dataset_contents")
  DBI::dbExecute(con,
    "ALTER TABLE cran_dataset_contents_new RENAME TO cran_dataset_contents")
  invisible(NULL)
}

#' Let a version link stand without a profile behind it.
#'
#' cran_dataset_versions.content_id was NOT NULL, which is what made "the
#' reader took no fingerprint" mean "the dataset leaves the catalog". The
#' constraint cannot be dropped in place, so the table is rebuilt from its own
#' CREATE with that one phrase removed: every column it has picked up since,
#' and every row, come across untouched. The index it carries is recreated by
#' the caller.
#'
#' A one-time no-op once the constraint is gone.
.relax_dataset_version_content_id <- function(con) {
  if (!"cran_dataset_versions" %in% DBI::dbListTables(con)) return(invisible(NULL))
  sql <- DBI::dbGetQuery(con,
    "SELECT sql FROM sqlite_master
      WHERE type = 'table' AND name = 'cran_dataset_versions'")$sql
  notnull <- "content_id INTEGER NOT NULL"
  if (length(sql) != 1L || is.na(sql) || !grepl(notnull, sql, fixed = TRUE)) {
    return(invisible(NULL))
  }
  cols   <- paste(sprintf('"%s"', DBI::dbListFields(con, "cran_dataset_versions")),
                  collapse = ", ")
  create <- sub(notnull, "content_id INTEGER", sql, fixed = TRUE)
  create <- sub("cran_dataset_versions", "cran_dataset_versions_new", create,
                fixed = TRUE)
  DBI::dbExecute(con, create)
  DBI::dbExecute(con, sprintf(
    "INSERT INTO cran_dataset_versions_new (%s) SELECT %s FROM cran_dataset_versions",
    cols, cols))
  DBI::dbExecute(con, "DROP TABLE cran_dataset_versions")
  DBI::dbExecute(con,
    "ALTER TABLE cran_dataset_versions_new RENAME TO cran_dataset_versions")
  invisible(NULL)
}

.ensure_dataset_tables <- function(con) {
  .relax_dataset_version_content_id(con)
  tables <- DBI::dbListTables(con)
  if (!"cran_datasets" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE cran_datasets (
      package TEXT NOT NULL, name TEXT NOT NULL, file TEXT, internal INTEGER,
      current_version TEXT, current_content_id INTEGER,
      PRIMARY KEY (package, name))")
  }
  # content_id is nullable: a dataset whose values the reader could not take
  # comes back with no fingerprints, so there is no profile row for it to point
  # at and none can be invented without telling two objects that were never
  # compared that they hold the same data. The link still says the package
  # ships this dataset at this version, and confidence and notes beside it say
  # what was and was not read. How many links stand like this is published in
  # the manifest.
  if (!"cran_dataset_versions" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE cran_dataset_versions (
      package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
      content_id INTEGER, format TEXT, compression TEXT, confidence TEXT,
      is_current INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (package, name, version))")
  }
  if (!"cran_dataset_contents" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE cran_dataset_contents (
      content_id INTEGER PRIMARY KEY,
      profile_fp TEXT NOT NULL,
      content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
      nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
      UNIQUE (profile_fp))")
  }
  .rekey_dataset_contents(con)
  if (!"cran_dataset_sketches" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE cran_dataset_sketches (
      content_id INTEGER PRIMARY KEY, row_sketch TEXT)")
  }
  # The analyzer describes more of a dataset over time, and those fields arrive
  # as columns that do not exist yet. Without this the widened CREATE above only
  # applies to a database being built from nothing, and every incremental run
  # against a downloaded one silently drops them.
  .ensure_dataset_columns(con)
  # Before the carry below, which asks the version links what depth they hold
  # for a profile and is a scan of the whole table per profile without this.
  # Measured on 110,000 profiles: four and a half minutes with the index put
  # back afterwards, three seconds with it put back here. The rebuild in
  # .relax_dataset_version_content_id takes the table's indexes down with it,
  # so this is where they come back either way, and so does the one in
  # .rekey_dataset_contents.
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cran_dsv_content ON cran_dataset_versions(content_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cran_dsc_schema ON cran_dataset_contents(schema_fp)")
  # The fingerprints used to be the table's unique key, and every reader asking
  # "which packages ship this data" was served by the index that constraint
  # carried. profile_fp is the key now, so that index has to be asked for by
  # name; without it the discovery query is a scan of the whole table. Asked
  # for only where all three columns are there, the way the schema index above
  # is: a table stripped to a couple of columns by a restore has nothing here
  # to index, and refusing to open it over that would be the wrong refusal.
  if (all(.DATASET_CONTENT_KEY_COLS %in%
          DBI::dbListFields(con, "cran_dataset_contents"))) {
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cran_dsc_content ON cran_dataset_contents(content_fp, schema_fp, fp_algo_version)")
  }
  # After the widening, so a column that has changed table is added to its new
  # home before the copy on the old one goes: the two halves of one move, in
  # the order that never leaves the field homeless. The value it was holding
  # travels in between, for the one field that can be carried.
  .carry_reading_depth_to_profiles(con)
  .retire_moved_dataset_columns(con)
  invisible(NULL)
}

#' Migrate away from the pre-normalization flat cran_datasets table (one row per
#' dataset per version, carrying columns/row_sketch inline). It collides by name
#' with the normalized identity table, so an incremental run against a database
#' that still holds it would fail the identity append. Drop it, and clear the
#' datasets_scanned sentinel on every package we are NOT writing this shard, so
#' the flat rows are rebuilt into the normalized tables instead of being skipped
#' as already scanned. The current shard's packages keep the marker just written
#' for them. Identified by the absence of the identity-only current_version
#' column, so it is a one-time no-op once the normalized schema is in place.
.migrate_legacy_dataset_table <- function(con, keep_pkgs = character(0L)) {
  tables <- DBI::dbListTables(con)
  if (!"cran_datasets" %in% tables) return(invisible(NULL))
  if ("current_version" %in% DBI::dbListFields(con, "cran_datasets")) {
    return(invisible(NULL))
  }
  DBI::dbExecute(con, "DROP TABLE cran_datasets")
  if ("cran_code_summary" %in% tables &&
      "datasets_scanned" %in% DBI::dbListFields(con, "cran_code_summary")) {
    keep_pkgs <- unique(as.character(keep_pkgs))
    if (length(keep_pkgs) > 0L) {
      ph <- paste(rep("?", length(keep_pkgs)), collapse = ",")
      DBI::dbExecute(con, sprintf(
        "UPDATE cran_code_summary SET datasets_scanned = NULL WHERE package NOT IN (%s)", ph),
        params = as.list(keep_pkgs))
    } else {
      DBI::dbExecute(con, "UPDATE cran_code_summary SET datasets_scanned = NULL")
    }
  }
  invisible(NULL)
}

#' Write per-version dataset records into the four normalized tables. `df` is one
#' row per (package, version, dataset) with columns package, name, version, file,
#' internal, format, compression, confidence, class, kind, nrow, ncol,
#' n_missing_total, content_fp, schema_fp, fp_algo_version, columns, row_sketch,
#' is_current. Runs inside the caller's transaction.
.write_datasets_normalized <- function(con, df, pkgs) {
  .migrate_legacy_dataset_table(con, pkgs)
  .ensure_dataset_tables(con)
  # Per-package wipe: children (version links) then parents (identity). Contents
  # and sketches are shared/immutable and are reclaimed by GC, not deleted here.
  .delete_by_package(con, "cran_dataset_versions", pkgs)
  .delete_by_package(con, "cran_datasets",         pkgs)
  if (is.null(df) || nrow(df) == 0L) return(invisible(NULL))

  df$fp_algo_version <- as.integer(df$fp_algo_version)
  df$internal        <- as.integer(df$internal)
  df$is_current      <- as.integer(df$is_current)
  # Atomic vectors / matrices / S4 have values but no column schema, so schema_fp
  # is NA. Use an empty string so they still dedup by content and satisfy the
  # NOT NULL + UNIQUE(content_fp, schema_fp, fp_algo_version) constraint (a NULL
  # would make every such row distinct and get dropped by INSERT OR IGNORE).
  df$schema_fp[is.na(df$schema_fp)] <- ""

  # Nothing upstream bounds one column profile, and one pathological value is
  # enough to make the published database unloadable: MySQL refuses any single
  # value over its 32 MiB packet ceiling and fails the whole table's load, not
  # the row's. A file read as something it is not has already produced profiles
  # of 321 MB here.
  #
  # What is refused is the profile, never the row. The class, the shape, the
  # counts and the fingerprints are all still true and still worth storing, and
  # the refused size stays beside them so a reader can tell a row whose columns
  # would not fit from an object that has no columns at all.
  df$columns_refused_bytes <- 0L
  if ("columns" %in% names(df)) {
    sizes <- nchar(as.character(df$columns), type = "bytes")
    over  <- !is.na(sizes) & sizes > MAX_DATASET_COLUMNS_BYTES
    if (any(over)) {
      df$columns_refused_bytes[over] <- sizes[over]
      df$columns[over] <- NA_character_
      named <- sprintf("%s %s (%s)", df$package[over], df$name[over],
                       vapply(sizes[over], format_bytes, character(1L)))
      cat(sprintf("refused %d column profile%s over %s: %s%s\n",
                  sum(over), if (sum(over) == 1L) "" else "s",
                  format_bytes(MAX_DATASET_COLUMNS_BYTES),
                  paste(head(named, 5L), collapse = ", "),
                  if (length(named) > 5L)
                    sprintf(" and %d more", length(named) - 5L) else ""),
          file = stdout())
      flush(stdout())
    }
  }

  # A single package version can surface one dataset name twice: an exported
  # data/ object and an internal sysdata object of the same name, or the same
  # object reached through two files. (package, name) is unique in cran_datasets
  # and (package, name, version) in cran_dataset_versions, so collapse to one
  # record per (package, name, version) up front, preferring the exported copy
  # (internal = 0 sorts first). Without this the version append fails the PK.
  df <- df[order(df$package, df$name, df$version, df$internal), , drop = FALSE]
  df <- df[!duplicated(paste(df$package, df$name, df$version, sep = "\x1f")), , drop = FALSE]

  # Which records the reader fingerprinted. Nearly everything is: a column too
  # long to read still has bytes, and the reader hashes them on the way past,
  # so even a frame it never opened has an identity. The ones it did not are
  # the four shapes with nothing to hash: an S4 object it holds no
  # representation for, a packed raster, an R script under data/ that only R
  # can run, and a frame whose every column is a generated sequence, which
  # occupies no bytes at all. Every such record used to be dropped here, whole,
  # so the dataset left the catalog rather than appearing in it with what is
  # known about it.
  #
  # They still get no profile row. The digest a profile is keyed on can be
  # taken over such a record, but taking it would put two objects that were
  # never compared on one row whenever the little that is known about them
  # agrees, and the page that row feeds says "the same data in N packages".
  # They get the identity row and the version link, with no content_id, and
  # confidence and notes beside it say what was read and what was not. How many
  # of them there are is published in the manifest, because a catalog entry
  # with nothing behind it is a coverage figure and a shard where the number
  # climbs is the reader losing objects it used to measure.
  fingerprinted <- !is.na(df$content_fp) & nzchar(df$content_fp)
  if (any(!fingerprinted)) {
    # Said out loud for the same reason the refusal above is: a dataset in the
    # catalog with nothing behind it is a coverage figure, and a shard where
    # that number climbs is the reader losing objects it used to measure.
    named <- sprintf("%s %s", df$package[!fingerprinted], df$name[!fingerprinted])
    cat(sprintf("kept %d dataset%s with no profile, unmeasured by the reader: %s%s\n",
                length(named), if (length(named) == 1L) "" else "s",
                paste(head(named, 5L), collapse = ", "),
                if (length(named) > 5L)
                  sprintf(" and %d more", length(named) - 5L) else ""),
        file = stdout())
    flush(stdout())
  }

  # 1. Shared profiles: one INSERT OR IGNORE per distinct profile digest. Taken
  # here rather than earlier, so it covers the refusal above: a row whose
  # column profile would not fit stores NA and a refused size, and the digest
  # says so, because it has to describe what the row holds.
  df$profile_fp <- .dataset_profile_fp(df)
  ck <- rep(NA_character_, nrow(df))
  ck[fingerprinted] <- df$profile_fp[fingerprinted]
  cts <- df[fingerprinted & !duplicated(ck), , drop = FALSE]
  df$content_id <- NA_integer_
  if (nrow(cts) > 0L) {
    content_cols <- intersect(names(.DATASET_CONTENT_COLS), names(cts))
    ins_cols <- c("profile_fp", .DATASET_CONTENT_KEY_COLS, content_cols)
    DBI::dbExecute(con,
      sprintf("INSERT OR IGNORE INTO cran_dataset_contents (%s) VALUES (%s)",
              paste(sprintf('"%s"', ins_cols), collapse = ", "),
              paste(rep("?", length(ins_cols)), collapse = ", ")),
      params = lapply(ins_cols, function(k) {
        v <- cts[[k]]
        if (is.logical(v)) as.integer(v) else v
      }))

    # Resolve content_id for the profiles in this shard and attach to every row
    # that has one. The rest keep NA, which is the whole of what the shared
    # profile table can say about them.
    ids <- DBI::dbGetQuery(con,
      "SELECT content_id, profile_fp FROM cran_dataset_contents")
    key_map <- stats::setNames(ids$content_id, ids$profile_fp)
    df$content_id[fingerprinted] <- unname(key_map[ck[fingerprinted]])
  }

  # 2. Sketches: one INSERT OR IGNORE per content_id.
  sk <- df[!is.na(df$content_id) & !duplicated(df$content_id) & !is.na(df$row_sketch),
           c("content_id", "row_sketch"), drop = FALSE]
  if (nrow(sk) > 0L) {
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO cran_dataset_sketches (content_id, row_sketch) VALUES (?, ?)",
      params = list(sk$content_id, sk$row_sketch))
  }

  # 3. Version links (package was wiped above, so a plain append is idempotent).
  ver_cols <- c("package", "name", "version", "content_id", "format",
                "compression", "confidence", "is_current",
                intersect(names(.DATASET_VERSION_COLS), names(df)))
  ver <- df[, ver_cols, drop = FALSE]
  DBI::dbAppendTable(con, "cran_dataset_versions", ver)

  # 4. Identity, one per (package, name), stamped with the current version's content.
  cur <- df[df$is_current == 1L, , drop = FALSE]
  cur <- cur[!duplicated(paste(cur$package, cur$name, sep = "\x1f")), , drop = FALSE]
  if (nrow(cur) > 0L) {
    idn <- data.frame(package = cur$package, name = cur$name, file = cur$file,
                      internal = cur$internal, current_version = cur$version,
                      current_content_id = cur$content_id, stringsAsFactors = FALSE)
    for (k in intersect(names(.DATASET_IDENTITY_COLS), names(cur))) {
      idn[[k]] <- cur[[k]]
    }
    DBI::dbAppendTable(con, "cran_datasets", idn)
  }
  invisible(NULL)
}

#' Reclaim content/sketch rows no longer referenced by any version link (a
#' dataset whose data changed orphans its previous content), so the
#' content-addressed tables cannot grow without bound.
#'
#' The subquery excludes the links that name no profile. NOT IN over a set
#' holding one NULL is NULL for every row it is asked about, so a single
#' unfingerprinted dataset anywhere in the table would quietly retire the whole
#' reclaim and leave nothing in the log to say so.
.gc_dataset_contents <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"cran_dataset_contents" %in% tables) return(invisible(NULL))
  referenced <-
    "SELECT content_id FROM cran_dataset_versions WHERE content_id IS NOT NULL"
  DBI::dbExecute(con, sprintf(
    "DELETE FROM cran_dataset_sketches WHERE content_id NOT IN (%s)", referenced))
  DBI::dbExecute(con, sprintf(
    "DELETE FROM cran_dataset_contents WHERE content_id NOT IN (%s)", referenced))
  invisible(NULL)
}

#' Free space on the filesystem holding `path`, in bytes.
#'
#' R has no portable answer to this, so it asks df. -P is the POSIX output
#' format, which guarantees one line per filesystem however long the device
#' name is; -k fixes the block size at 1024 so the numbers mean the same thing
#' on macOS (whose default is 512) and on Linux.
#'
#' The available column is read as the third all-digit field rather than the
#' fourth field, because a device name can carry a space (autofs mounts on
#' macOS are reported as "map auto_home") and shift every position after it.
#'
#' @param path A directory. Its filesystem is the one measured.
#' @return Free bytes, or NA_real_ when df is unavailable or says something
#'   this cannot read. NA means "not measured", never "none".
free_disk_bytes <- function(path) {
  if (!nzchar(path %||% "")) path <- "."
  out <- tryCatch(
    suppressWarnings(system2("df", c("-Pk", shQuote(path)),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0L))
  if (length(out) < 2L) return(NA_real_)
  fields <- strsplit(trimws(out[length(out)]), "[[:space:]]+")[[1L]]
  nums <- suppressWarnings(as.numeric(fields[grepl("^[0-9]+$", fields)]))
  if (length(nums) < 3L) return(NA_real_)
  nums[3L] * 1024
}

# Free space that a VACUUM of `path` actually depends on.
#
# SQLite builds the compacted copy in the temp directory, which is not
# necessarily the filesystem the database lives on, and then writes it back
# beside the original under a rollback journal. Both have to have room, so the
# smaller of the two is the one that decides. NA when neither could be
# measured; a filesystem that did answer is used on its own.
.vacuum_free_bytes <- function(path) {
  measured <- c(free_disk_bytes(dirname(path)), free_disk_bytes(tempdir()))
  measured <- measured[!is.na(measured)]
  if (length(measured) == 0L) return(NA_real_)
  min(measured)
}

#' Return the pages a delete freed to the filesystem.
#'
#' .gc_dataset_contents() and upsert_shard()'s per-package delete remove rows,
#' and SQLite puts every page they release on the database's own free list
#' rather than shrinking the file, so the published database only ever records
#' the largest it has ever been. VACUUM is what actually hands the space back,
#' and the only other one in the tree is in export_metrics(), which the
#' pipeline never calls.
#'
#' Skipping is a normal outcome and never an error. A free list too small to be
#' worth a rewrite is the free list working, and a disk that cannot hold the
#' copy is a reason to publish the database as it stands rather than to lose
#' the run. Both say so in `reason`; the caller logs it.
#'
#' The caller must not be inside a transaction: SQLite refuses to VACUUM there.
#'
#' @param con        Open DBI connection to the database at `path`.
#' @param path       The database file, needed to measure the file itself.
#' @param min_reclaim Smallest free-list size worth rewriting the file for.
#' @param free_bytes Free space the rewrite has to fit in. NA means it could
#'   not be measured, in which case the reclaim goes ahead: VACUUM is
#'   atomic, so a disk that turns out to be too small costs the reclaim and
#'   leaves the database exactly as it was.
#' @return list(ran, before, after, reclaimed, reason). `reclaimed` is 0
#'   whenever `ran` is FALSE, so a caller can credit it unconditionally.
vacuum_db <- function(con, path, min_reclaim = VACUUM_MIN_RECLAIM_BYTES,
                      free_bytes = .vacuum_free_bytes(path)) {
  before <- as.numeric(file.info(path)$size %||% 0)
  skipped <- function(reason) {
    list(ran = FALSE, before = before, after = before, reclaimed = 0,
         reason = reason)
  }

  free_pages <- tryCatch({
    page  <- as.numeric(DBI::dbGetQuery(con, "PRAGMA page_size")[[1L]])
    count <- as.numeric(DBI::dbGetQuery(con, "PRAGMA freelist_count")[[1L]])
    page * count
  }, error = function(e) NA_real_)
  if (is.na(free_pages)) {
    return(skipped("the free list could not be measured"))
  }
  if (free_pages < min_reclaim) {
    return(skipped(sprintf(
      "its free list holds %s, and %s is the least worth rewriting the file for",
      format_bytes(free_pages), format_bytes(min_reclaim))))
  }

  needed <- before * VACUUM_DISK_FACTOR
  if (!is.na(free_bytes) && free_bytes < needed) {
    return(skipped(sprintf(
      "the disk has %s free and the rewrite needs about %s",
      format_bytes(free_bytes), format_bytes(needed))))
  }

  err <- tryCatch({
    DBI::dbExecute(con, "VACUUM")
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(err)) return(skipped(sprintf("the rewrite failed: %s", err)))

  after <- as.numeric(file.info(path)$size %||% 0)
  list(ran = TRUE, before = before, after = after,
       reclaimed = max(0, before - after), reason = "")
}

#' Open (or create) the dataset SQLite database, ensuring the four normalized
#' dataset tables exist. Mirrors open_or_init_db() but for the data series.
#'
#' @param path File path for the dataset SQLite database.
#' @return An open DBI connection. Caller must dbDisconnect().
open_or_init_data_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  .ensure_dataset_tables(con)
  con
}

.create_read_attempts <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE cran_analyzer_read_attempts (
      package          TEXT NOT NULL,
      version          TEXT NOT NULL,
      attempts         INTEGER NOT NULL DEFAULT 0,
      analyzer_version TEXT,
      last_attempt     TEXT,
      PRIMARY KEY (package, version)
    )")
  invisible(NULL)
}

#' Replace the per-package attempt table with the per-version one.
#'
#' A count taken over a whole package belongs to no version of it, so there is
#' nothing to carry across: a package that was read at one version and not at
#' another produced exactly one row, and which version it was about is the fact
#' the old shape did not hold. The rows are dropped and the packages asked
#' again, which costs MAX_ANALYZER_READ_ATTEMPTS runs and is the same cost a new
#' analyzer build already imposes on every one of them.
#'
#' A one-time no-op once the table carries a version.
.migrate_read_attempts_by_version <- function(con) {
  if (!"cran_analyzer_read_attempts" %in% DBI::dbListTables(con)) {
    return(invisible(NULL))
  }
  if ("version" %in% DBI::dbListFields(con, "cran_analyzer_read_attempts")) {
    return(invisible(NULL))
  }
  DBI::dbExecute(con, "DROP TABLE cran_analyzer_read_attempts")
  .create_read_attempts(con)
  invisible(NULL)
}

#' Open (or create) the pipeline SQLite database.
#'
#' If the file does not yet exist it is created. The four non-summary tables
#' (cran_code_churn, cran_api_history, cran_metrics_failures,
#' cran_analyzer_read_attempts) are created with fixed schemas and indexes on
#' first open, so a database downloaded from an older release gains the ones it
#' does not have yet. cran_code_summary is created lazily by upsert_shard the
#' first time data is written (its schema is dynamic).
#'
#' @param path File path for the SQLite database.
#' @return An open DBI connection. The caller is responsible for calling
#'   DBI::dbDisconnect() when done.
open_or_init_db <- function(path) {
  con    <- DBI::dbConnect(RSQLite::SQLite(), path)
  tables <- DBI::dbListTables(con)

  if (!"cran_code_churn" %in% tables) {
    DBI::dbExecute(con, "
      CREATE TABLE cran_code_churn (
        package TEXT,
        version TEXT,
        file    TEXT,
        added   INTEGER,
        deleted INTEGER
      )")
  }

  if (!"cran_api_history" %in% tables) {
    DBI::dbExecute(con, "
      CREATE TABLE cran_api_history (
        package         TEXT,
        version         TEXT,
        exports_added   TEXT,
        exports_removed TEXT,
        n_exports       INTEGER
      )")
  }

  if (!"cran_metrics_failures" %in% tables) {
    DBI::dbExecute(con, "
      CREATE TABLE cran_metrics_failures (
        package              TEXT PRIMARY KEY,
        consecutive_failures INTEGER NOT NULL DEFAULT 0,
        last_attempt         TEXT
      )")
  }

  # Package versions handed to the analyzer that it did not read. The fields
  # the backfill queues wait on (n_fns_r, the dataset rows) come from the
  # binary alone, so a version the pure-R fallback analysed carries none of
  # them and the queue hands its package back on every run for good.
  # analyzer_version is the build that could not read it, so a later build can
  # ask again.
  #
  # Per version rather than per package, because the n_fns_r queue reads every
  # stored row: a package whose newest version the analyzer reads and whose
  # older one it cannot is a package the queue holds forever, and a record
  # taken over the whole package is cleared by the read that succeeded.
  if (!"cran_analyzer_read_attempts" %in% tables) {
    .create_read_attempts(con)
  } else {
    .migrate_read_attempts_by_version(con)
  }

  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_churn_pkg_ver ON cran_code_churn(package, version)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_churn_pkg ON cran_code_churn(package)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_api_pkg_ver ON cran_api_history(package, version)")

  con
}

#' Query the latest analyzed version per package from the DB.
#'
#' Uses latest_release_date (set by add_cross_version_metrics on the newest
#' version row) as the primary signal. Falls back to a window-function query
#' over released/rowid for packages that lack that marker.
#'
#' Memory cost: O(n_packages), not O(n_rows).
#'
#' @param con Open DBI connection to the pipeline SQLite database.
#' @return data.frame with columns package (chr) and version (chr); one row
#'   per package. Empty data.frame when cran_code_summary does not exist yet.
db_analyzed_state <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"cran_code_summary" %in% tables) {
    return(data.frame(package = character(0L), version = character(0L),
                      stringsAsFactors = FALSE))
  }

  cols <- DBI::dbListFields(con, "cran_code_summary")

  if ("latest_release_date" %in% cols) {
    # Primary: add_cross_version_metrics marks the newest-version row per package.
    primary <- DBI::dbGetQuery(con,
      "SELECT package, version
       FROM cran_code_summary
       WHERE latest_release_date IS NOT NULL")
  } else {
    primary <- data.frame(package = character(0L), version = character(0L),
                          stringsAsFactors = FALSE)
  }

  # Fallback: packages with no non-NULL latest_release_date row (e.g. legacy data
  # or missing column). Use an explicit ORDER so the result does not depend on
  # SQLite internal row order.
  order_expr <- if ("released" %in% cols) {
    "ORDER BY released DESC, rowid DESC"
  } else {
    "ORDER BY rowid DESC"
  }
  fallback <- DBI::dbGetQuery(con, sprintf("
    SELECT package, version FROM (
      SELECT package, version,
             ROW_NUMBER() OVER (
               PARTITION BY package
               %s
             ) AS rn
      FROM cran_code_summary
      WHERE package NOT IN (
        SELECT DISTINCT package FROM cran_code_summary
        WHERE latest_release_date IS NOT NULL
      )
    ) WHERE rn = 1", order_expr))

  rbind(primary, fallback)
}

#' Upsert one shard's rows into the pipeline database in-place.
#'
#' For each package present in summary_df, deletes all prior rows from the
#' three metric tables (cran_code_summary, cran_code_churn, cran_api_history),
#' then appends the fresh rows. Everything runs inside one transaction so the
#' DB is never left in a partially-written state.
#'
#' Schema growth for cran_code_summary: if summary_df contains columns not yet
#' present in the table, ALTER TABLE ... ADD COLUMN is issued for each before
#' the append. If the table does not exist yet, it is created from summary_df
#' (schema-flexible) and indexed.
#'
#' Logical columns in all three data.frames are coerced to 0/1 INTEGER.
#'
#' @param con        Open DBI connection from open_or_init_db().
#' @param summary_df data.frame; columns package + version required.
#' @param churn_df   data.frame; columns package, version, file, added, deleted.
#' @param api_df     data.frame; columns package, version, exports_added,
#'   exports_removed, n_exports.
#' @param functions_df Optional data.frame of per-function detail (package,
#'   version, lang, name, exported, file, line, loc, n_params, cyclocomp).
#'   NULL (the default) leaves cran_functions untouched.
#' @param edges_df   Optional data.frame of per-call-edge detail (package,
#'   version, graph, from, to). NULL (the default) leaves cran_call_edges
#'   untouched. Detail is expected to cover each package's latest version only;
#'   the delete-by-package step still clears any prior-version detail rows so no
#'   stale rows survive a re-analysis.
#' @return invisible(NULL)
upsert_shard <- function(con, summary_df, churn_df, api_df,
                         functions_df = NULL, edges_df = NULL,
                         vignettes_df = NULL) {
  pkgs <- unique(as.character(summary_df$package))
  if (length(pkgs) == 0L) return(invisible(NULL))

  # Defensive dedup: cran_code_summary has a UNIQUE(package, version) index, so a
  # single package that somehow yields two rows for one version would otherwise
  # abort the whole shard. Keep the last occurrence per (package, version).
  dup_key <- paste(summary_df$package, summary_df$version, sep = "\x1f")
  if (anyDuplicated(dup_key)) {
    keep_row  <- !duplicated(dup_key, fromLast = TRUE)
    summary_df <- summary_df[keep_row, , drop = FALSE]
  }

  DBI::dbWithTransaction(con, {
    # -- Delete prior rows for these packages from every table ---------------
    # Detail tables are wiped per-package (not per-version) so a package moving
    # to a new latest version does not leave its previous version's detail rows.
    .delete_by_package(con, "cran_code_summary", pkgs)
    .delete_by_package(con, "cran_code_churn",   pkgs)
    .delete_by_package(con, "cran_api_history",  pkgs)
    if (!is.null(functions_df)) .delete_by_package(con, "cran_functions",  pkgs)
    if (!is.null(edges_df))     .delete_by_package(con, "cran_call_edges", pkgs)
    # Per package, not per version, for the same reason as the other detail
    # tables: a package moving to a new latest version must not leave the
    # previous version's vignette rows behind as though it still shipped them.
    if (!is.null(vignettes_df))  .delete_by_package(con, "cran_vignettes", pkgs)

    # -- Insert fresh summary rows (with schema-growth handling) -------------
    summary_write <- .coerce_logicals(summary_df)
    tables        <- DBI::dbListTables(con)

    if (!"cran_code_summary" %in% tables) {
      # First-ever write: create the table from the data.frame schema.
      DBI::dbWriteTable(con, "cran_code_summary", summary_write,
                        row.names = FALSE, overwrite = FALSE, append = FALSE)
    } else {
      # Possibly new columns have appeared since the table was first created.
      existing_cols <- DBI::dbListFields(con, "cran_code_summary")
      for (col in setdiff(names(summary_write), existing_cols)) {
        col_type <- if (is.integer(summary_write[[col]])) "INTEGER"
                    else if (is.numeric(summary_write[[col]])) "REAL"
                    else "TEXT"
        DBI::dbExecute(con,
          sprintf("ALTER TABLE cran_code_summary ADD COLUMN \"%s\" %s",
                  col, col_type))
      }
      DBI::dbAppendTable(con, "cran_code_summary", summary_write)
    }
    DBI::dbExecute(con,
      "CREATE UNIQUE INDEX IF NOT EXISTS idx_summary_pkg_ver
       ON cran_code_summary(package, version)")

    # -- Insert fresh churn rows ---------------------------------------------
    churn_write <- .coerce_logicals(churn_df)
    if (!is.null(churn_write) && nrow(churn_write) > 0L) {
      DBI::dbAppendTable(con, "cran_code_churn", churn_write)
    }

    # -- Insert fresh api rows -----------------------------------------------
    api_write <- .coerce_logicals(api_df)
    if (!is.null(api_write) && nrow(api_write) > 0L) {
      DBI::dbAppendTable(con, "cran_api_history", api_write)
    }

    # -- Insert fresh per-function / per-call-edge detail --------------------
    .append_detail_table(con, "cran_functions",  functions_df)
    .append_detail_table(con, "cran_call_edges", edges_df)
    .append_detail_table(con, "cran_vignettes",  vignettes_df)
  })

  invisible(NULL)
}

#' Upsert one shard's dataset rows into the dataset database in-place.
#'
#' Runs the normalized-dataset write and the content GC inside one transaction
#' on the dataset connection. Separated from upsert_shard so the code and
#' dataset tables live in different files.
#'
#' @param data_con    Open DBI connection from open_or_init_data_db().
#' @param datasets_df  Per-(package, version, dataset) rows, or NULL.
#' @param pkgs         Character vector of packages written this shard.
#' @return invisible(NULL)
upsert_datasets <- function(data_con, datasets_df, pkgs) {
  pkgs <- unique(as.character(pkgs))
  DBI::dbWithTransaction(data_con, {
    .write_datasets_normalized(data_con, datasets_df, pkgs)
    .gc_dataset_contents(data_con)
  })
  invisible(NULL)
}

#' Build a per-DB insight manifest matching the pipeline MANIFEST SCHEMA.
#'
#' All values are measured from `con`; a missing table counts 0 and a missing
#' numeric column yields NULL mean/median (rendered as JSON null). bootstrap's
#' n_universe/n_remaining may be NULL when unmeasurable.
#'
#' @param con         Open DBI connection to the pipeline SQLite database.
#' @param series      "code" or "data".
#' @param repo        "owner/name" of the publishing repo.
#' @param db_filename The asset filename this manifest describes.
#' @param db_bytes    On-disk size of the DB file, in bytes.
#' @param tables      Character vector of table names to report row counts for.
#' @param fp_table    Table to fingerprint.
#' @param fp_cols     Columns within fp_table forming the fingerprint key.
#' @param pkg_table   Table to count DISTINCT package from for n_packages.
#' @param ver_table   Table to count rows from for n_versions.
#' @param stat_table  Table to probe for stat_cols.
#' @param stat_cols   Character vector of numeric columns to summarise.
#' @param bootstrap   list(n_analyzed, n_universe, n_remaining,
#'   bootstrap_complete, n_datasets_unscanned, n_datasets_unreadable,
#'   n_datasets_unmeasured). n_universe/n_remaining and the three dataset
#'   counts may be NULL, in which case they are left out.
#' @param coverage    Optional frame from dataset_column_coverage(). When given,
#'   the manifest carries how many declared columns hold nothing for anybody,
#'   so the finding outlives the run that made it. NULL leaves the block out,
#'   which is what the code series does: it has no dataset columns to measure.
#' @return A named list matching the MANIFEST SCHEMA.
build_manifest <- function(con, series, repo, db_filename, db_bytes,
                           tables, fp_table, fp_cols, pkg_table, ver_table,
                           stat_table, stat_cols, bootstrap, coverage = NULL) {
  present <- DBI::dbListTables(con)
  count_tbl <- function(t) {
    if (!t %in% present) return(0L)
    as.integer(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) n FROM "%s"', t))$n)
  }
  table_counts <- stats::setNames(lapply(tables, count_tbl), tables)

  n_packages <- if (pkg_table %in% present) {
    as.integer(DBI::dbGetQuery(con,
      sprintf('SELECT COUNT(DISTINCT package) n FROM "%s"', pkg_table))$n)
  } else 0L
  n_versions <- count_tbl(ver_table)

  # Fingerprint over the concatenation of fp_cols keys, ordered by the SQL
  # tuple (not by sorting the already-concatenated strings). Code-series
  # keys join fields with ":" (matching db_fingerprint()); data-series keys
  # join fields with "\x1f" per the manifest schema.
  fp_sep <- if (identical(series, "code")) ":" else "\x1f"
  fingerprint <- {
    if (!fp_table %in% present) {
      digest::digest("", algo = "sha256", serialize = FALSE)
    } else {
      cols <- paste(sprintf('"%s"', fp_cols), collapse = ", ")
      df <- DBI::dbGetQuery(con,
        sprintf('SELECT %s FROM "%s" ORDER BY %s', cols, fp_table, cols))
      keys <- if (nrow(df) == 0L) character(0L) else
        apply(df, 1L, function(r) paste(r, collapse = fp_sep))
      # Rows are ordered by SQLite's ORDER BY (BINARY collation, i.e. byte
      # order) *before* concatenation, exactly matching db_fingerprint()'s
      # "ORDER BY package, version". Sorting the already-concatenated
      # "package:version" strings in R instead is NOT equivalent: whenever
      # one key is a prefix of another followed by a character below ':'
      # (0x3a) -- e.g. package "Rcpp" vs "Rcpp11" -- tuple order and
      # concatenated-string order disagree, so the two fingerprints would
      # diverge for real CRAN data.
      digest::digest(paste(keys, collapse = ","),
                     algo = "sha256", serialize = FALSE)
    }
  }

  # Stats: mean/median per column that exists AND is numeric, else NULL.
  # A non-numeric column (e.g. character) must never be coerced into a
  # fabricated statistic.
  stat_fields <- list()
  stat_cols_present <- if (stat_table %in% present) DBI::dbListFields(con, stat_table) else character(0L)
  for (col in stat_cols) {
    if (col %in% stat_cols_present) {
      v <- DBI::dbGetQuery(con, sprintf('SELECT "%s" AS v FROM "%s"', col, stat_table))$v
      if (is.numeric(v)) {
        v <- v[!is.na(v)]
        stat_fields[[paste0(col, "_mean")]]   <- if (length(v)) mean(v) else NULL
        stat_fields[[paste0(col, "_median")]] <- if (length(v)) stats::median(v) else NULL
      } else {
        stat_fields[[paste0(col, "_mean")]]   <- NULL
        stat_fields[[paste0(col, "_median")]] <- NULL
      }
    } else {
      stat_fields[[paste0(col, "_mean")]]   <- NULL
      stat_fields[[paste0(col, "_median")]] <- NULL
    }
  }

  out <- list(
    schema_version = 1L,
    series         = series,
    repo           = repo,
    db_filename    = db_filename,
    generated_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    db_bytes       = round(as.numeric(db_bytes)),
    fingerprint    = fingerprint,
    n_packages     = n_packages,
    n_versions     = n_versions,
    tables         = table_counts,
    stats          = stat_fields,
    bootstrap      = list(
      n_analyzed         = bootstrap$n_analyzed,
      n_universe         = bootstrap$n_universe,
      n_remaining        = bootstrap$n_remaining,
      bootstrap_complete = isTRUE(bootstrap$bootstrap_complete),
      # A different question from bootstrap_complete, and one it hides:
      # completion is measured against the code analysis, so it reads true
      # while packages sit with no dataset scan at all and no queue that will
      # ever pick them up.
      n_datasets_unscanned = bootstrap$n_datasets_unscanned,
      # How many of those the pipeline has stopped asking about: asked to the
      # cap under this analyzer build and never read. The count above comes
      # down as the backfill drains and this one does not, so it is the one
      # that says what the corpus is missing for good, until a build that can
      # read them arrives.
      n_datasets_unreadable = bootstrap$n_datasets_unreadable,
      # How many datasets are in the catalog with nothing behind them: the
      # reader described them and could not fingerprint them, so they have an
      # identity row and a version link and no profile. Unlike the two counts
      # above it is per dataset rather than per package, and the table count
      # beside it in this same file is its denominator. It is here rather than
      # only in a line the shard prints because that line scrolls away with the
      # run, and a shard where this number jumps is the one worth seeing.
      n_datasets_unmeasured = bootstrap$n_datasets_unmeasured
    )
  )

  # The names are capped and the count is not. A reader chasing this wants the
  # number first, and enough names to start looking; the full list is a query
  # against the database the manifest describes.
  if (!is.null(coverage)) {
    all_null <- coverage[coverage$n_rows > 0L & coverage$measured == 0L, , drop = FALSE]
    named <- sort(paste(all_null$table, all_null$column, sep = "."))
    out$coverage <- list(
      n_columns  = nrow(coverage),
      n_all_null = nrow(all_null),
      all_null   = head(named, 20L)
    )
  }
  out
}

#' Union `pkgs` into a sorted, deduped newline file at `path` (accumulates the
#' run's changed set across shards for the changelog).
#'
#' @param path Newline-delimited text file. Created if absent.
#' @param pkgs Character vector of package names touched this run.
#' @return Invisibly NULL.
record_changed_packages <- function(path, pkgs) {
  existing <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0L)
  all <- sort(unique(c(existing, as.character(pkgs))))
  all <- all[nzchar(all)]
  writeLines(all, path)
  invisible(NULL)
}

#' Read the accumulated changed-package set (empty vector if absent).
#'
#' @param path Newline-delimited text file as written by record_changed_packages().
#' @return Character vector, sorted as stored; character(0L) when path is absent.
read_changed_packages <- function(path) {
  if (!file.exists(path)) return(character(0L))
  x <- readLines(path, warn = FALSE)
  x[nzchar(x)]
}

#' Compute a SHA-256 fingerprint over the current package:version set in the DB.
#'
#' Queries only the two key columns (result bounded to O(n_packages)) and
#' hashes the sorted "package:version" strings. Semantically equivalent to
#' metrics_fingerprint() but reads from the live DB rather than a data.frame.
#'
#' @param con Open DBI connection to the pipeline SQLite database.
#' @return 64-character lower-case hex string (SHA-256).
db_fingerprint <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"cran_code_summary" %in% tables) {
    return(digest::digest("", algo = "sha256", serialize = FALSE))
  }
  df   <- DBI::dbGetQuery(con,
    "SELECT package, version FROM cran_code_summary ORDER BY package, version")
  keys <- if (nrow(df) == 0L) character(0L) else paste(df$package, df$version, sep = ":")
  digest::digest(paste(keys, collapse = ","), algo = "sha256", serialize = FALSE)
}

# ---------------------------------------------------------------------------
# cran_archived_meta: narrow, point-lookup identity table for ARCHIVED packages
# ---------------------------------------------------------------------------
# The viewer's archived-package detail page reads a single row from this table by
# `WHERE package = ?`. It must NEVER join the ~200-column cran_code_summary at
# query time, so the identity fields are projected here into a WITHOUT ROWID
# table keyed on package (a clustered point-lookup, no secondary indexes).

# Target column -> source column in cran_code_summary. `title`/`description`
# arrive once a package is scanned by the current pipeline; older rows lack them
# and are topped up by the harvest path. Every source may be absent from a given
# DB's dynamic schema, so presence is checked before it is read.
.ARCHIVED_META_SOURCES <- c(
  title            = "title",
  description      = "description",
  authors          = "authors",
  maintainer       = "maintainer",
  maintainer_email = "maintainer_email",
  license          = "license",
  url              = "url",
  depends          = "depends",
  imports          = "imports",
  suggests         = "suggests",
  linkingto        = "linking_to",
  enhances         = "enhances"
)

# Full column order for INSERTs into cran_archived_meta.
.ARCHIVED_META_COLS <- c(
  "package", "last_version", "title", "description", "authors", "maintainer",
  "maintainer_email", "license", "url", "depends", "imports", "suggests",
  "linkingto", "enhances", "desc_sha", "source_scanned_at"
)

#' Create the narrow archived-metadata table if it does not exist.
#'
#' WITHOUT ROWID with PRIMARY KEY (package): the viewer's point-lookup lands on
#' the clustered key with no rowid indirection and no secondary index to consult.
.ensure_archived_meta_table <- function(con) {
  if ("cran_archived_meta" %in% DBI::dbListTables(con)) return(invisible(NULL))
  DBI::dbExecute(con, "
    CREATE TABLE cran_archived_meta (
      package           TEXT NOT NULL,
      last_version      TEXT,
      title             TEXT,
      description       TEXT,
      authors           TEXT,
      maintainer        TEXT,
      maintainer_email  TEXT,
      license           TEXT,
      url               TEXT,
      depends           TEXT,
      imports           TEXT,
      suggests          TEXT,
      linkingto         TEXT,
      enhances          TEXT,
      desc_sha          TEXT,
      source_scanned_at TEXT,
      PRIMARY KEY (package)
    ) WITHOUT ROWID")
  invisible(NULL)
}

#' Reduce a multi-version slice to one row per package: the LAST version.
#'
#' "Last" is the maximum by numeric_version() ordering (NOT string max, so
#' 1.10 > 1.2). Versions that do not parse are deprioritised and only chosen
#' when a package has no parseable version; ties are broken by the raw version
#' string so the pick is deterministic. Every column of the winning row is kept.
#'
#' @param rows data.frame with at least columns `package` and `version`.
#' @return data.frame, one row per distinct package in `rows`.
.pick_last_version_rows <- function(rows) {
  if (nrow(rows) == 0L) return(rows)
  parts <- split(seq_len(nrow(rows)), rows$package)
  keep  <- vapply(parts, function(idx) {
    vs <- as.character(rows$version[idx])
    nv <- numeric_version(vs, strict = FALSE)
    o  <- order(nv, vs)              # ascending; unparseable (NA) sort last
    ok <- !is.na(nv[o])
    if (any(ok)) idx[o[ok][sum(ok)]] # last parseable in ascending order = max
    else         idx[o[length(o)]]   # all unparseable: last by string order
  }, integer(1L))
  rows[keep, , drop = FALSE]
}

# Run the parameterized UPSERT for a fully-built cran_archived_meta frame.
# `on_conflict` is the SET body appended to `ON CONFLICT(package) DO UPDATE SET`.
.write_archived_meta <- function(con, df, on_conflict) {
  if (is.null(df) || nrow(df) == 0L) return(invisible(NULL))
  df <- df[, .ARCHIVED_META_COLS, drop = FALSE]
  ph <- paste(rep("?", length(.ARCHIVED_META_COLS)), collapse = ", ")
  sql <- sprintf(
    "INSERT INTO cran_archived_meta (%s) VALUES (%s)\nON CONFLICT(package) DO UPDATE SET\n%s",
    paste(.ARCHIVED_META_COLS, collapse = ", "), ph, on_conflict)
  # unname(): the ? placeholders are positional, so params must not be named.
  DBI::dbExecute(con, sql, params = unname(lapply(df, as.character)))
  invisible(NULL)
}

#' Project each archived package's LAST-version row from cran_code_summary into
#' the narrow cran_archived_meta table. Pure re-shaping of data already in the
#' DB: no tarball download, no re-scan.
#'
#' The UPSERT is non-destructive. It refreshes the projected fields from the wide
#' table but never overwrites a non-NULL value with NULL (COALESCE), so titles or
#' descriptions previously filled by the harvest path survive a re-projection.
#' When the wide table now carries a title (a package re-scanned by the current
#' pipeline), that title wins and desc_sha is cleared (the row is projection-
#' sourced again, no longer harvest-validated).
#'
#' @param con           Open DBI connection to the pipeline database.
#' @param archived_pkgs Character vector of archived package names (those the
#'   universe marks with latest_version = NA).
#' @param scanned_at    Run timestamp stored in source_scanned_at.
#' @return invisible(NULL)
project_archived_meta <- function(con, archived_pkgs,
                                  scanned_at = format(Sys.time(),
                                    "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  .ensure_archived_meta_table(con)
  archived_pkgs <- unique(as.character(archived_pkgs))
  archived_pkgs <- archived_pkgs[!is.na(archived_pkgs) & nzchar(archived_pkgs)]
  if (!"cran_code_summary" %in% DBI::dbListTables(con) ||
      length(archived_pkgs) == 0L) {
    return(invisible(NULL))
  }

  existing_cols <- DBI::dbListFields(con, "cran_code_summary")
  src_present   <- .ARCHIVED_META_SOURCES[.ARCHIVED_META_SOURCES %in% existing_cols]
  sel_cols      <- unique(c("package", "version", unname(src_present)))
  select        <- paste(sprintf('"%s"', sel_cols), collapse = ", ")

  rows <- .fetch_by_package(con, "cran_code_summary", archived_pkgs, select = select)
  if (nrow(rows) == 0L) return(invisible(NULL))
  last <- .pick_last_version_rows(rows)
  n    <- nrow(last)

  src <- function(target) {
    s <- .ARCHIVED_META_SOURCES[[target]]
    if (s %in% names(last)) as.character(last[[s]]) else rep(NA_character_, n)
  }
  meta <- data.frame(
    package           = as.character(last$package),
    last_version      = as.character(last$version),
    title             = src("title"),
    description       = src("description"),
    authors           = src("authors"),
    maintainer        = src("maintainer"),
    maintainer_email  = src("maintainer_email"),
    license           = src("license"),
    url               = src("url"),
    depends           = src("depends"),
    imports           = src("imports"),
    suggests          = src("suggests"),
    linkingto         = src("linkingto"),
    enhances          = src("enhances"),
    desc_sha          = NA_character_,   # set only by the harvest path
    source_scanned_at = rep(as.character(scanned_at), n),
    stringsAsFactors  = FALSE
  )

  # COALESCE(excluded, existing) never downgrades a stored value to NULL, so a
  # prior harvest is preserved; title carries desc_sha's reset with it.
  on_conflict <- paste(
    "  last_version      = excluded.last_version,",
    "  authors           = COALESCE(excluded.authors, cran_archived_meta.authors),",
    "  maintainer        = COALESCE(excluded.maintainer, cran_archived_meta.maintainer),",
    "  maintainer_email  = COALESCE(excluded.maintainer_email, cran_archived_meta.maintainer_email),",
    "  license           = COALESCE(excluded.license, cran_archived_meta.license),",
    "  url               = COALESCE(excluded.url, cran_archived_meta.url),",
    "  depends           = COALESCE(excluded.depends, cran_archived_meta.depends),",
    "  imports           = COALESCE(excluded.imports, cran_archived_meta.imports),",
    "  suggests          = COALESCE(excluded.suggests, cran_archived_meta.suggests),",
    "  linkingto         = COALESCE(excluded.linkingto, cran_archived_meta.linkingto),",
    "  enhances          = COALESCE(excluded.enhances, cran_archived_meta.enhances),",
    "  title             = COALESCE(excluded.title, cran_archived_meta.title),",
    "  description       = COALESCE(excluded.description, cran_archived_meta.description),",
    "  desc_sha          = CASE WHEN excluded.title IS NOT NULL THEN NULL",
    "                           ELSE cran_archived_meta.desc_sha END,",
    "  source_scanned_at = excluded.source_scanned_at",
    sep = "\n")

  DBI::dbWithTransaction(con, .write_archived_meta(con, meta, on_conflict))
  invisible(NULL)
}

#' UPSERT one harvested row into cran_archived_meta.
#'
#' The harvest path is authoritative for title/description/desc_sha (it read them
#' straight from the package's own DESCRIPTION); the remaining projected fields
#' are topped up only where the harvest produced a value (COALESCE), so a field
#' the DESCRIPTION omits keeps whatever the projection supplied.
#'
#' @param con     Open DBI connection.
#' @param row_df  One-row data.frame carrying all .ARCHIVED_META_COLS.
#' @return invisible(NULL)
upsert_archived_meta_row <- function(con, row_df) {
  .ensure_archived_meta_table(con)
  on_conflict <- paste(
    "  last_version      = excluded.last_version,",
    "  title             = excluded.title,",
    "  description       = excluded.description,",
    "  authors           = COALESCE(excluded.authors, cran_archived_meta.authors),",
    "  maintainer        = COALESCE(excluded.maintainer, cran_archived_meta.maintainer),",
    "  maintainer_email  = COALESCE(excluded.maintainer_email, cran_archived_meta.maintainer_email),",
    "  license           = COALESCE(excluded.license, cran_archived_meta.license),",
    "  url               = COALESCE(excluded.url, cran_archived_meta.url),",
    "  depends           = COALESCE(excluded.depends, cran_archived_meta.depends),",
    "  imports           = COALESCE(excluded.imports, cran_archived_meta.imports),",
    "  suggests          = COALESCE(excluded.suggests, cran_archived_meta.suggests),",
    "  linkingto         = COALESCE(excluded.linkingto, cran_archived_meta.linkingto),",
    "  enhances          = COALESCE(excluded.enhances, cran_archived_meta.enhances),",
    "  desc_sha          = excluded.desc_sha,",
    "  source_scanned_at = excluded.source_scanned_at",
    sep = "\n")
  .write_archived_meta(con, row_df, on_conflict)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# cran_author_package_span: when each author joined (and left) each package
# ---------------------------------------------------------------------------
# cran_code_summary knows only when a PACKAGE first appeared, so an author added
# to a 2010 package in 2024 looked like they had been on CRAN since 2010. This
# table records, per (author, package), the first and last package-version that
# actually lists them. It is a projection over rows already in the DB: no
# download, no re-scan, no re-analysis.
#
# Author page:  SELECT MIN(first_seen) ... WHERE author_key = ?  (PK prefix)
# Package page: SELECT ...              WHERE package = ?        (idx_caps_package)

# Column order for INSERTs into cran_author_package_span.
.AUTHOR_SPAN_COLS <- c(
  "author_key", "package", "given", "family", "first_version", "first_seen",
  "last_version", "last_seen", "n_versions"
)

# Zero-row frame with the exact span column types.
.empty_author_spans <- function() {
  data.frame(
    author_key    = character(0L),
    package       = character(0L),
    given         = character(0L),
    family        = character(0L),
    first_version = character(0L),
    first_seen    = character(0L),
    last_version  = character(0L),
    last_seen     = character(0L),
    n_versions    = integer(0L),
    stringsAsFactors = FALSE
  )
}

#' Drop and recreate the span table (and its package index) from scratch.
#'
#' The projection is a full rebuild every run, so the table is dropped rather
#' than upserted: the result depends only on cran_code_summary, which makes it
#' deterministic and idempotent, and drops authors whose rows have gone away.
#' WITHOUT ROWID + PRIMARY KEY (author_key, package) makes the author page's
#' MIN(first_seen) lookup a clustered scan of one key prefix; idx_caps_package
#' serves the package page's reverse lookup.
.rebuild_author_span_table <- function(con) {
  DBI::dbExecute(con, "DROP TABLE IF EXISTS cran_author_package_span")
  DBI::dbExecute(con, "
    CREATE TABLE cran_author_package_span (
      author_key    TEXT NOT NULL,
      package       TEXT NOT NULL,
      given         TEXT,
      family        TEXT,
      first_version TEXT,
      first_seen    TEXT,
      last_version  TEXT,
      last_seen     TEXT,
      n_versions    INTEGER,
      PRIMARY KEY (author_key, package)
    ) WITHOUT ROWID")
  DBI::dbExecute(con,
    "CREATE INDEX idx_caps_package ON cran_author_package_span(package)")
  invisible(NULL)
}

#' Build a lexicographically-sortable key out of a version string.
#'
#' numeric_version() gets the ordering right (1.10 > 1.9), but ordering the whole
#' table by it costs seconds, because order() has to xtfrm the underlying list.
#' Zero-padding every numeric component to 10 digits gives a plain string whose
#' C-locale (radix) order is the same, for a fraction of the cost:
#'   "1.9"  -> "0000000001.0000000009"
#'   "1.10" -> "0000000001.0000000010"
#' "-" and "." are the same separator to numeric_version(), so they are folded
#' together first. Vectorised; NA in, NA out (which order() then sorts last).
#'
#' @param v Character vector of version strings.
#' @return Character vector of sort keys (comparable only against each other).
.version_sort_key <- function(v) {
  x <- gsub("-", ".", v, fixed = TRUE)
  x <- gsub("([0-9]+)", "000000000\\1", x)   # 9 leading zeros on every digit run
  gsub("0*([0-9]{10})", "\\1", x)            # keep the last 10 digits of each run
}

#' Reduce a package-version slice to one span row per (author_key, package).
#'
#' Rows are ordered within each package by `released` ascending, tie-broken by
#' version number (so 1.10 follows 1.9) and then the raw version string; a
#' missing date sorts last, keeping the order total and deterministic. Every
#' version's `authors` JSON is then read in one vectorised pass
#' (.xv_author_pairs) and each author identity's first and last appearance is
#' picked out by position.
#'
#' @param rows data.frame with columns package, version and (optionally)
#'   released, authors. May carry rows for many packages, but every row of a
#'   package must be present or its span will be wrong: callers chunk by package.
#' @return data.frame with .AUTHOR_SPAN_COLS; zero rows when no author is found.
.author_spans_from_rows <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0L ||
      !all(c("package", "version") %in% names(rows))) {
    return(.empty_author_spans())
  }
  n   <- nrow(rows)
  pkg <- as.character(rows$package)
  ver <- as.character(rows$version)
  rel <- if ("released" %in% names(rows)) as.character(rows$released) else rep(NA_character_, n)
  aut <- if ("authors"  %in% names(rows)) as.character(rows$authors)  else rep(NA_character_, n)

  o   <- order(pkg, rel, .version_sort_key(ver), ver, method = "radix")
  pkg <- pkg[o]; ver <- ver[o]; rel <- rel[o]; aut <- aut[o]

  pairs <- .xv_author_pairs(aut)
  # A nameless entry (no given AND no family) is not an identity: drop it rather
  # than collapse every such author of a package onto one blank key.
  pairs <- pairs[nzchar(trimws(pairs$author_key)), , drop = FALSE]
  if (nrow(pairs) == 0L) return(.empty_author_spans())

  # `row` indexes the ordered rows and is ascending, so within a group the FIRST
  # pair seen is the earliest version and the LAST is the most recent one.
  ri  <- pairs$row
  key <- paste(pairs$author_key, pkg[ri], sep = "\r")

  # An author listed twice in one DESCRIPTION must not inflate n_versions.
  keep  <- !duplicated(paste(key, ri, sep = "\r"))
  pairs <- pairs[keep, , drop = FALSE]
  ri    <- ri[keep]
  key   <- key[keep]

  ukeys <- key[!duplicated(key)]
  idx   <- match(key, ukeys)
  k     <- length(ukeys)
  pos   <- seq_along(idx)
  first <- integer(k); first[rev(idx)] <- rev(pos)   # last write wins -> earliest
  last  <- integer(k); last[idx]       <- pos        # last write wins -> latest
  n_ver <- tabulate(idx, nbins = k)

  rf <- ri[first]
  rl <- ri[last]
  data.frame(
    author_key    = pairs$author_key[first],
    package       = pkg[rf],
    given         = pairs$given[last],               # display form: most recent
    family        = pairs$family[last],
    first_version = ver[rf],
    first_seen    = rel[rf],
    last_version  = ver[rl],
    last_seen     = rel[rl],
    n_versions    = as.integer(n_ver),
    stringsAsFactors = FALSE
  )
}

#' Project the author spans of every package in the DB into
#' cran_author_package_span. Pure re-shaping of data already in the DB: no
#' tarball download, no re-scan.
#'
#' Reads only package/version/released/authors -- never the ~200-column wide row
#' -- and streams that narrow slice out of SQLite `fetch_rows` at a time. The
#' read is one sequential scan rather than a per-package indexed lookup: on the
#' full table the random-access form costs ~6x more, and the four columns of
#' every package-version together weigh only tens of MB. The CPU work is then
#' done a package batch at a time, so the parse never expands the whole table's
#' authors at once. A package's rows must all reach the same batch, hence the
#' sort by package before batching.
#'
#' @param con        Open DBI connection to the pipeline database.
#' @param chunk_size Packages per CPU batch.
#' @param fetch_rows Rows per SQLite fetch.
#' @return invisible(NULL)
project_author_spans <- function(con, chunk_size = 2000L, fetch_rows = 100000L) {
  cols <- if ("cran_code_summary" %in% DBI::dbListTables(con)) {
    DBI::dbListFields(con, "cran_code_summary")
  } else {
    character(0L)
  }
  # An empty (or author-less) source still leaves the viewer a table to query.
  if (!all(c("package", "version", "authors") %in% cols)) {
    DBI::dbWithTransaction(con, .rebuild_author_span_table(con))
    return(invisible(NULL))
  }

  # `released` is the release date; older schemas called it `date`.
  date_col <- if ("released" %in% cols) "released" else if ("date" %in% cols) "date" else NA_character_
  sel      <- c("package", "version", "authors", if (!is.na(date_col)) date_col)
  select   <- paste(sprintf('"%s"', sel), collapse = ", ")

  # ---- read: one sequential scan of the four narrow columns, in chunks -------
  rows <- local({
    rs <- DBI::dbSendQuery(con,
      sprintf("SELECT %s FROM cran_code_summary", select))
    on.exit(DBI::dbClearResult(rs), add = TRUE)
    acc <- list()
    repeat {
      chunk <- DBI::dbFetch(rs, n = fetch_rows)
      if (nrow(chunk) == 0L) break
      acc[[length(acc) + 1L]] <- chunk
    }
    if (length(acc) == 0L) NULL else do.call(rbind, acc)
  })

  spans <- list()
  if (!is.null(rows) && nrow(rows) > 0L) {
    if (!is.na(date_col) && !identical(date_col, "released")) {
      names(rows)[names(rows) == date_col] <- "released"
    }
    rows <- rows[order(rows$package, method = "radix"), , drop = FALSE]

    # Batch on package boundaries: chunk_size packages of complete history each.
    runs  <- rle(rows$package)$lengths
    batch <- rep(((seq_along(runs) - 1L) %/% chunk_size) + 1L, runs)
    for (i in split(seq_len(nrow(rows)), batch)) {
      part <- .author_spans_from_rows(rows[i, , drop = FALSE])
      if (nrow(part) > 0L) spans[[length(spans) + 1L]] <- part
    }
  }
  out <- if (length(spans) > 0L) do.call(rbind, spans) else .empty_author_spans()
  # Insert in primary-key order: sequential appends into the clustered B-tree.
  out <- out[order(out$author_key, out$package), .AUTHOR_SPAN_COLS, drop = FALSE]

  DBI::dbWithTransaction(con, {
    .rebuild_author_span_table(con)
    if (nrow(out) > 0L) {
      DBI::dbAppendTable(con, "cran_author_package_span", out)
    }
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Rich release notes (headline + per-package metrics table + catalog summary)
# ---------------------------------------------------------------------------

#' Render a byte count as a compact human-readable string.
#'
#' Bytes below 1024 render as "N bytes"; above that, KB/MB/GB in powers of
#' 1024, whole numbers for KB and MB, one decimal place for GB. NULL/NA (an
#' unmeasured size) renders as "n/a", never a fabricated 0.
#'
#' @param n A single byte count (numeric), or NULL/NA.
#' @return A one-line string, e.g. "870 MB", "1.4 GB", "235 KB", "512 bytes".
format_bytes <- function(n) {
  if (is.null(n) || length(n) == 0L || is.na(n)) return("n/a")
  n <- as.numeric(n)
  # Round half up for the whole-number units so an exact x.5 boundary
  # (e.g. 240128 / 1024 = 234.5) matches everyday expectation rather than
  # R's round-half-to-even default (which would report 234 KB).
  half_up <- function(x) floor(x + 0.5)
  if (n < 1024)  return(sprintf("%d bytes", as.integer(half_up(n))))
  kb <- n / 1024
  if (kb < 1024) return(sprintf("%d KB", as.integer(half_up(kb))))
  mb <- kb / 1024
  if (mb < 1024) return(sprintf("%d MB", as.integer(half_up(mb))))
  gb <- mb / 1024
  sprintf("%.1f GB", gb)
}

#' Fetch all rows for a set of packages from one table, chunking IN-lists to
#' <= 900 params so a large changed-package set never exceeds SQLite's
#' bound-parameter limit. Mirrors the chunking pattern in .delete_by_package.
#'
#' @param con    Open DBI connection.
#' @param table  Table name (trusted; not user input).
#' @param pkgs   Character vector of package names to fetch (deduped).
#' @param select SELECT-list fragment, inserted verbatim (default "*").
#' @return data.frame of matching rows (any number per package); a 0x0
#'   data.frame when the table is absent or pkgs is empty.
.fetch_by_package <- function(con, table, pkgs, select = "*") {
  pkgs <- unique(as.character(pkgs))
  if (!table %in% DBI::dbListTables(con) || length(pkgs) == 0L) {
    return(data.frame())
  }
  chunk_size <- 900L
  out <- list()
  for (i in seq(1L, length(pkgs), by = chunk_size)) {
    chunk <- pkgs[i:min(i + chunk_size - 1L, length(pkgs))]
    ph    <- paste(rep("?", length(chunk)), collapse = ", ")
    out[[length(out) + 1L]] <- DBI::dbGetQuery(
      con,
      sprintf("SELECT %s FROM %s WHERE package IN (%s)", select, table, ph),
      params = as.list(chunk))
  }
  do.call(rbind, out)
}

#' Pick each package's "latest tracked version" row out of a multi-row slice
#' of cran_code_summary.
#'
#' Winner per package: the row with a non-NA latest_release_date (there is
#' at most one, per add_cross_version_metrics, which stamps it only on the
#' newest-version row); ties (or a schema that lacks the column) fall back
#' to version (lexicographic, descending), then to insertion order via the
#' rowid_ column (expected to be selected as `rowid AS rowid_, *`) when
#' present.
#'
#' Selected with one order() over the whole slice rather than split()/rbind()
#' per package. Same winner, and the difference is not academic: the notes are
#' rendered from every changed package, so a --recollect run put 33,282
#' packages and 207,465 rows through this function, where the per-package
#' split-and-rebind cost three minutes of the shard's clock.
#'
#' @param rows data.frame from .fetch_by_package() for cran_code_summary;
#'   may have zero rows.
#' @return data.frame, one row per distinct package present in `rows`,
#'   ordered by package name.
.pick_latest_rows <- function(rows) {
  n <- nrow(rows)
  if (n == 0L) return(rows)
  marked <- if ("latest_release_date" %in% names(rows)) {
    !is.na(rows$latest_release_date)
  } else {
    rep(FALSE, n)
  }
  # Version only breaks ties among marked rows, matching the per-package rule:
  # an unmarked group is decided by rowid, not by version.
  vkey <- ifelse(marked, as.character(rows$version), "")
  rid  <- if ("rowid_" %in% names(rows)) as.numeric(rows$rowid_) else rep(0, n)
  ord  <- order(rows$package, !marked, -xtfrm(vkey), -rid, seq_len(n))
  keep <- ord[!duplicated(rows$package[ord])]
  rows[keep, , drop = FALSE]
}

#' Derive the notes table's numeric metrics from latest-version rows of
#' cran_code_summary, binding to the real schema with the documented
#' fallbacks. A column absent from the frame's schema, or NA for a specific
#' package, yields NA (rendered "n/a" downstream) -- never a fabricated 0.
#'
#' Vectorised over rows: one call covers the whole changed set, because a
#' catch-up run hands this function tens of thousands of packages.
#'
#' @param rows data.frame of one row per package (as returned by
#'   .pick_latest_rows()); may have zero rows.
#' @return list(loc_r, functions, exports, deps, api_added, api_removed,
#'   bump); each a vector as long as nrow(rows).
.row_metrics <- function(rows) {
  n   <- nrow(rows)
  has <- function(col) col %in% names(rows)
  num <- function(col) {
    if (!has(col)) return(rep(NA_real_, n))
    suppressWarnings(as.numeric(rows[[col]]))
  }
  chr <- function(col) {
    if (!has(col)) return(rep(NA_character_, n))
    as.character(rows[[col]])
  }

  loc_r <- num("loc_r")

  # Functions: n_exports + n_internal when the schema carries the split
  # columns; the fused rpkg-analyzer field n_fns_r only when it does not.
  ne <- num("n_exports"); ni <- num("n_internal")
  functions <- if (has("n_exports") || has("n_internal")) {
    ifelse(is.na(ne) & is.na(ni), NA_real_,
           ifelse(is.na(ne), 0, ne) + ifelse(is.na(ni), 0, ni))
  } else {
    num("n_fns_r")
  }

  exports <- ne

  # Deps: n_deps_direct when available; else a best-effort count parsed out
  # of the raw Depends/Imports DESCRIPTION text (excluding R itself). The
  # parse runs only for the rows that need it, which is normally none.
  deps <- num("n_deps_direct")
  gap  <- which(is.na(deps))
  if (length(gap) > 0L && (has("depends") || has("imports"))) {
    dep_txt <- chr("depends")[gap]
    imp_txt <- chr("imports")[gap]
    deps[gap] <- vapply(seq_along(gap), function(i) {
      parts <- c(dep_txt[i], imp_txt[i])
      parts <- parts[!is.na(parts)]
      txt   <- paste(parts, collapse = ",")
      if (!nzchar(trimws(txt))) return(NA_real_)
      pkg_names <- strsplit(txt, ",", fixed = TRUE)[[1L]]
      pkg_names <- trimws(sub("\\s*\\(.*", "", pkg_names, perl = TRUE))
      pkg_names <- pkg_names[nzchar(pkg_names) & !grepl("^R$", pkg_names, perl = TRUE)]
      as.numeric(length(pkg_names))
    }, numeric(1L))
  }

  # What the release actually did to the package's API, which the summary row
  # already carries per version and nothing downstream of the manifest reads.
  list(loc_r = loc_r, functions = functions, exports = exports, deps = deps,
       api_added = num("exports_added_n"), api_removed = num("exports_removed_n"),
       bump = chr("bump_type"))
}

#' Count each package's rows in the dataset database's identity table.
#'
#' @param data_con Open DBI connection to the dataset database, or NULL.
#' @param pkgs     Character vector of packages to count for.
#' @return Named integer vector (names = pkgs). NA when the dataset DB/table
#'   is unavailable (unmeasurable); a real 0 when the table exists but a
#'   package simply has no dataset rows.
.count_datasets <- function(data_con, pkgs) {
  pkgs <- unique(as.character(pkgs))
  if (length(pkgs) == 0L) return(stats::setNames(integer(0L), character(0L)))
  if (is.null(data_con) || !("cran_datasets" %in% DBI::dbListTables(data_con))) {
    return(stats::setNames(rep(NA_integer_, length(pkgs)), pkgs))
  }
  rows <- .fetch_by_package(data_con, "cran_datasets", pkgs, select = "package")
  tab  <- table(factor(rows$package, levels = pkgs))
  stats::setNames(as.integer(tab), pkgs)
}

#' Format a scalar for display: "n/a" for NULL/NA, else comma-grouped.
#'
#' @param x A length-0/1 numeric-ish value.
#' @return A one-line string.
.fmt_n <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("n/a")
  format(round(as.numeric(x)), big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' Size of a rendered notes body in bytes, counted the way GitHub receives it.
#'
#' writeLines() terminates every line, so each line costs its own bytes plus a
#' newline. Bytes rather than characters because the body is compared against a
#' limit on what is sent, and a package or maintainer name can be multi-byte.
#'
#' @param lines Character vector of markdown lines.
#' @return Integer byte count.
.notes_bytes <- function(lines) {
  if (length(lines) == 0L) return(0L)
  as.integer(sum(nchar(lines, type = "bytes")) + length(lines))
}

#' A parenthesised, signed change against the previous release, or "" when
#' there is nothing to compare against.
#'
#' Zero prints as "(+0)" rather than vanishing: a bullet with no parenthesis
#' means no baseline, and a reader has to be able to tell that apart from a
#' figure that did not move.
#'
#' @param cur  Current figure (numeric-ish scalar), or NULL.
#' @param prev Previous release's figure (numeric-ish scalar), or NULL.
#' @return "" or a string like " (+1,204)".
.fmt_delta <- function(cur, prev) {
  if (is.null(cur) || is.null(prev)) return("")
  a <- suppressWarnings(as.numeric(cur))
  b <- suppressWarnings(as.numeric(prev))
  if (length(a) != 1L || length(b) != 1L || is.na(a) || is.na(b)) return("")
  d <- a - b
  sprintf(" (%s%s)", if (d >= 0) "+" else "-", .fmt_n(abs(d)))
}

#' Fetch a nested manifest field (e.g. "tables.cran_datasets") without
#' erroring on a manifest that predates the field.
#'
#' @param manifest Parsed manifest (list), or NULL.
#' @param ...      Path components.
#' @return The value, or NULL when any component is absent.
.manifest_at <- function(manifest, ...) {
  path <- c(...)
  cur <- manifest
  for (p in path) {
    if (is.null(cur) || !is.list(cur) || !(p %in% names(cur))) return(NULL)
    cur <- cur[[p]]
  }
  if (length(cur) == 0L) NULL else cur
}

#' Build the one-paragraph headline: new/updated counts, catalog size, and
#' the bootstrap clause.
#'
#' @param code_manifest Parsed code-manifest.json (list).
#' @param changed_pkgs  Character vector, this run's changed packages.
#' @param seed_pkgs     Character vector, the prior release's package set
#'   ("new to the catalog" = not present here).
#' @param run_status    Parsed run-status.json (list), or NULL. Used only to
#'   report the shard's failures, which are otherwise visible nowhere but the
#'   CI log.
#' @return Character vector of one or two lines (one markdown paragraph).
.build_headline <- function(code_manifest, changed_pkgs, seed_pkgs,
                            run_status = NULL) {
  n_changed <- length(changed_pkgs)
  n_new     <- sum(!changed_pkgs %in% seed_pkgs)
  n_updated <- n_changed - n_new

  bs <- code_manifest$bootstrap
  bootstrap_clause <- if (is.null(bs) || is.null(bs$n_universe)) {
    ""
  } else if (isTRUE(bs$bootstrap_complete)) {
    " Bootstrap complete."
  } else {
    n_universe  <- as.numeric(bs$n_universe)
    n_remaining <- as.numeric(bs$n_remaining %||% 0)
    if (length(n_remaining) == 0L || is.na(n_remaining) || n_remaining < 0) {
      n_remaining <- 0
    }
    if (is.na(n_universe) || n_universe <= 0) {
      # Degenerate/empty universe: no meaningful progress to report.
      ""
    } else {
      # Progress and the remaining count share one denominator (n_universe), so
      # the percentage reaches 100 only when nothing remains; floor() never
      # rounds up to 100 while work is queued. "processed" (not "complete")
      # keeps the completion wording in the bootstrap_complete branch only.
      n_remaining <- min(n_remaining, n_universe)
      pct <- floor(100 * (n_universe - n_remaining) / n_universe)
      sprintf(" Bootstrap %s%% processed (%s remaining).",
              format(pct, trim = TRUE), .fmt_n(n_remaining))
    }
  }

  new_word <- if (isTRUE(n_new == 1L)) "package" else "packages"
  pkg_word <- if (isTRUE(as.numeric(code_manifest$n_packages) == 1)) "package" else "packages"
  ver_word <- if (isTRUE(as.numeric(code_manifest$n_versions) == 1)) "version" else "versions"
  headline <- sprintf(
    "%s %s new to the catalog, %s updated. Now tracking %s %s across %s %s.%s",
    .fmt_n(n_new), new_word, .fmt_n(n_updated),
    .fmt_n(code_manifest$n_packages), pkg_word,
    .fmt_n(code_manifest$n_versions), ver_word,
    bootstrap_clause)

  # A package that failed to analyze is not in the table, not in the counts, and
  # until now not in the notes either: the only trace was a line in a CI log
  # nobody opens on a green run. Said here, with the shard it came from, so the
  # number is comparable run to run.
  n_fail <- suppressWarnings(as.numeric(.manifest_at(run_status, "shard_failures") %||% 0))
  fail_line <- if (length(n_fail) == 1L && !is.na(n_fail) && n_fail > 0) {
    sprintf(
      "%s of the %s packages in the most recent shard failed to analyze and are retried until %d consecutive failures retire them.",
      .fmt_n(n_fail), .fmt_n(.manifest_at(run_status, "n_shard")), MAX_CLONE_FAILURES)
  } else {
    character(0L)
  }

  c(headline, fail_line)
}

#' Build the "Updated this release" table's rows: one row per changed
#' package that has a row in the code DB, with its latest-version metrics,
#' what the release did to its API, and its dataset count.
#'
#' Row order is decided here, and it is not the alphabet. A release that
#' removed an export sorts first, because that is the change that can break a
#' package downstream; then by how many exports moved at all; then by code
#' size; then by name, so the order is stable between runs. The point is the
#' cap: when the table cannot show everything, what it drops should be the
#' quietest changes, not everything after the letter B. The bootstrap's
#' 33,282-package listing showed a11yShiny through ABHgenotypeR.
#'
#' @param code_con     Open DBI connection to the code database, or NULL.
#' @param data_con     Open DBI connection to the dataset database, or NULL.
#' @param changed_pkgs Character vector, this run's changed packages.
#' @param seed_pkgs    Character vector, the prior release's package set.
#' @return data.frame: package, version (tagged " (new)" or with its bump
#'   type), loc_r, functions, exports, api_added, api_removed, deps,
#'   datasets, most consequential first. Zero rows when there is nothing to
#'   show.
.build_package_rows <- function(code_con, data_con, changed_pkgs, seed_pkgs) {
  empty <- data.frame(package = character(0L), version = character(0L),
                      loc_r = numeric(0L), functions = numeric(0L),
                      exports = numeric(0L), api_added = numeric(0L),
                      api_removed = numeric(0L), deps = numeric(0L),
                      datasets = numeric(0L), stringsAsFactors = FALSE)
  if (is.null(code_con) || length(changed_pkgs) == 0L) return(empty)
  if (!"cran_code_summary" %in% DBI::dbListTables(code_con)) return(empty)

  # Name the columns instead of SELECT *: cran_code_summary is 233 columns
  # wide, and the notes read every version of every changed package, so a
  # catch-up run pulled hundreds of megabytes across to print forty rows.
  # Intersected with the live schema so an older database simply reports n/a
  # for what it never stored.
  wanted <- c("package", "version", "loc_r", "n_exports", "n_internal",
              "n_fns_r", "n_deps_direct", "depends", "imports",
              "latest_release_date", "bump_type", "exports_added_n",
              "exports_removed_n")
  present <- intersect(wanted, DBI::dbListFields(code_con, "cran_code_summary"))
  select  <- paste(c("rowid AS rowid_", sprintf('"%s"', present)), collapse = ", ")

  raw <- .fetch_by_package(code_con, "cran_code_summary", changed_pkgs, select = select)
  if (nrow(raw) == 0L) return(empty)

  latest    <- .pick_latest_rows(raw)
  m         <- .row_metrics(latest)
  ds_counts <- .count_datasets(data_con, latest$package)

  pkg    <- as.character(latest$package)
  is_new <- !(pkg %in% seed_pkgs)
  # "(new)" for a first appearance; otherwise the bump the maintainer declared,
  # which says whether this is a typo fix or a rewrite before any number does.
  tag <- ifelse(is_new, "new",
                ifelse(is.na(m$bump) | m$bump %in% c("", "initial"), NA_character_, m$bump))
  ver <- as.character(latest$version)
  ver <- ifelse(is.na(tag), ver, paste0(ver, " (", tag, ")"))

  out <- data.frame(package = pkg, version = ver,
                    loc_r = m$loc_r, functions = m$functions,
                    exports = m$exports,
                    api_added = m$api_added, api_removed = m$api_removed,
                    deps = m$deps,
                    datasets = as.numeric(unname(ds_counts[pkg])),
                    stringsAsFactors = FALSE)

  z       <- function(x) ifelse(is.na(x), 0, x)
  removed <- z(out$api_removed)
  moved   <- removed + z(out$api_added)
  out[order(-(removed > 0L), -moved, -z(out$loc_r), out$package), , drop = FALSE]
}

#' Format one package's API change for the table: exports gained and lost.
#'
#' @param added   Exports added this version (numeric, may be NA).
#' @param removed Exports removed this version (numeric, may be NA).
#' @return "n/a" when the database never recorded it, "-" when the API stood
#'   still, else "+12", "-3" or "+12/-3".
.fmt_api <- function(added, removed) {
  if (is.na(added) && is.na(removed)) return("n/a")
  a <- if (is.na(added)) 0 else added
  r <- if (is.na(removed)) 0 else removed
  if (a == 0 && r == 0) return("-")
  paste(c(if (a > 0) sprintf("+%s", .fmt_n(a)),
          if (r > 0) sprintf("-%s", .fmt_n(r))), collapse = "/")
}

#' Render the "## Updated this release" section: a markdown table bounded by
#' both a row cap and a byte budget, with an explicit count of what it left
#' out, or an honest "no changes" / empty-shell fallback.
#'
#' Nothing is dropped quietly. Rows the table did not print are counted in a
#' closing row, and changed packages with no row in the database at all are
#' counted in a sentence under it, because a number a reader cannot see is
#' worse than one they can.
#'
#' @param rows      data.frame from .build_package_rows(), most consequential
#'   first.
#' @param n_changed Total changed-package count for this run (from
#'   changed-packages.txt, independent of DB presence).
#' @param cap       Max rows to print before collapsing into a summary row.
#' @param budget    Bytes this section may occupy, including its closing
#'   lines. Rows are added only while they fit.
#' @return Character vector of markdown lines (no trailing blank line).
.build_table_section <- function(rows, n_changed, cap = NOTES_TABLE_MAX_ROWS,
                                 budget = Inf) {
  if (n_changed == 0L) {
    return(c("## Updated this release", "", "No package changes in this release."))
  }
  header <- c("| Package | Version | R LOC | Functions | Exports | API | Deps | Datasets |",
              "|---|---|--:|--:|--:|--:|--:|--:|")

  # Changed packages the code database has no row for: they were in
  # changed-packages.txt, so something did happen to them, and they are not in
  # the table. Say so rather than letting them evaporate between the two.
  n_missing <- max(0L, n_changed - nrow(rows))
  missing_lines <- if (n_missing > 0L) {
    c("", sprintf(
      "%s of the %s changed packages have no row in the code database and are not listed.",
      .fmt_n(n_missing), .fmt_n(n_changed)))
  } else {
    character(0L)
  }

  if (nrow(rows) == 0L) {
    # Every changed package was absent from the code DB (edge case): changes
    # did happen this run, so do not claim otherwise -- show the empty shell.
    return(c("## Updated this release", "", header, missing_lines))
  }

  measured_api <- any(!is.na(rows$api_added) | !is.na(rows$api_removed))
  caption <- if (nrow(rows) > 1L) {
    if (measured_api) {
      c("Most consequential first: releases that removed an export, then by how much of the API moved.", "")
    } else {
      c("Largest first by R code size; this database records no API history to rank by.", "")
    }
  } else {
    character(0L)
  }

  fixed <- c("## Updated this release", "", caption, header)
  # The closing row is written for the largest number it could carry, so the
  # space it needs is reserved before any row is admitted.
  omit_line <- function(n) sprintf("| ...and %s more changed packages | | | | | | | |", .fmt_n(n))
  reserved  <- .notes_bytes(c(omit_line(nrow(rows)), missing_lines))
  room      <- budget - .notes_bytes(fixed) - reserved

  candidates <- utils::head(rows, cap)
  body <- sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |",
                  candidates$package, candidates$version,
                  .vfmt_n(candidates$loc_r), .vfmt_n(candidates$functions),
                  .vfmt_n(candidates$exports),
                  mapply(.fmt_api, candidates$api_added, candidates$api_removed),
                  .vfmt_n(candidates$deps), .vfmt_n(candidates$datasets))

  # Admit rows only while the running total stays inside the budget. This is
  # what makes the bound a bound: names and versions vary in width, so a fixed
  # number of rows is not a number of bytes.
  fits  <- cumsum(nchar(body, type = "bytes") + 1L) <= room
  shown <- body[fits]
  n_omitted <- nrow(rows) - length(shown)
  if (n_omitted > 0L) shown <- c(shown, omit_line(n_omitted))

  c(fixed, shown, missing_lines)
}

#' .fmt_n over a vector.
#'
#' @param x Numeric-ish vector.
#' @return Character vector of the same length.
.vfmt_n <- function(x) vapply(x, .fmt_n, character(1L), USE.NAMES = FALSE)

#' Render the "## Catalog at a glance" section straight from the manifests
#' already read; nothing here is recomputed from the databases. The code and
#' data DB sizes are shown human-readable via format_bytes() (never as raw
#' byte counts).
#'
#' Every figure that the previous release also published is followed by the
#' change since then, so the notes say how far the catalog moved and not only
#' where it landed. The previous manifests are the ones the workflow already
#' downloads before the shard loop; no extra fetch, and no delta at all when
#' they are absent, which is the honest answer on a cold start.
#'
#' @param code_manifest Parsed code-manifest.json (list).
#' @param data_manifest Parsed data-manifest.json (list).
#' @param prev_code     Parsed prev-code-manifest.json (list), or NULL.
#' @param prev_data     Parsed prev-data-manifest.json (list), or NULL.
#' @param baseline      Tag the deltas are measured against, or NULL/"".
#' @return Character vector of markdown lines (no trailing blank line).
.build_catalog_section <- function(code_manifest, data_manifest,
                                   prev_code = NULL, prev_data = NULL,
                                   baseline = NULL) {
  f          <- .manifest_at(code_manifest, "tables", "cran_functions")
  median_loc <- .manifest_at(code_manifest, "stats", "loc_r_median")
  mean_loc   <- .manifest_at(code_manifest, "stats", "loc_r_mean")
  median_fns <- .manifest_at(code_manifest, "stats", "n_fns_r_median")
  # Count distinct datasets (the cran_datasets table), not dataset *versions*
  # (n_versions counts cran_dataset_versions). Fall back to n_versions only if
  # the table count is somehow absent.
  d      <- .manifest_at(data_manifest, "tables", "cran_datasets") %||% data_manifest$n_versions
  d_prev <- if (is.null(prev_data)) NULL else
    .manifest_at(prev_data, "tables", "cran_datasets") %||% prev_data$n_versions
  contents      <- .manifest_at(data_manifest, "tables", "cran_dataset_contents")
  median_rows   <- .manifest_at(data_manifest, "stats", "nrow_median")
  median_cols   <- .manifest_at(data_manifest, "stats", "ncol_median")

  catalog_line <- sprintf("- %s packages%s, %s versions%s, %s functions%s",
    .fmt_n(code_manifest$n_packages),
    .fmt_delta(code_manifest$n_packages, .manifest_at(prev_code, "n_packages")),
    .fmt_n(code_manifest$n_versions),
    .fmt_delta(code_manifest$n_versions, .manifest_at(prev_code, "n_versions")),
    .fmt_n(f),
    .fmt_delta(f, .manifest_at(prev_code, "tables", "cran_functions")))

  # Both halves of the shape of a typical package, from statistics the manifest
  # already computes every run and nothing has ever read.
  code_line <- if (is.null(median_fns)) {
    sprintf("- R code: median %s LOC per package", .fmt_n(median_loc))
  } else {
    sprintf("- R code: median %s LOC and %s functions per package",
            .fmt_n(median_loc), .fmt_n(median_fns))
  }
  if (!is.null(mean_loc)) {
    code_line <- sprintf("%s, mean %s LOC", code_line, .fmt_n(mean_loc))
  }

  # The dataset series ships its own database and used to get one line of the
  # notes. It has its own totals, its own movement and its own typical shape.
  data_line <- sprintf("- Datasets: %s%s across %s packages%s, %s dataset versions%s",
    .fmt_n(d), .fmt_delta(d, d_prev),
    .fmt_n(data_manifest$n_packages),
    .fmt_delta(data_manifest$n_packages, .manifest_at(prev_data, "n_packages")),
    .fmt_n(data_manifest$n_versions),
    .fmt_delta(data_manifest$n_versions, .manifest_at(prev_data, "n_versions")))

  shape_line <- if (is.null(median_rows) || is.null(median_cols)) {
    character(0L)
  } else if (is.null(contents)) {
    sprintf("- Typical dataset: %s rows by %s columns",
            .fmt_n(median_rows), .fmt_n(median_cols))
  } else {
    sprintf("- Typical dataset: %s rows by %s columns (median over %s measured)",
            .fmt_n(median_rows), .fmt_n(median_cols), .fmt_n(contents))
  }

  db_line <- sprintf(
    # The code and dataset databases ship as two separate releases, so state
    # both sizes and say so -- the same notes body is attached to each release.
    "- Databases: code metrics %s and dataset metrics %s (published as separate code and data releases)",
    format_bytes(code_manifest$db_bytes), format_bytes(data_manifest$db_bytes))

  baseline_line <- if (is.null(prev_code) && is.null(prev_data)) {
    character(0L)
  } else if (is.null(baseline) || !nzchar(baseline)) {
    "- Figures in parentheses are the change since the release this run started from."
  } else {
    sprintf("- Figures in parentheses are the change since %s, the release this run started from.",
            baseline)
  }

  c("## Catalog at a glance", "",
    catalog_line, code_line, data_line, shape_line, db_line, baseline_line)
}

#' Last-resort clamp: hand back at most `budget` bytes, and say when that
#' happened.
#'
#' The table is fitted to the budget before it is assembled, so this only
#' fires if the parts that are not the table overrun on their own. It drops
#' whole lines from the end rather than cutting one in half, and always leaves
#' a marker: a body that was shortened has to look shortened.
#'
#' @param lines  Character vector of markdown lines.
#' @param budget Maximum bytes, newlines included.
#' @return Character vector whose .notes_bytes() is <= budget.
.fit_notes <- function(lines, budget) {
  if (.notes_bytes(lines) <= budget) return(lines)
  marker <- "<sub>notes truncated to stay inside GitHub's release body limit</sub>"
  room   <- budget - .notes_bytes(marker)
  keep   <- if (room <= 0L) character(0L)
            else lines[cumsum(nchar(lines, type = "bytes") + 1L) <= room]
  out <- c(keep, marker)
  if (.notes_bytes(out) > budget) {
    # Not even the marker fits. Whatever is published then, it is not a body
    # that claims to be complete.
    out <- substr(marker, 1L, max(0L, budget - 1L))
  }
  out
}

#' Build the full release notes body: headline paragraph, per-package
#' metrics table, catalog summary, and a plumbing footer. No top-level "# "
#' heading is emitted -- the GitHub release title already carries that.
#'
#' The whole body is held to `budget` bytes. The sections that are not the
#' table are built first and measured, and the table is given what is left, so
#' the guarantee does not depend on how long package names happen to be.
#'
#' @param code_manifest Parsed code-manifest.json (list).
#' @param data_manifest Parsed data-manifest.json (list).
#' @param changed_pkgs  Character vector, this run's changed packages.
#' @param seed_pkgs     Character vector, the prior release's package set
#'   (empty when seed-packages.txt is absent/empty: every changed package
#'   counts as new).
#' @param code_con      Open DBI connection to the code database, or NULL.
#' @param data_con      Open DBI connection to the dataset database, or NULL.
#' @param cap           Max table rows before collapsing into a summary row.
#' @param prev_code_manifest Parsed prev-code-manifest.json (list), or NULL.
#' @param prev_data_manifest Parsed prev-data-manifest.json (list), or NULL.
#' @param run_status    Parsed run-status.json (list), or NULL.
#' @param baseline      Tag the deltas are measured against, or NULL.
#' @param budget        Maximum body size in bytes.
#' @return Character vector of markdown lines.
build_release_notes <- function(code_manifest, data_manifest, changed_pkgs,
                                seed_pkgs, code_con, data_con,
                                cap = NOTES_TABLE_MAX_ROWS,
                                prev_code_manifest = NULL,
                                prev_data_manifest = NULL,
                                run_status = NULL,
                                baseline = NULL,
                                budget = NOTES_BODY_MAX_BYTES) {
  headline        <- .build_headline(code_manifest, changed_pkgs, seed_pkgs, run_status)
  rows            <- .build_package_rows(code_con, data_con, changed_pkgs, seed_pkgs)
  catalog_section <- .build_catalog_section(code_manifest, data_manifest,
                                            prev_code_manifest, prev_data_manifest,
                                            baseline)

  short_fp <- substr(code_manifest$fingerprint %||% "", 1L, 8L)
  prev_fp  <- substr(.manifest_at(prev_code_manifest, "fingerprint") %||% "", 1L, 8L)
  footer   <- if (nzchar(prev_fp)) {
    sprintf("<sub>fingerprint %s (was %s) - full manifest in the release assets</sub>",
            short_fp, prev_fp)
  } else {
    sprintf("<sub>fingerprint %s - full manifest in the release assets</sub>", short_fp)
  }

  # Three blank lines separate the four blocks below; they cost a byte each.
  fixed_bytes <- .notes_bytes(headline) + .notes_bytes(catalog_section) +
    .notes_bytes(footer) + 3L
  table_section <- .build_table_section(rows, length(changed_pkgs), cap = cap,
                                        budget = budget - fixed_bytes)

  .fit_notes(c(headline, "", table_section, "", catalog_section, "", footer), budget)
}

# ---- metric coverage -------------------------------------------------------
#
# What fraction of the corpus each metric actually reports, recorded every run.
#
# What this catches, stated plainly, because it is narrower than it looks:
#
#   - a metric that is not computed for anybody, which means it stopped running
#   - a flag that is true for nobody in the whole corpus, which means it is
#     looking for something that is not there
#   - either of those appearing abruptly against the previous run
#
# What it does NOT catch, and this was measured rather than assumed: the
# has_vignettes bug that motivated it. That pattern matched .Rmd and .Rnw and
# never .qmd, so Quarto-only packages reported no vignettes while the .Rmd
# majority reported fine. Replaying it here, a run losing twenty packages to a
# growing format raises nothing, and it should not: a twenty-package move is
# indistinguishable from the ecosystem changing. Slow erosion in a subset is
# invisible to any threshold loose enough to be quiet on a normal week.
#
# Catching that shape needs a second, independent measurement of the same fact
# to disagree with the first. Both now exist for vignettes, one from the CRAN
# tarball and one from the repository tree, and comparing them is the follow-up
# this does not do.

#' Per-metric coverage over a summary frame.
#'
#' For each column: how many rows carry a value at all (`measured`), and for a
#' logical metric how many are TRUE (`positive`). A metric with a `measured` of
#' zero was never computed; one whose `positive` is zero across the whole corpus
#' is either genuinely unused or looking for the wrong thing, and the count
#' alone cannot tell those apart. That is the point: it says look here.
metric_coverage <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(data.frame(metric = character(), n_rows = integer(), measured = integer(),
                      positive = integer(), kind = character(),
                      stringsAsFactors = FALSE))
  }
  skip <- c("package", "version", "released", "ref")
  cols <- setdiff(names(summary_df), skip)
  n <- nrow(summary_df)
  rows <- lapply(cols, function(cn) {
    v <- summary_df[[cn]]
    measured <- sum(!is.na(v))
    is_flag <- is.logical(v) || (is.numeric(v) && all(v[!is.na(v)] %in% c(0, 1)))
    positive <- if (is_flag) sum(v[!is.na(v)] == 1 | v[!is.na(v)] == TRUE) else NA_integer_
    data.frame(metric = cn, n_rows = n, measured = as.integer(measured),
               positive = as.integer(positive),
               kind = if (is_flag) "flag" else "value", stringsAsFactors = FALSE)
  })
  # A frame carrying nothing but identifiers has no metrics to report, which is
  # a valid state (the bootstrap writes one) and not an error.
  if (!length(rows)) return(metric_coverage(NULL))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Metrics worth a second look, given this run's coverage and the last one's.
#'
#' Two questions, because the failures looked different. A metric that reports
#' nothing at all was never computed. A flag that is TRUE nowhere across tens of
#' thousands of packages is looking for something that is not there, which is
#' what a stale extension list looks like from the outside. A metric that fell
#' sharply against the previous run changed meaning without anyone saying so.
#'
#' `prior` may be NULL on a first run, in which case only the absolute checks
#' apply and the drop check is skipped rather than treated as a pass.
metric_coverage_alerts <- function(cov, prior = NULL, drop_tol = 0.5) {
  out <- character(0)
  if (is.null(cov) || nrow(cov) == 0) return("no metrics reported at all")

  never <- cov[cov$measured == 0L, , drop = FALSE]
  for (i in seq_len(nrow(never)))
    out <- c(out, sprintf("%s: not computed for any of %d packages",
                          never$metric[i], never$n_rows[i]))

  flags <- cov[cov$kind == "flag" & cov$measured > 0L &
               !is.na(cov$positive) & cov$positive == 0L, , drop = FALSE]
  for (i in seq_len(nrow(flags)))
    out <- c(out, sprintf("%s: measured on %d packages and true for none",
                          flags$metric[i], flags$measured[i]))

  if (!is.null(prior) && nrow(prior) > 0) {
    m <- merge(cov, prior, by = "metric", suffixes = c("", "_prev"))
    for (i in seq_len(nrow(m))) {
      a <- m$measured_prev[i]; b <- m$measured[i]
      if (!is.na(a) && a > 100L && b < a * drop_tol)
        out <- c(out, sprintf("%s: measured on %d packages, was %d", m$metric[i], b, a))
      pa <- m$positive_prev[i]; pb <- m$positive[i]
      if (!is.na(pa) && !is.na(pb) && pa > 100L && pb < pa * drop_tol)
        out <- c(out, sprintf("%s: true for %d packages, was %d", m$metric[i], pb, pa))
    }
  }
  out
}

# ---- dataset column coverage ----------------------------------------------
#
# The same question metric_coverage asks of cran_code_summary, asked of the
# three dataset tables. It went unasked for a year and the answer, when it was
# finally taken, was that a hundred of the content columns held nothing at all
# for any package in the archive: the reader had never emitted the field, or a
# generation bump had not been made and every widened row was being discarded
# on the way in. Either way the column shipped as public data and read as an
# honest NA, which is exactly what an empty column is not.
#
# Asked in SQL rather than by pulling the frame into R, because the contents
# table is the largest object the pipeline publishes and a shard has to be able
# to afford this every run. It costs one scan per table, which is the order of
# work the manifest's own fingerprint and statistics already spend on the same
# tables a few lines later.

#' Per-column coverage over the dataset tables.
#'
#' For every declared dataset column the table actually has: how many rows the
#' table holds, and how many of them carry a value at all. A column measured on
#' nobody is either a field the analyzer never emits or one whose writes are
#' being discarded, and the count alone cannot tell those apart. That is the
#' point: it says look here.
#'
#' @param con Connection to the dataset database.
#' @return data.frame(table, column, n_rows, measured); zero rows when none of
#'   the dataset tables exist yet.
dataset_column_coverage <- function(con) {
  empty <- data.frame(table = character(), column = character(),
                      n_rows = integer(), measured = integer(),
                      stringsAsFactors = FALSE)
  specs <- list(
    cran_dataset_contents = .DATASET_CONTENT_COLS,
    cran_dataset_versions = .DATASET_VERSION_COLS,
    cran_datasets         = .DATASET_IDENTITY_COLS)
  present <- DBI::dbListTables(con)
  out <- list()
  for (tbl in names(specs)) {
    if (!tbl %in% present) next
    cols <- intersect(names(specs[[tbl]]), DBI::dbListFields(con, tbl))
    if (!length(cols)) next
    # One scan per table: SQLite's COUNT(col) skips NULLs, so the coverage of
    # every column at once is a single aggregate query.
    sel <- paste(c('COUNT(*) AS "n_rows"',
                   sprintf('COUNT("%s") AS "c%d"', cols, seq_along(cols))),
                 collapse = ", ")
    got <- DBI::dbGetQuery(con, sprintf('SELECT %s FROM "%s"', sel, tbl))
    out[[tbl]] <- data.frame(
      table = tbl, column = cols,
      n_rows = as.integer(got$n_rows),
      measured = as.integer(unlist(got[sprintf("c%d", seq_along(cols))],
                                   use.names = FALSE)),
      stringsAsFactors = FALSE)
  }
  if (!length(out)) return(empty)
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' Dataset columns worth a second look, given this run's coverage.
#'
#' A column with no value in any row of a table that holds rows. An empty table
#' says nothing (a first shard has not written anything yet), so it raises
#' nothing: the alert is about a column the corpus had every chance to fill.
dataset_coverage_alerts <- function(cov) {
  if (is.null(cov) || nrow(cov) == 0L) return(character(0L))
  dead <- cov[cov$n_rows > 0L & cov$measured == 0L, , drop = FALSE]
  if (nrow(dead) == 0L) return(character(0L))
  sprintf("%s.%s: no value in any of %d %s",
          dead$table, dead$column, dead$n_rows,
          ifelse(dead$n_rows == 1L, "row", "rows"))
}
