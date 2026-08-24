# tests/testthat/test-dataset-coverage.R: the coverage canary over the dataset
# tables.
#
# metric_coverage asks of cran_code_summary how much of the corpus each metric
# actually reports. The dataset tables were never asked, and the answer turned
# out to be that a hundred of their columns hold nothing at all for anybody.
# These tests cover the same question asked of them, and the one number the
# manifest carries so the answer survives the run that produced it.

# One per-version dataset row, the way analyze.R hands it to the writer. Kept
# here rather than shared with test-datasets.R because each test file is
# sourced into an environment of its own.
.mk_cov_row <- function(package = "p", version = "1.0", content_fp = "C1") {
  data.frame(
    package = package, version = version,
    is_current = 1L, fp_algo_version = 3L,
    name = "d", file = "data/d.rda", internal = 0L,
    format = "rda", format_version = 2L, compression = "gzip",
    class = "data.frame", kind = "data.frame", nrow = 3L, ncol = 2L,
    length = NA_integer_, n_cols = 2L, n_missing_total = 0L,
    schema_fp = "S1", shape_fp = "SH", content_fp = content_fp,
    s4_package = NA_character_, confidence = "exact", notes = NA_character_,
    columns = '[{"name":"a","type":"integer"}]', row_sketch = '["0001","0002"]',
    stringsAsFactors = FALSE
  )
}

.cov_con <- function(rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  if (!is.null(rows)) {
    DBI::dbWithTransaction(con, .write_datasets_normalized(con, rows, unique(rows$package)))
  } else {
    .ensure_dataset_tables(con)
  }
  con
}

test_that("dataset_column_coverage counts, per declared column, the rows that carry a value", {
  row <- .mk_cov_row()
  row$mean <- 2.5
  con <- .cov_con(row)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cov <- dataset_column_coverage(con)
  expect_s3_class(cov, "data.frame")
  expect_identical(names(cov), c("table", "column", "n_rows", "measured"))

  pick <- function(tbl, col) cov[cov$table == tbl & cov$column == col, ]
  # nrow is written for this row, mean is written for this row, and the ~140
  # other content columns are not. class rides the version link, because the
  # digests the content row is keyed by do not cover the class chain.
  expect_equal(pick("cran_dataset_contents", "nrow")$measured, 1L)
  expect_equal(pick("cran_dataset_contents", "mean")$measured, 1L)
  expect_equal(pick("cran_dataset_contents", "density")$measured, 0L)
  expect_equal(pick("cran_dataset_contents", "nrow")$n_rows, 1L)
  expect_equal(pick("cran_dataset_versions", "class")$measured, 1L)

  # All three dataset tables are covered, not just the wide one.
  expect_setequal(unique(cov$table),
                  c("cran_dataset_contents", "cran_dataset_versions", "cran_datasets"))
})

test_that("dataset_column_coverage reports nothing for tables that are not there", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cov <- dataset_column_coverage(con)
  expect_equal(nrow(cov), 0L)
  expect_identical(names(cov), c("table", "column", "n_rows", "measured"))
})

test_that("dataset_coverage_alerts names a column that is empty for the whole corpus", {
  row <- .mk_cov_row()
  con <- .cov_con(row)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  alerts <- dataset_coverage_alerts(dataset_column_coverage(con))
  expect_true(any(grepl("cran_dataset_contents.density", alerts, fixed = TRUE)))
  # A column this row does fill is not an alert.
  expect_false(any(grepl("cran_dataset_contents.class", alerts, fixed = TRUE)))
  # And the message says how much of the corpus it looked at, because a column
  # empty across one row means nothing and across a million means a great deal.
  expect_true(any(grepl("1 row", alerts)))
})

test_that("dataset_coverage_alerts stays quiet on an empty table", {
  con <- .cov_con(NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Nothing has been written, so every column is empty and none of that is a
  # finding. A first shard must not open with 148 alerts.
  expect_identical(dataset_coverage_alerts(dataset_column_coverage(con)), character(0L))
})

test_that("build_manifest carries the count of columns nobody fills", {
  row <- .mk_cov_row()
  con <- .cov_con(row)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cov <- dataset_column_coverage(con)
  m <- build_manifest(
    con, series = "data", repo = "r/x", db_filename = "d.db", db_bytes = 1,
    tables = c("cran_datasets"), fp_table = "cran_datasets",
    fp_cols = c("package", "name"), pkg_table = "cran_datasets",
    ver_table = "cran_dataset_versions", stat_table = "cran_dataset_contents",
    stat_cols = c("nrow"),
    bootstrap = list(n_analyzed = 1L, n_universe = 1L, n_remaining = 0L,
                     bootstrap_complete = TRUE),
    coverage = cov)

  expect_true(m$coverage$n_all_null > 0L)
  expect_equal(m$coverage$n_columns, nrow(cov))
  expect_true("cran_dataset_contents.density" %in% m$coverage$all_null)
  # The list of names is capped: the count is the number that matters and the
  # names are there to start the search, not to be the search.
  expect_lte(length(m$coverage$all_null), 20L)
})

test_that("build_manifest leaves the coverage block out when nobody measured it", {
  con <- .cov_con(.mk_cov_row())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  m <- build_manifest(
    con, series = "code", repo = "r/x", db_filename = "d.db", db_bytes = 1,
    tables = c("cran_datasets"), fp_table = "cran_datasets",
    fp_cols = c("package", "name"), pkg_table = "cran_datasets",
    ver_table = "cran_dataset_versions", stat_table = "cran_dataset_contents",
    stat_cols = c("nrow"),
    bootstrap = list(n_analyzed = 1L, n_universe = 1L, n_remaining = 0L,
                     bootstrap_complete = TRUE))
  expect_null(m$coverage)
})
