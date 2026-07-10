# tests/testthat/test-manifest.R
test_that("build_manifest reports measured counts, stats and fingerprint", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "cran_code_summary", data.frame(
    package = c("a","a","b"), version = c("1.0","1.1","2.0"),
    loc_r = c(10L, 20L, 30L), n_fns_r = c(1L, 2L, 3L), stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "cran_api_history", data.frame(
    package = "a", version = "1.0", stringsAsFactors = FALSE))

  m <- build_manifest(
    con, series = "code", repo = "r-observatory/cran-code-metrics",
    db_filename = "cran-code-metrics.db", db_bytes = 4096L,
    tables = c("cran_code_summary", "cran_api_history", "cran_functions"),
    fp_table = "cran_code_summary", fp_cols = c("package", "version"),
    pkg_table = "cran_code_summary", ver_table = "cran_code_summary",
    stat_table = "cran_code_summary", stat_cols = c("loc_r", "n_fns_r"),
    bootstrap = list(n_analyzed = 2L, n_universe = 5L, n_remaining = 3L,
                     bootstrap_complete = FALSE))

  expect_identical(m$schema_version, 1L)
  expect_identical(m$series, "code")
  expect_identical(m$n_packages, 2L)
  expect_identical(m$n_versions, 3L)
  expect_identical(m$tables[["cran_code_summary"]], 3L)
  expect_identical(m$tables[["cran_functions"]], 0L)      # absent table -> 0
  expect_equal(m$stats[["loc_r_mean"]], 20)
  expect_equal(m$stats[["loc_r_median"]], 20)
  expect_true(grepl("^[0-9a-f]{64}$", m$fingerprint))
  expect_true(is.null(m$bootstrap$n_universe) || m$bootstrap$n_universe == 5L)
})

test_that("build_manifest emits null stats for absent columns", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "cran_code_summary", data.frame(
    package = "a", version = "1.0", stringsAsFactors = FALSE))
  m <- build_manifest(
    con, series = "code", repo = "r", db_filename = "x.db", db_bytes = 1L,
    tables = "cran_code_summary", fp_table = "cran_code_summary",
    fp_cols = c("package","version"), pkg_table = "cran_code_summary",
    ver_table = "cran_code_summary", stat_table = "cran_code_summary",
    stat_cols = c("loc_r"), bootstrap = list(n_analyzed = 1L, n_universe = NULL,
      n_remaining = NULL, bootstrap_complete = FALSE))
  expect_null(m$stats[["loc_r_mean"]])
  expect_null(m$stats[["loc_r_median"]])
})
