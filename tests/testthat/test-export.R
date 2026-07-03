# tests/testthat/test-export.R: tests for scripts/export.R

# ---------------------------------------------------------------------------
# Shared fixtures (rebuilt in each test for self-sufficiency)
# ---------------------------------------------------------------------------

.make_summary <- function() {
  data.frame(
    package        = c("pkgA", "pkgA", "pkgB"),
    version        = c("1.0",  "1.1",  "2.0"),
    date           = c("2024-01-01", "2024-06-01", "2024-03-01"),
    loc_r          = c(100L, 120L, 50L),
    has_tests      = c(TRUE, TRUE, FALSE),           # logical column
    lang_breakdown = c('{"R":100}', '{"R":120}', '{"R":50}'),  # JSON text
    stringsAsFactors = FALSE
  )
}

.make_churn <- function() {
  data.frame(
    package = c("pkgA",    "pkgA",       "pkgB"),
    version = c("1.1",     "1.1",        "2.0"),
    file    = c("R/foo.R", "data/x.rda", "R/bar.R"),
    added   = c(10L,       NA_integer_,  5L),   # NA for binary file
    deleted = c(2L,        NA_integer_,  1L),
    stringsAsFactors = FALSE
  )
}

.make_api <- function() {
  data.frame(
    package         = c("pkgA",    "pkgA",    "pkgB"),
    version         = c("1.0",     "1.1",     "2.0"),
    exports_added   = c('["foo"]', '["bar"]', '["baz"]'),
    exports_removed = c('[]',      '[]',      '[]'),
    n_exports       = c(1L, 2L, 1L),
    stringsAsFactors = FALSE
  )
}

.empty_churn <- function() {
  data.frame(package = character(0), version = character(0),
             file = character(0), added = integer(0), deleted = integer(0),
             stringsAsFactors = FALSE)
}

.empty_api <- function() {
  data.frame(package = character(0), version = character(0),
             exports_added = character(0), exports_removed = character(0),
             n_exports = integer(0), stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# export_metrics: basic correctness
# ---------------------------------------------------------------------------

test_that("export_metrics writes a valid SQLite file with three tables", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_metrics(tmp, .make_summary(), .make_churn(), .make_api())

  expect_true(file.exists(tmp))

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true("cran_code_summary" %in% tables)
  expect_true("cran_code_churn"   %in% tables)
  expect_true("cran_api_history"  %in% tables)
})

test_that("export_metrics row counts match input data.frames", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_metrics(tmp, .make_summary(), .make_churn(), .make_api())

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_code_summary")$n, 3)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_code_churn")$n, 3)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_api_history")$n, 3)
})

test_that("logical column is stored as 0/1 integer", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_metrics(tmp, .make_summary(), .make_churn(), .make_api())

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row_a <- DBI::dbGetQuery(con,
    "SELECT has_tests FROM cran_code_summary WHERE package='pkgA' AND version='1.0'")
  row_b <- DBI::dbGetQuery(con,
    "SELECT has_tests FROM cran_code_summary WHERE package='pkgB' AND version='2.0'")

  expect_equal(row_a$has_tests, 1L)
  expect_equal(row_b$has_tests, 0L)
})

test_that("JSON text column is preserved as-is", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_metrics(tmp, .make_summary(), .make_churn(), .make_api())

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- DBI::dbGetQuery(con,
    "SELECT lang_breakdown FROM cran_code_summary WHERE package='pkgA' AND version='1.0'")
  expect_equal(row$lang_breakdown, '{"R":100}')
})

test_that("NA values in churn added/deleted are preserved", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_metrics(tmp, .make_summary(), .make_churn(), .make_api())

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- DBI::dbGetQuery(con,
    "SELECT added, deleted FROM cran_code_churn WHERE file='data/x.rda'")
  expect_true(is.na(row$added))
  expect_true(is.na(row$deleted))
})

# ---------------------------------------------------------------------------
# export_metrics: indexes
# ---------------------------------------------------------------------------

test_that("indexes exist on all three tables", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_metrics(tmp, .make_summary(), .make_churn(), .make_api())

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  idx <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='index'")$name

  expect_true(any(grepl("summary", idx)))
  expect_true(any(grepl("churn",   idx)))
  expect_true(any(grepl("api",     idx)))
})

test_that("churn table has both (package,version) and (package) indexes", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_metrics(tmp, .make_summary(), .make_churn(), .make_api())

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  idx <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='cran_code_churn'")$name

  expect_gte(length(idx), 2L)
})

# ---------------------------------------------------------------------------
# export_metrics: empty data.frames
# ---------------------------------------------------------------------------

test_that("empty summary_df creates cran_code_summary with package and version columns", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  empty_summary <- data.frame(package = character(0), version = character(0),
                               stringsAsFactors = FALSE)
  export_metrics(tmp, empty_summary, .empty_churn(), .empty_api())

  expect_true(file.exists(tmp))

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_true("cran_code_summary" %in% DBI::dbListTables(con))
  cols <- DBI::dbListFields(con, "cran_code_summary")
  expect_true("package" %in% cols)
  expect_true("version" %in% cols)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_code_summary")$n, 0)
})

# ---------------------------------------------------------------------------
# export_metrics: overwrites existing file
# ---------------------------------------------------------------------------

test_that("export_metrics replaces an existing file cleanly", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  writeLines("not a sqlite file", tmp)  # corrupt placeholder

  summary_df <- data.frame(package = "pkgA", version = "1.0",
                            stringsAsFactors = FALSE)
  expect_no_error(
    export_metrics(tmp, summary_df, .empty_churn(), .empty_api()))

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("cran_code_summary" %in% DBI::dbListTables(con))
})

# ---------------------------------------------------------------------------
# write_manifest
# ---------------------------------------------------------------------------

test_that("write_manifest writes pretty-printed JSON that round-trips correctly", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  obj <- list(pipeline = "cran-code-metrics", n_packages = 42L,
              tags = c("a", "b"))
  write_manifest(tmp, obj)

  expect_true(file.exists(tmp))
  txt    <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  parsed <- jsonlite::fromJSON(txt)
  expect_equal(parsed$pipeline,   "cran-code-metrics")
  expect_equal(parsed$n_packages, 42L)
  # Pretty-printing: output contains newlines and indentation
  expect_true(grepl("\n", txt, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# metrics_fingerprint
# ---------------------------------------------------------------------------

test_that("metrics_fingerprint returns a 64-character lower-case hex string", {
  fp <- metrics_fingerprint(.make_summary())
  expect_type(fp, "character")
  expect_equal(nchar(fp), 64L)
  expect_true(grepl("^[0-9a-f]{64}$", fp))
})

test_that("metrics_fingerprint changes when a package gains a new version", {
  df1 <- data.frame(
    package = c("pkgA", "pkgB"),
    version = c("1.0",  "2.0"),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    package = c("pkgA", "pkgA", "pkgB"),
    version = c("1.0",  "1.1",  "2.0"),
    stringsAsFactors = FALSE
  )
  expect_false(metrics_fingerprint(df1) == metrics_fingerprint(df2))
})

test_that("metrics_fingerprint is stable for identical inputs", {
  df <- .make_summary()
  expect_equal(metrics_fingerprint(df), metrics_fingerprint(df))
})

test_that("metrics_fingerprint handles an empty summary_df", {
  empty <- data.frame(package = character(0), version = character(0),
                      stringsAsFactors = FALSE)
  fp <- metrics_fingerprint(empty)
  expect_type(fp, "character")
  expect_equal(nchar(fp), 64L)
})
