# tests/testthat/test-summary-schema.R: declared column types and retired
# columns on the summary table. Identical in both code-metrics pipelines.

# A shard whose every 0.5.0 column is NA, the shape that used to type text as INTEGER.
.ss_all_na_shard <- function(pkg = "pkgA", version = "1.0") {
  df <- data.frame(package = pkg, version = version, loc_r = 10L,
                   stringsAsFactors = FALSE)
  for (col in names(.SUMMARY_050_COLS)) df[[col]] <- NA
  df
}

.ss_types <- function(con) {
  info <- DBI::dbGetQuery(con, sprintf('PRAGMA table_info("%s")', SUMMARY_TABLE))
  stats::setNames(info$type, info$name)
}

.ss_upsert <- function(con, df, analyzer_version) {
  upsert_shard(con, df, churn_df = .empty_churn(), api_df = .empty_api(),
               analyzer_version = analyzer_version)
}

test_that("a fresh table takes the declared type for every 0.5.0 column", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, .ss_all_na_shard(), "0.5.0")
  types <- .ss_types(con)
  expect_identical(types[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
})

test_that("an existing table gains the declared types through the ALTER path", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, data.frame(package = "old", version = "0.9", loc_r = 1L,
                             stringsAsFactors = FALSE), "0.4.0")
  .ss_upsert(con, .ss_all_na_shard(), "0.5.0-test")
  types <- .ss_types(con)
  expect_identical(types[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
  expect_equal(DBI::dbGetQuery(con, sprintf(
    'SELECT COUNT(*) n FROM "%s"', SUMMARY_TABLE))$n, 2L)
})

test_that("the whole-database writer declares the same types", {
  path <- withr::local_tempfile(fileext = ".db")
  export_metrics(path, .ss_all_na_shard(), .empty_churn(), .empty_api(),
                 analyzer_version = "0.5.0")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.ss_types(con)[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
})

test_that("below 0.5.0 the frame decides the types, as it always has", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  df <- data.frame(package = "pkgA", version = "1.0", input_kind = NA,
                   stringsAsFactors = FALSE)
  .ss_upsert(con, df, "0.4.0")
  types <- .ss_types(con)
  expect_identical(types[["input_kind"]], "INTEGER")
  expect_false("news_file" %in% names(types))
})

# An analyzer that answers the self-check for this pipeline's input kind.
.ss_stub_bin <- function(dir, version) {
  stub <- file.path(dir, "stub-analyzer.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then',
    sprintf('  echo "rpkg-analyzer %s"', version),
    "  exit 0",
    "fi",
    sprintf('echo "{\\"rec\\":\\"summary\\",\\"input_kind\\":\\"%s\\"}"', ANALYZER_INPUT_KIND)),
    stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("a run under a 0.5.0 analyzer writes the declared types", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .ss_stub_bin(withr::local_tempdir(), "0.5.0-test"),
                      PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = "")
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)
  assign("analyze_package", function(dest, pkg) {
    s <- .ss_all_na_shard(pkg)
    s$analyzer_version <- "0.5.0-test"
    s$latest_release_date <- "2026-01-01"
    s$datasets_scanned <- TRUE
    s$detail_scanned <- TRUE
    list(summary = s,
         api = data.frame(package = pkg, version = "1.0", exports_added = "[]",
                          exports_removed = "[]", n_exports = 1L, stringsAsFactors = FALSE),
         churn = NULL, functions = NULL, edges = NULL, datasets = NULL,
         binary_versions = "1.0")
  }, envir = env)
  io <- list(
    package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

  out <- withr::local_tempdir()
  suppressWarnings(run_update(io, out, shard_size = 10L))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.ss_types(con)[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
})
