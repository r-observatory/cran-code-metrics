# tests/testthat/test-release-text-run.R: the text history through run_update,
# with analyze_package replaced so no git or analyzer is needed.

.rtr_io <- function(pkgs = "pkgA") list(
  package_list = function() data.frame(package = pkgs, latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

# analyze_package for one version the analyzer read, text included unless with_text is FALSE.
.rtr_stub_analyze <- function(with_text = TRUE, frame = parent.frame()) {
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  withr::defer(assign("analyze_package", old, envir = env), envir = frame)
  assign("analyze_package", function(dest, pkg) {
    res <- list(
      summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
        latest_release_date = "2026-01-01", datasets_scanned = TRUE,
        detail_scanned = TRUE, analyzer_version = rpkg_analyzer_version(),
        stringsAsFactors = FALSE),
      api = data.frame(package = pkg, version = "1.0", exports_added = "[]",
        exports_removed = "[]", n_exports = 1L, stringsAsFactors = FALSE),
      churn = NULL, functions = NULL, edges = NULL, datasets = NULL,
      binary_versions = "1.0")
    if (with_text) {
      res$text <- .release_text_collect(list(.release_text_rows(
        pkg, "1.0", c(Package = pkg, Version = "1.0", RoxygenNote = "7.3.2"),
        list(package_version = "1.0", news_file = "NEWS.md",
             release_notes_source = "news_md", release_notes = "- first",
             release_notes_truncated = FALSE),
        rpkg_analyzer_version())), "1.0")
    }
    res
  }, envir = env)
}

.rtr_stub_bin <- function(version = "0.4.0-test", frame = parent.frame()) {
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(withr::local_tempdir(.local_envir = frame),
                                           version),
    PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = "",
    .local_envir = frame)
}

.rtr_query <- function(path, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

test_that("a shard writes the history to its own database and the latest rows beside the code", {
  .rtr_stub_bin()
  .rtr_stub_analyze()
  out <- withr::local_tempdir()
  run_update(.rtr_io(), out, shard_size = 10L)

  text_db <- file.path(out, RELEASE_TEXT_DB_FILENAME)
  expect_true(file.exists(text_db))
  expect_equal(.rtr_query(text_db, sprintf('SELECT COUNT(*) n FROM "%s"',
                                           DESCRIPTION_HISTORY_TABLE))$n, 3L)
  expect_equal(.rtr_query(text_db, sprintf('SELECT COUNT(*) n FROM "%s"',
                                           RELEASE_NOTES_HISTORY_TABLE))$n, 1L)
  code_db <- file.path(out, DB_FILENAME)
  expect_identical(.rtr_query(code_db, sprintf('SELECT field FROM "%s"',
                                               DESCRIPTION_FIELDS_TABLE))$field, "RoxygenNote")
  expect_false(DESCRIPTION_HISTORY_TABLE %in%
                 .rtr_query(code_db, "SELECT name FROM sqlite_master")$name)
})

test_that("a shard whose packages carry no text still runs and writes an empty history", {
  .rtr_stub_bin()
  .rtr_stub_analyze(with_text = FALSE)
  out <- withr::local_tempdir()
  expect_no_error(run_update(.rtr_io(), out, shard_size = 10L))
  expect_equal(.rtr_query(file.path(out, RELEASE_TEXT_DB_FILENAME), sprintf(
    'SELECT COUNT(*) n FROM "%s"', RELEASE_TEXT_VERSIONS_TABLE))$n, 0L)
})

test_that("a failed text write fails the shard before the code rows are written", {
  .rtr_stub_bin()
  .rtr_stub_analyze()
  env <- environment(run_update)
  old <- get("upsert_release_text", envir = env)
  assign("upsert_release_text", function(...) stop("disk full"), envir = env)
  on.exit(assign("upsert_release_text", old, envir = env), add = TRUE)

  out <- withr::local_tempdir()
  expect_error(run_update(.rtr_io(), out, shard_size = 10L), "disk full")
  tables <- .rtr_query(file.path(out, DB_FILENAME), "SELECT name FROM sqlite_master")$name
  expect_false("cran_code_summary" %in% tables)
})

test_that("a run re-reads the package whose 0.5.0 rows the history lacks", {
  .rtr_stub_bin()
  .rtr_stub_analyze()
  out <- withr::local_tempdir()
  con <- open_or_init_db(file.path(out, DB_FILENAME))
  upsert_shard(con, data.frame(package = "pkgA", version = "1.0", loc_r = 1L,
                               n_fns_r = 1L, analyzer_version = "0.5.0",
                               latest_release_date = "2026-01-01",
                               datasets_scanned = 1L, detail_scanned = 1L,
                               stringsAsFactors = FALSE),
               churn_df = .empty_churn(), api_df = .empty_api())
  DBI::dbDisconnect(con)

  expect_message(m <- run_update(.rtr_io(), out, shard_size = 10L),
                 "release text history lacks 1 analysed version")
  expect_identical(m$n_fresh, 1L)
  expect_equal(.rtr_query(file.path(out, RELEASE_TEXT_DB_FILENAME), sprintf(
    'SELECT COUNT(*) n FROM "%s"', RELEASE_TEXT_VERSIONS_TABLE))$n, 1L)
})

test_that("a gap under the running 0.5.0 build is re-read once and then stays closed", {
  # The stored row names the running build, so only the reconciliation can put
  # it back in the queue; a build change would re-read it anyway.
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(withr::local_tempdir(), "0.5.0-test",
                                           reads = "0.0.1", input_kind = ANALYZER_INPUT_KIND),
    PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = "")
  .rtr_stub_analyze()
  out <- withr::local_tempdir()
  con <- open_or_init_db(file.path(out, DB_FILENAME))
  upsert_shard(con, data.frame(package = "pkgA", version = "1.0", loc_r = 1L,
                               n_fns_r = 1L, analyzer_version = "0.5.0-test",
                               latest_release_date = "2026-01-01",
                               datasets_scanned = 1L, detail_scanned = 1L,
                               stringsAsFactors = FALSE),
               churn_df = .empty_churn(), api_df = .empty_api())
  DBI::dbDisconnect(con)

  expect_message(first <- suppressWarnings(run_update(.rtr_io(), out, shard_size = 10L)),
                 "release text history lacks 1 analysed version")
  expect_identical(first$n_fresh, 1L)
  expect_no_message(second <- suppressWarnings(run_update(.rtr_io(), out, shard_size = 10L)),
                    message = "release text history lacks")
  expect_identical(second$n_fresh, 0L)
})

test_that("the text manifest names the code database it was published beside", {
  .rtr_stub_bin()
  .rtr_stub_analyze()
  out <- withr::local_tempdir()
  run_update(.rtr_io(), out, shard_size = 10L)
  tm <- jsonlite::fromJSON(file.path(out, "text-manifest.json"))
  cm <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  expect_identical(tm$series, "text")
  expect_identical(tm$db_filename, RELEASE_TEXT_DB_FILENAME)
  expect_identical(tm$n_versions, 1L)
  expect_identical(tm$code_fingerprint, cm$fingerprint)
})

test_that("a shard with no text still publishes an empty text manifest", {
  .rtr_stub_bin()
  .rtr_stub_analyze(with_text = FALSE)
  out <- withr::local_tempdir()
  run_update(.rtr_io(), out, shard_size = 10L)
  expect_identical(jsonlite::fromJSON(file.path(out, "text-manifest.json"))$n_versions, 0L)
})

test_that("a run that would publish a smaller text history is refused", {
  .rtr_stub_bin()
  .rtr_stub_analyze()
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-text-manifest.json"), list(
    schema_version = 1L, series = "text", n_packages = 5L, n_versions = 50L,
    tables = list(cran_description_history = 900L, cran_release_notes_history = 40L)))
  expect_error(run_update(.rtr_io(), out, shard_size = 10L), "text n_packages")
})
