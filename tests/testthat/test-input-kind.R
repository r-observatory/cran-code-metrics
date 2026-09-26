# tests/testthat/test-input-kind.R: a 0.5.0 analyzer must show it reads the
# --input-kind flag before a run lets it near the catalog.

.ik_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

test_that("the self-check passes only when the analyzer names the kind it was given", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.5.0-test", reads = "0.0.1", input_kind = "release"))
  expect_true(rpkg_analyzer_selfcheck("release"))
  expect_false(rpkg_analyzer_selfcheck("git"))

  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.5.0-test", reads = "0.0.1"))
  expect_false(rpkg_analyzer_selfcheck("release"))
})

test_that("a 0.5.0 analyzer that does not name the kind stops the run before any shard", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.5.0-test", reads = "0.0.1"))
  out <- withr::local_tempdir()
  expect_error(run_update(.ik_io(), out, shard_size = 10L), "--input-kind release")
  expect_false(file.exists(file.path(out, DB_FILENAME)))
})

test_that("a 0.5.0 analyzer that names the kind is let through", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.5.0-test", reads = "0.0.1", input_kind = "release"),
    PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = "")
  out <- withr::local_tempdir()
  m <- suppressWarnings(run_update(.ik_io(), out, shard_size = 10L))
  expect_identical(m$n_shard, 1L)
})

test_that("a build before 0.5.0 is never asked for the self-check", {
  skip_on_os("windows")
  # This stub reads nothing, so a self-check would fail; reaching the shard proves it was skipped.
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.4.0-test"),
    PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = "")
  out <- withr::local_tempdir()
  m <- suppressWarnings(run_update(.ik_io(), out, shard_size = 10L))
  expect_identical(m$n_shard, 1L)
})

test_that("the analyzer CI installs answers the self-check package", {
  # Before 0.5.0 the flag is ignored and a summary still comes back; from 0.5.0 it
  # must name the kind, or every scheduled run stops before its first shard.
  skip_on_os("windows")
  skip_if(!nzchar(rpkg_analyzer_bin()), "needs rpkg-analyzer")
  dir <- withr::local_tempdir()
  writeLines(c("Package: selfcheck", "Version: 0.0.1"), file.path(dir, "DESCRIPTION"))
  expect_false(is.null(analyze_with_binary(dir)))
  if (analyzer_at_least(rpkg_analyzer_version(), "0.5.0")) {
    expect_true(rpkg_analyzer_selfcheck())
  }
})
