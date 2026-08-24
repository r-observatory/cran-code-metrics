# tests/testthat/test-analyzer-read-attempts.R
#
# The third state of a dataset scan. datasets_scanned answers one question with
# two answers: the reader ran (whatever it found), or it did not. A package the
# analyzer was asked about and could not read is neither, and while it had no
# record of its own it stayed in the backfill queue for good: every run
# re-analysed it, every run reported a change, and the workflow published a
# dated release for a database that had not moved.
#
# The record is the same shape the pipeline already uses for a package that
# cannot be cloned: a count, a cap, and no place in the queue past it.

# A real git repo with one tag per version, so the real analyze_package runs
# over it and produces one summary row for each.
.dsa_clone_at <- function(versions) {
  function(pkg, dest) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
    system2("git", c("-C", dest, "config", "user.email", "t@example.com"),
            stdout = FALSE, stderr = FALSE)
    system2("git", c("-C", dest, "config", "user.name", "Test Bot"),
            stdout = FALSE, stderr = FALSE)
    writeLines("export(hello)", file.path(dest, "NAMESPACE"))
    dir.create(file.path(dest, "R"), showWarnings = FALSE)
    writeLines("hello <- function() 'hello'", file.path(dest, "R", "hello.R"))
    for (v in versions) {
      writeLines(c("Package: pkgA", paste0("Version: ", v), "Title: Test Package",
                   "Description: Minimal package for the dataset scan tests.",
                   "Author: Test Bot", "Maintainer: Test Bot <t@example.com>",
                   "License: MIT"), file.path(dest, "DESCRIPTION"))
      system2("git", c("-C", dest, "add", "-A"), stdout = FALSE, stderr = FALSE)
      system2("git", c("-C", dest, "commit", "-m", v), stdout = FALSE, stderr = FALSE)
      system2("git", c("-C", dest, "tag", v), stdout = FALSE, stderr = FALSE)
    }
    TRUE
  }
}

.dsa_io <- function(versions = "1.0") list(
  package_list = function() data.frame(package = "pkgA",
                                       latest_version = versions[[length(versions)]],
                                       stringsAsFactors = FALSE),
  clone = .dsa_clone_at(versions))

# Consecutive runs over a universe that does not change. The sequence of
# `changed` is what the workflow publishes on, so it is the thing under test.
.dsa_runs <- function(out, n = 4L, versions = "1.0") {
  io <- .dsa_io(versions)
  vapply(seq_len(n), function(i) isTRUE(suppressWarnings(
    run_update(io, out, shard_size = 10L))$changed), logical(1L))
}

.dsa_four_runs <- function(out) .dsa_runs(out, 4L)

# ---------------------------------------------------------------------------
# The record itself
# ---------------------------------------------------------------------------

test_that("an unread dataset scan is counted until the package stops being asked", {
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(.analyzer_read_exhausted(con), character(0L))
  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS - 1L)) {
    .record_analyzer_read_attempt(con, "pkgA", "1.0", "0.4.0-test")
    expect_equal(.analyzer_read_exhausted(con), character(0L))
  }
  .record_analyzer_read_attempt(con, "pkgA", "1.0", "0.4.0-test")
  expect_equal(.analyzer_read_exhausted(con), "pkgA")
  expect_identical(.n_datasets_unreadable(con), 1L)
})

test_that("a scan that reads the package forgets the attempts before it", {
  # Otherwise a package that failed once on a bad day carries that count for
  # the rest of the build's life and gives up sooner than it should.
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  .record_analyzer_read_attempt(con, "pkgA", "1.0", "0.4.0-test")
  .clear_analyzer_read_attempts(con, "pkgA")
  expect_identical(.n_datasets_unreadable(con), 0L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_analyzer_read_attempts")$n, 0L)
})

test_that("a new analyzer build asks a package it gave up on again", {
  # A count that cannot come down is a package retired for good on the say-so
  # of one build. The reader that could not read it is part of the record, so
  # the next reader starts from nothing.
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
    .record_analyzer_read_attempt(con, "pkgA", "1.0", "0.4.0-test")
  }
  expect_equal(.analyzer_read_exhausted(con), "pkgA")

  expect_equal(.forget_other_builds_read_attempts(con, "0.5.0-test"), 1L)
  expect_equal(.analyzer_read_exhausted(con), character(0L))
})

test_that("a run that cannot name its analyzer forgets nothing", {
  # The no-binary run records attempts with no build against them. Clearing
  # those on a run that also cannot name a build would reset the count every
  # time and the queue would never drain, which is the whole failure.
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
    .record_analyzer_read_attempt(con, "pkgA", "1.0", NA_character_)
  }
  expect_equal(.forget_other_builds_read_attempts(con, NA_character_), 0L)
  expect_equal(.analyzer_read_exhausted(con), "pkgA")
})

# ---------------------------------------------------------------------------
# One record per version, not one per package
# ---------------------------------------------------------------------------
# The queue that waits on n_fns_r reads every stored row, not just the latest,
# so an older version the analyzer cannot read holds the package in it. A
# record kept per package is cleared by the read that succeeded on the newest
# version, so nothing was ever counted against the one that failed.

test_that("the versions the analyzer did not produce are the ones counted", {
  df <- data.frame(package = "pkgA", version = c("1.0", "2.0"),
                   stringsAsFactors = FALSE)
  expect_equal(.analyzer_unread_versions(df, "2.0"), "1.0")
  expect_equal(.analyzer_unread_versions(df, c("1.0", "2.0")), character(0L))
  # A caller that cannot say which versions the analyzer produced answers for
  # none of them, the same way the build stamp refuses to.
  expect_equal(.analyzer_unread_versions(df, NULL), c("1.0", "2.0"))
  expect_equal(.analyzer_unread_versions(NULL, "1.0"), character(0L))
})

test_that("a version's count is its own, and one at the cap is enough", {
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
    .record_analyzer_read_attempt(con, "pkgA", "1.0", "0.4.0-test")
  }
  .record_analyzer_read_attempt(con, "pkgA", "2.0", "0.4.0-test")
  got <- DBI::dbGetQuery(con,
    "SELECT version, attempts FROM cran_analyzer_read_attempts ORDER BY version")
  expect_equal(got$version, c("1.0", "2.0"))
  expect_equal(got$attempts, c(MAX_ANALYZER_READ_ATTEMPTS, 1L))
  # The queues ask for packages, so one version with no way of being read is a
  # package with no way of leaving them.
  expect_equal(.analyzer_read_exhausted(con), "pkgA")
})

test_that("a version that was read stops being asked about and the rest do not", {
  # The read that succeeds is a read of that version. Clearing the package
  # would clear the record of the version that failed in the same breath.
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  .record_analyzer_read_attempt(con, "pkgA", "1.0", "0.4.0-test")
  .record_analyzer_read_attempt(con, "pkgA", "2.0", "0.4.0-test")
  .clear_analyzer_read_attempts(con, "pkgA", keep = "1.0")
  expect_equal(
    DBI::dbGetQuery(con,
      "SELECT version FROM cran_analyzer_read_attempts")$version, "1.0")
  # Nothing named keeps nothing: a package the analyzer read whole.
  .clear_analyzer_read_attempts(con, "pkgA")
  expect_identical(.n_datasets_unreadable(con), 0L)
})

test_that("a database holding the per-package record is given the per-version one", {
  # The published database carries this table, so the shape has to change under
  # a run that opens an older release rather than only in one built from
  # nothing.
  path <- withr::local_tempfile(fileext = ".db")
  con  <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE cran_analyzer_read_attempts (
      package          TEXT PRIMARY KEY,
      attempts         INTEGER NOT NULL DEFAULT 0,
      analyzer_version TEXT,
      last_attempt     TEXT)")
  DBI::dbExecute(con, "INSERT INTO cran_analyzer_read_attempts
      VALUES ('pkgA', 2, '0.4.0-test', '2026-01-01T00:00:00Z')")
  DBI::dbDisconnect(con)

  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("version" %in% DBI::dbListFields(con, "cran_analyzer_read_attempts"))
  # A count taken over a whole package belongs to no version of it, so it
  # cannot be carried onto one. pkgA is asked again, which costs the cap.
  expect_equal(.analyzer_read_exhausted(con), character(0L))
})

# ---------------------------------------------------------------------------
# End to end: the three states of a dataset scan, over four runs each
# ---------------------------------------------------------------------------

test_that("a package the analyzer cannot read leaves the queue instead of never settling", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  out <- withr::local_tempdir()
  changed <- .dsa_four_runs(out)

  # It may take the cap to get there; what it may not do is never get there.
  expect_true(changed[[1L]])
  expect_false(changed[[4L]])
  expect_false(changed[[length(changed)]])

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Still honestly unread. The package aged out of the queue; it was never
  # scanned, and nothing in the database says it was.
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1L]])))
  expect_identical(.n_datasets_unscanned(con), 1L)
  expect_identical(.n_datasets_unreadable(con), 1L)
})

test_that("the count of packages nobody could read reaches both manifests", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  out <- withr::local_tempdir()
  .dsa_four_runs(out)

  cm <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  dm <- jsonlite::fromJSON(file.path(out, "data-manifest.json"))
  # Unscanned says how many were never read. This says how many of those the
  # pipeline has stopped asking about, which is the number that does not come
  # down on its own.
  expect_identical(cm$bootstrap$n_datasets_unscanned, 1L)
  expect_identical(cm$bootstrap$n_datasets_unreadable, 1L)
  expect_identical(dm$bootstrap$n_datasets_unreadable, 1L)
})

test_that("a run with no analyzer at all settles too", {
  # Deliberate: production installs the binary, so this is a local run or a
  # degraded download. Converging late is better than a marker that claims a
  # scan nothing performed.
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = "")
  skip_if(nzchar(rpkg_analyzer_bin()), "rpkg-analyzer is on PATH")
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  out <- withr::local_tempdir()
  changed <- suppressWarnings(.dsa_four_runs(out))

  expect_true(changed[[1L]])
  expect_false(changed[[4L]])

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1L]])))
  expect_identical(.n_datasets_unreadable(con), 1L)
})

test_that("a package the reader did read keeps no attempt record and settles at once", {
  skip_on_os("windows")
  skip_if(!nzchar(rpkg_analyzer_bin()), "no rpkg-analyzer binary to read with")
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  out <- withr::local_tempdir()
  changed <- .dsa_four_runs(out)
  expect_equal(changed, c(TRUE, FALSE, FALSE, FALSE))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # The package ships no data. The reader ran and found none, which is a scan.
  expect_true(any(!is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM cran_code_summary")[[1L]])))
  expect_identical(.n_datasets_unreadable(con), 0L)
  expect_identical(.n_datasets_unscanned(con), 0L)
})

test_that("a package read at its newest version and not at an older one settles", {
  # The configuration the single-version cases above cannot reach: the reader
  # runs, the package is scanned, and one stored row still carries none of the
  # fields the n_fns_r queue waits on. The read of the newest version used to
  # clear the whole package's record, so the row that failed was never counted
  # and the package came back every run for good.
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN =
    .stub_analyzer_bin(stub_dir, "0.4.0-test", reads = "2.0"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  out     <- withr::local_tempdir()
  changed <- .dsa_runs(out, 6L, versions = c("1.0", "2.0"))

  expect_true(changed[[1L]])
  expect_false(changed[[length(changed)]])

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT version, n_fns_r, datasets_scanned FROM cran_code_summary
      ORDER BY version")
  expect_equal(rows$version, c("1.0", "2.0"))
  # The newest version was read: the package is scanned, and honestly so.
  expect_true(is.na(rows$n_fns_r[[1L]]))
  expect_false(is.na(rows$n_fns_r[[2L]]))
  expect_equal(rows$datasets_scanned[[2L]], 1L)

  # And the count sits on the version that was never read, not on the package.
  att <- DBI::dbGetQuery(con,
    "SELECT version, attempts FROM cran_analyzer_read_attempts")
  expect_equal(att$version, "1.0")
  expect_gte(att$attempts[[1L]], MAX_ANALYZER_READ_ATTEMPTS)

  # The queues have given the package up, and its datasets are still there:
  # they come from the newest version, and the newest version was read. What
  # went unread is an older version's metrics, which is a different gap and
  # not the one the dataset figures count.
  expect_equal(.analyzer_read_exhausted(con), "pkgA")
  expect_identical(.n_datasets_unscanned(con), 0L)
  expect_identical(.n_datasets_unreadable(con), 0L)
})

test_that("a package given up on is asked again by the next analyzer build", {
  # The cap is a verdict about one reader. A count that outlived its reader
  # would retire a package for good on the say-so of a build nobody runs any
  # more, and the row it protects carries no scan marker, so the marker-based
  # invalidation cannot reach it either.
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test"))
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))

  out <- withr::local_tempdir()
  io  <- .dsa_io()
  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
    suppressWarnings(run_update(io, out, shard_size = 10L))
  }
  settled <- suppressWarnings(run_update(io, out, shard_size = 10L))
  expect_false(settled$changed)

  # A new build arrives. Same package, same failure, but nothing here has been
  # asked of this reader yet.
  .stub_analyzer_bin(stub_dir, "0.5.0-test")
  retried <- suppressWarnings(run_update(io, out, shard_size = 10L))
  expect_equal(retried$n_fresh, 1L)
  expect_true(retried$changed)
})
