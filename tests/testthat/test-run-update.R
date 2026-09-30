# tests/testthat/test-run-update.R: tests for scripts/update.R
#
# All tests are fully offline; they inject a fake io whose clone() builds a
# real minimal git repo in a temp directory, allowing the real analyze_package
# to run on it.
#
# Source order expected (from the test validation command):
#   config.R -> git.R -> context.R -> metrics/*.R -> analyze.R -> export.R -> update.R

# ---------------------------------------------------------------------------
# Shared helpers (not test_that blocks)
# ---------------------------------------------------------------------------

# Create a minimal real git repo at dest for analyze_package to work on.
# Each element of `versions` becomes a commit + lightweight tag.
# DESCRIPTION Version: is bumped on each iteration so there is always
# a change to commit.
.make_fake_clone <- function(pkg, dest, versions = "1.0") {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "config", "user.email", "test@example.com"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "config", "user.name", "Test Bot"),
          stdout = FALSE, stderr = FALSE)

  for (ver in versions) {
    writeLines(c(
      paste("Package:", pkg),
      paste("Version:", ver),
      "Title: Test Package",
      "Description: Minimal test package for run_update tests.",
      "Author: Test Bot",
      "Maintainer: Test Bot <test@example.com>",
      "License: MIT"
    ), file.path(dest, "DESCRIPTION"))

    writeLines("export(hello)", file.path(dest, "NAMESPACE"))

    r_dir <- file.path(dest, "R")
    dir.create(r_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(
      c(paste0("## Version ", ver, " of ", pkg),
        "hello <- function() 'hello'"),
      file.path(r_dir, "hello.R")
    )

    system2("git", c("-C", dest, "add", "-A"),    stdout = FALSE, stderr = FALSE)
    # Commit message uses the version string only (no space) because system2
    # joins args with spaces and runs via shell, which would split a message
    # like "version 1.0" into two shell words.
    system2("git", c("-C", dest, "commit", "-m", ver),
            stdout = FALSE, stderr = FALSE)
    system2("git", c("-C", dest, "tag", ver),     stdout = FALSE, stderr = FALSE)
  }
  TRUE
}

# Build a fake io from a data.frame(package, latest_version).
# fail_clones: character vector of package names whose clone() returns FALSE.
# version_map: named list pkg -> character vector of version tags; if absent,
#   clone() uses the single latest_version from pkg_df.
.fake_io <- function(pkg_df, fail_clones = character(0L),
                     version_map = NULL) {
  list(
    package_list = function() pkg_df,
    clone = function(pkg, dest) {
      if (pkg %in% fail_clones) return(FALSE)
      vers <- if (!is.null(version_map) && pkg %in% names(version_map)) {
        version_map[[pkg]]
      } else {
        v <- pkg_df$latest_version[pkg_df$package == pkg]
        if (length(v) == 0L || is.na(v)) "1.0" else as.character(v)
      }
      .make_fake_clone(pkg, dest, versions = vers)
    }
  )
}

# Override WORK_DIR for the duration of a test.
# Returns a list that .restore_work_dir() can consume.
.override_work_dir <- function() {
  orig <- WORK_DIR
  tmp  <- tempfile("ccm_work_")
  dir.create(tmp, recursive = TRUE)
  WORK_DIR <<- tmp
  list(orig = orig, tmp = tmp)
}

.restore_work_dir <- function(state) {
  WORK_DIR <<- state$orig
  unlink(state$tmp, recursive = TRUE, force = TRUE)
}

# ---------------------------------------------------------------------------
# Test 1: sharded bootstrap -- three runs exhaust a 5-package universe
# ---------------------------------------------------------------------------


#' Skip when the analyzer binary is absent.
#'
#' These two tests assert that a shard ADVANCES between runs. Advancing depends
#' on the per-package detail sentinel being written, and only the analyzer
#' binary writes it, so without the binary the same shard is selected forever
#' and the assertions fail with count mismatches that say nothing about the
#' cause. Only a linux-x86_64 build is published, so this skips on macOS rather
#' than reporting twelve mysterious failures to anyone developing there.
skip_without_analyzer <- function() {
  if (!nzchar(rpkg_analyzer_bin()))
    testthat::skip("needs rpkg-analyzer: without it the backfill pool never drains")
}

test_that("sharded bootstrap: three runs cover universe of 5, fourth is no-op", {
  skip_without_analyzer()
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkgs   <- c("pkgA", "pkgB", "pkgC", "pkgD", "pkgE")
  pkg_df <- data.frame(package = pkgs, latest_version = rep("1.0", 5L),
                       stringsAsFactors = FALSE)
  io <- .fake_io(pkg_df)

  # ---- Run 1 ---------------------------------------------------------------
  m1 <- run_update(io, out_dir, shard_size = 2L)

  expect_equal(m1$n_shard,    2L)
  expect_equal(m1$n_universe, 5L)
  expect_false(m1$bootstrap_complete)
  expect_true(m1$changed)
  expect_equal(m1$shard_failures$count, 0L)

  con  <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs1 <- sort(DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM cran_code_summary")$package)
  DBI::dbDisconnect(con)

  expect_equal(length(pkgs1), 2L)
  expect_equal(pkgs1, c("pkgA", "pkgB"))  # deterministic alphabetical shard

  # code-manifest.json and data-manifest.json written to out_dir
  expect_true(file.exists(file.path(out_dir, "code-manifest.json")))
  expect_true(file.exists(file.path(out_dir, "data-manifest.json")))

  # ---- Run 2 ---------------------------------------------------------------
  m2 <- run_update(io, out_dir, shard_size = 2L)

  expect_equal(m2$n_shard, 2L)
  expect_false(m2$bootstrap_complete)
  expect_equal(m2$n_analyzed, 4L)

  con  <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs2 <- sort(DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM cran_code_summary")$package)
  DBI::dbDisconnect(con)

  expect_equal(length(pkgs2), 4L)
  expect_true(all(c("pkgA", "pkgB") %in% pkgs2))  # carry-forward preserved
  expect_true(all(c("pkgC", "pkgD") %in% pkgs2))  # new this shard

  # ---- Run 3 ---------------------------------------------------------------
  m3 <- run_update(io, out_dir, shard_size = 2L)

  expect_equal(m3$n_shard, 1L)          # only pkgE remains
  expect_true(m3$bootstrap_complete)
  expect_equal(m3$n_analyzed, 5L)

  con  <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs3 <- sort(DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM cran_code_summary")$package)
  DBI::dbDisconnect(con)

  expect_equal(pkgs3, sort(pkgs))

  # ---- Run 4: no-op --------------------------------------------------------
  m4 <- run_update(io, out_dir, shard_size = 2L)

  expect_false(m4$changed)
  expect_equal(m4$n_shard,    0L)
  expect_equal(m4$n_analyzed, 5L)
  expect_true(m4$bootstrap_complete)
})

# ---------------------------------------------------------------------------
# Test 2: version change triggers re-analysis; old rows are replaced
# ---------------------------------------------------------------------------

test_that("package with new version is re-analyzed and carry-forward rows replaced", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  # Initial universe: pkgA at 1.0 (single version in repo).
  pkg_df_v1 <- data.frame(package = "pkgA", latest_version = "1.0",
                           stringsAsFactors = FALSE)
  io_v1 <- .fake_io(pkg_df_v1, version_map = list(pkgA = "1.0"))

  run_update(io_v1, out_dir, shard_size = 10L)

  con    <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  rows_v1 <- DBI::dbGetQuery(
    con, "SELECT version FROM cran_code_summary WHERE package='pkgA'")
  DBI::dbDisconnect(con)

  expect_equal(nrow(rows_v1), 1L)
  expect_equal(rows_v1$version, "1.0")

  # Universe updated: pkgA now at 1.1; repo has both 1.0 and 1.1.
  pkg_df_v2 <- data.frame(package = "pkgA", latest_version = "1.1",
                           stringsAsFactors = FALSE)
  io_v2 <- .fake_io(pkg_df_v2, version_map = list(pkgA = c("1.0", "1.1")))

  m2 <- run_update(io_v2, out_dir, shard_size = 10L)

  expect_true(m2$changed)
  expect_equal(m2$n_shard, 1L)

  con    <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  rows_v2 <- DBI::dbGetQuery(
    con,
    "SELECT version FROM cran_code_summary WHERE package='pkgA' ORDER BY version")
  DBI::dbDisconnect(con)

  # Fresh analysis replaces old row(s); now both versions are present.
  expect_equal(nrow(rows_v2), 2L)
  expect_true("1.0" %in% rows_v2$version)
  expect_true("1.1" %in% rows_v2$version)
})

# ---------------------------------------------------------------------------
# Test 3: clone failure is recorded and does not abort the shard
# ---------------------------------------------------------------------------

test_that("clone failure is recorded in shard_failures and does not abort", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(
    package        = c("pkgFail", "pkgOk"),
    latest_version = c("1.0",     "1.0"),
    stringsAsFactors = FALSE
  )
  io <- .fake_io(pkg_df, fail_clones = "pkgFail")

  m <- run_update(io, out_dir, shard_size = 10L)

  # Failure is recorded in the manifest.
  expect_equal(m$shard_failures$count,    1L)
  expect_true("pkgFail" %in% m$shard_failures$packages)

  # Run did not abort: pkgOk must be in the DB.
  con     <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs_db <- DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM cran_code_summary")$package
  DBI::dbDisconnect(con)

  expect_true("pkgOk"   %in% pkgs_db)
  expect_false("pkgFail" %in% pkgs_db)
})

# ---------------------------------------------------------------------------
# Test 4: force_full re-analyzes packages already in the DB
# ---------------------------------------------------------------------------

test_that("force_full re-analyzes packages already in DB within shard_size limit", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(
    package        = c("pkgA", "pkgB", "pkgC"),
    latest_version = c("1.0",  "1.0",  "1.0"),
    stringsAsFactors = FALSE
  )
  io <- .fake_io(pkg_df)

  # Full bootstrap first.
  run_update(io, out_dir, shard_size = 10L)

  # A no-op run would set changed=FALSE; force_full=TRUE overrides that.
  m <- run_update(io, out_dir, shard_size = 2L, force_full = TRUE)

  expect_true(m$changed)
  expect_equal(m$n_shard, 2L)   # shard_size still caps the run
  # One package (pkgC) remains after this shard.
  expect_false(m$bootstrap_complete)
  expect_equal(m$shard_failures$count, 0L)
})

# ---------------------------------------------------------------------------
# Test 5: package reaching MAX_CLONE_FAILURES is excluded and counted
# ---------------------------------------------------------------------------

test_that("package hitting MAX_CLONE_FAILURES is excluded from todo and counted in permanent_failures", {
  skip_without_analyzer()
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(
    package        = c("pkgFail", "pkgOk"),
    latest_version = c("1.0",     "1.0"),
    stringsAsFactors = FALSE
  )
  io_mixed <- .fake_io(pkg_df, fail_clones = "pkgFail")

  # Run MAX_CLONE_FAILURES times; pkgFail accumulates consecutive_failures.
  for (i in seq_len(MAX_CLONE_FAILURES)) {
    run_update(io_mixed, out_dir, shard_size = 10L)
  }

  # At this point pkgFail has exactly MAX_CLONE_FAILURES consecutive failures.
  # Next run should exclude pkgFail from the to-do list entirely.
  m_final <- run_update(io_mixed, out_dir, shard_size = 10L)

  expect_equal(m_final$permanent_failures, 1L)
  # pkgFail excluded; pkgOk already analyzed, so nothing to do this run.
  expect_equal(m_final$n_shard, 0L)
  expect_equal(m_final$shard_failures$count, 0L)
})

test_that("the MAX_CLONE_FAILURES path parks the same way when Actions sets GITHUB_RUN_ID", {
  # Actions sets GITHUB_RUN_ID in the unit-test step too; a run id read from it
  # would skip pkgFail on calls 2 to 5 in CI only.
  withr::local_envvar(c(GITHUB_RUN_ID = "ci"))
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.4.0-test", reads = "1.0"))
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  pkg_df <- data.frame(package = c("pkgFail", "pkgOk"), latest_version = c("1.0", "1.0"),
                       stringsAsFactors = FALSE)
  io_mixed <- .fake_io(pkg_df, fail_clones = "pkgFail")
  for (i in seq_len(MAX_CLONE_FAILURES)) run_update(io_mixed, out_dir, shard_size = 10L)
  m_final <- run_update(io_mixed, out_dir, shard_size = 10L)
  expect_equal(m_final$permanent_failures, 1L)
  expect_equal(m_final$n_shard, 0L)
  expect_equal(m_final$shard_failures$count, 0L)
})

# ---------------------------------------------------------------------------
# Test 6: transient failure followed by success resets the failure counter
# ---------------------------------------------------------------------------

test_that("transient failure that later succeeds resets the failure counter", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(package = "pkgFlaky", latest_version = "1.0",
                       stringsAsFactors = FALSE)

  # Run 1: clone fails -> consecutive_failures = 1.
  io_fail <- .fake_io(pkg_df, fail_clones = "pkgFlaky")
  run_update(io_fail, out_dir, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  cf1 <- DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM cran_metrics_failures WHERE package = 'pkgFlaky'")
  DBI::dbDisconnect(con)
  expect_equal(cf1$consecutive_failures, 1L)

  # Run 2: clone succeeds -> failure record deleted, package appears in DB.
  io_ok <- .fake_io(pkg_df)
  m2 <- run_update(io_ok, out_dir, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  cf2 <- DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM cran_metrics_failures WHERE package = 'pkgFlaky'")
  pkgs_db <- DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM cran_code_summary")$package
  DBI::dbDisconnect(con)

  expect_equal(nrow(cf2), 0L)               # failure row deleted on success
  expect_true("pkgFlaky" %in% pkgs_db)      # package now in DB
  expect_equal(m2$permanent_failures, 0L)   # no permanent failures
})

# ---------------------------------------------------------------------------
# The line a worker prints, and the reason it used to swallow
# ---------------------------------------------------------------------------

test_that(".worker_line carries the reason a package failed", {
  line <- .worker_line(3L, 400L, FALSE, "pkgX", "analyze", 0L, 12.34,
                       "cannot open file 'DESCRIPTION'")
  expect_true(grepl("[3/400] FAIL pkgX: analyze after 12.3s", line, fixed = TRUE))
  expect_true(grepl("cannot open file 'DESCRIPTION'", line, fixed = TRUE))
  # One write, one line: two would let another fork's output land between them.
  expect_identical(nchar(gsub("[^\n]", "", line)), 1L)
})

test_that(".worker_line says nothing extra when there is no reason", {
  ok <- .worker_line(25L, 400L, TRUE, "pkgY", "ok", 7L, 1.5)
  expect_identical(ok, "[25/400] ok pkgY: 7 versions in 1.5s\n")
})

test_that(".worker_line keeps one fork's line inside one pipe write", {
  # Forks share fd 1. A write that fits in the pipe buffer arrives whole, so
  # lines are reordered but never spliced; a longer one can be split down the
  # middle and interleaved with another package's. An R condition message
  # carries whatever the failure quoted, including a whole file.
  reason <- paste(rep("a deparsed call that went on and on", 200L), collapse = "\n")
  line <- .worker_line(1L, 1L, FALSE, "pkgZ", "analyze", 0L, 0.5, reason)
  expect_lte(nchar(line, type = "bytes"), WORKER_LINE_MAX_BYTES)
  expect_identical(nchar(gsub("[^\n]", "", line)), 1L)
  expect_true(grepl("a deparsed call", line, fixed = TRUE))
})

test_that(".worker_line does not cut a multi-byte character in half", {
  # Clipping by bytes on a message that is not ASCII, which a maintainer name
  # or a file path routinely is not.
  line <- .worker_line(1L, 1L, FALSE, "pkgZ", "analyze", 0L, 0.5,
                       strrep("é中文", 500L))
  expect_lte(nchar(line, type = "bytes"), WORKER_LINE_MAX_BYTES)
  expect_true(validUTF8(line))
})

test_that("a package that failed to analyze says why in the run output", {
  # The reason went to warning() inside an mclapply fork, where nothing
  # collects it and the fork's exit discards it. Every failure in every run
  # was therefore a package name and no cause.
  out_dir <- tempfile(); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  # One core so the worker runs in this process and its output is capturable;
  # the emit is identical either way.
  orig_cores <- ANALYSIS_CORES
  ANALYSIS_CORES <<- 1L
  on.exit(ANALYSIS_CORES <<- orig_cores, add = TRUE)

  old <- analyze_package
  assign("analyze_package",
         function(dest, pkg) stop("no tags on this clone"),
         envir = environment(run_update))
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  io <- .fake_io(data.frame(package = "pkgBoom", latest_version = "1.0",
                            stringsAsFactors = FALSE))
  logged <- capture.output(suppressWarnings(run_update(io, out_dir, shard_size = 1L)))

  expect_true(any(grepl("FAIL pkgBoom", logged, fixed = TRUE)))
  expect_true(any(grepl("no tags on this clone", logged, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# The dataset marker is only earned by a run that actually read datasets
# ---------------------------------------------------------------------------
# Dataset rows come from the analyzer binary alone; analyze_package falls back
# to the pure-R analyze_version() whenever analyze_with_binary() returns NULL.
# Both tests stub that one call rather than depending on whether this machine
# has the binary, so the assertion means the same thing on a laptop and in CI.

.with_binary_returning <- function(value, expr) {
  env <- environment(analyze_package)
  old <- get("analyze_with_binary", envir = env)
  assign("analyze_with_binary", function(dir, ...) value, envir = env)
  on.exit(assign("analyze_with_binary", old, envir = env), add = TRUE)
  force(expr)
}

test_that("a package analyzed without the dataset reader is not marked scanned", {
  repo <- tempfile("ccm_ds_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgNoReader", repo, versions = c("1.0", "1.1"))

  res <- .with_binary_returning(NULL, analyze_package(repo, "pkgNoReader"))

  expect_equal(nrow(res$datasets), 0L)
  expect_true(all(is.na(res$summary$datasets_scanned)))
})

test_that("a package the reader looked at is marked scanned even with nothing to report", {
  repo <- tempfile("ccm_ds_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgReader", repo, versions = c("1.0", "1.1"))

  # A package that ships no data still gets a scan: the reader ran and found
  # nothing, which is a different fact from never having looked.
  metrics <- structure(list(loc_r = 1L),
                       functions = .empty_functions_df()[, -(1:2), drop = FALSE],
                       edges     = .empty_edges_df()[, -(1:2), drop = FALSE],
                       datasets  = .datasets_frame(list()))
  res <- .with_binary_returning(metrics, analyze_package(repo, "pkgReader"))

  last <- nrow(res$summary)
  expect_equal(nrow(res$datasets), 0L)
  expect_true(isTRUE(res$summary$datasets_scanned[last]))
  expect_true(all(is.na(res$summary$datasets_scanned[-last])))
})

# ---------------------------------------------------------------------------
# A DESCRIPTION that is not UTF-8
# ---------------------------------------------------------------------------

test_that("a package with a latin1 DESCRIPTION is analysed and gets its dependency columns", {
  withr::local_locale(c(LC_CTYPE = "C.UTF-8"))
  out_dir <- tempfile(); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  latin1_desc <- c(
    charToRaw("Package: pkgLatin\nVersion: 1.0\nTitle: Latin One\nAuthor: S"),
    as.raw(0xf8), charToRaw("ren H"), as.raw(0xf8),
    charToRaw("jsgaard\nMaintainer: S"), as.raw(0xf8),
    charToRaw("ren <s@example.com>\nImports: stats, utils\nLicense: GPL-2\nEncoding: latin1\n"))
  io <- list(
    package_list = function() data.frame(package = "pkgLatin", latest_version = "1.0",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) {
      .make_fake_clone(pkg, dest, versions = "1.0")
      writeBin(latin1_desc, file.path(dest, "DESCRIPTION"))
      system2("git", c("-C", dest, "commit", "-q", "-a", "-m", "1.0"),
              stdout = FALSE, stderr = FALSE)
      system2("git", c("-C", dest, "tag", "-f", "1.0"), stdout = FALSE, stderr = FALSE)
      TRUE
    })

  m <- suppressWarnings(run_update(io, out_dir, shard_size = 10L))

  expect_equal(m$shard_failures$count, 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(con,
    "SELECT imports FROM cran_code_summary WHERE package = 'pkgLatin'")
  expect_identical(row$imports, "stats, utils")
})

# ---------------------------------------------------------------------------
# A version that cannot be extracted fails the package and changes nothing
# ---------------------------------------------------------------------------

test_that("analyze_package fails as a whole when one version cannot be extracted", {
  repo <- tempfile("ccm_xf_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgX", repo, versions = c("1.0", "1.1"))
  real <- extract_version
  .local_global("extract_version", function(repo, ref, dest) {
    if (identical(ref, "1.1")) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
    real(repo, ref, dest)
  })
  err <- tryCatch(analyze_package(repo, "pkgX"), error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$ref, "1.1")
})

test_that("a cap during extraction extracts again into an emptied directory", {
  repo <- tempfile("ccm_xc_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgX", repo, versions = c("1.0", "1.1"))
  .local_global("analyze_with_binary", function(dir, kind = ANALYZER_INPUT_KIND, memo = NULL) NULL)
  want <- suppressWarnings(analyze_package(repo, "pkgX"))

  real  <- extract_version
  fired <- FALSE
  .local_global("extract_version", function(repo, ref, dest) {
    if (!fired && identical(ref, "1.1")) {
      fired <<- TRUE
      real(repo, ref, dest)
      writeLines("leftover <- function() 1", file.path(dest, "R", "leftover.R"))
      stop(.cap_error())
    }
    real(repo, ref, dest)
  })
  got <- suppressWarnings(analyze_package(repo, "pkgX"))
  expect_true(fired)
  expect_identical(got, want)
})

test_that("a version that cannot be extracted leaves the package's stored rows as they were", {
  out_dir <- tempfile(); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  orig_cores <- ANALYSIS_CORES
  ANALYSIS_CORES <<- 1L
  on.exit(ANALYSIS_CORES <<- orig_cores, add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.4.0-test", reads = c("1.0", "1.1")))

  v1 <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  run_update(.fake_io(v1, version_map = list(pkgA = "1.0")), out_dir, shard_size = 10L)
  before <- .package_rows(out_dir, "pkgA")
  expect_gt(nrow(before[[paste(DB_FILENAME, SUMMARY_TABLE)]]), 0L)

  real <- extract_version
  .local_global("extract_version", function(repo, ref, dest) {
    if (identical(ref, "1.1")) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
    real(repo, ref, dest)
  })
  v2 <- data.frame(package = "pkgA", latest_version = "1.1", stringsAsFactors = FALSE)
  logged <- capture.output(
    m <- run_update(.fake_io(v2, version_map = list(pkgA = c("1.0", "1.1"))),
                    out_dir, shard_size = 10L))

  expect_identical(m$shard_failures$packages, "pkgA")
  expect_true(any(grepl("FAIL pkgA: extract after", logged, fixed = TRUE)))
  expect_true(any(grepl("git archive of 1.1 exited 128", logged, fixed = TRUE)))
  expect_identical(.package_rows(out_dir, "pkgA"), before)
})

# ---------------------------------------------------------------------------
# An analyzer line that does not parse fails the package and changes nothing
# ---------------------------------------------------------------------------

# A stub analyzer that reads every version and, on `bad_version`, also prints
# a line that is not JSON.
.stub_bad_line_bin <- function(dir, bad_version) {
  stub <- file.path(dir, "stub-bad-line.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then echo "rpkg-analyzer 0.4.0-test"; exit 0; fi',
    'dir=$(echo "$1" | tr -d "\'")',
    'v=$(sed -n "s/^Version: *//p" "$dir/DESCRIPTION" | head -1)',
    'echo "{\\"rec\\":\\"summary\\",\\"loc_r\\":1,\\"n_fns_r\\":1}"',
    sprintf('if [ "$v" = "%s" ]; then echo "{\\"rec\\":\\"function\\",\\"name\\":"; fi', bad_version),
    "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("analyze_package fails with analyzer_parse_incomplete on a line that does not parse", {
  skip_on_os("windows")
  repo <- tempfile("ccm_pi_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgP", repo, versions = c("1.0", "1.1"))
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_bad_line_bin(withr::local_tempdir(), "1.1"))
  err <- tryCatch(analyze_package(repo, "pkgP"), error = function(e) e)
  expect_s3_class(err, "analyzer_parse_incomplete")
  expect_identical(err$n_bad, 1L)
})

test_that("an analyzer line that does not parse leaves stored rows and read attempts as they were", {
  skip_on_os("windows")
  out_dir <- tempfile(); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_bad_line_bin(withr::local_tempdir(), "1.1"))

  v1 <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  run_update(.fake_io(v1, version_map = list(pkgA = "1.0")), out_dir, shard_size = 10L)
  before <- .package_rows(out_dir, "pkgA")
  expect_gt(nrow(before[[paste(DB_FILENAME, SUMMARY_TABLE)]]), 0L)

  v2 <- data.frame(package = "pkgA", latest_version = "1.1", stringsAsFactors = FALSE)
  m <- run_update(.fake_io(v2, version_map = list(pkgA = c("1.0", "1.1"))),
                  out_dir, shard_size = 10L)

  expect_identical(m$shard_failures$packages, "pkgA")
  expect_identical(.package_rows(out_dir, "pkgA"), before)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(nrow(DBI::dbGetQuery(con,
    "SELECT * FROM cran_analyzer_read_attempts WHERE package = 'pkgA'")), 0L)
})

# ---------------------------------------------------------------------------
# A cap swallowed in analyze_package's per-version steps is evaluated again
# ---------------------------------------------------------------------------

test_that("a cap in a per-version step of analyze_package changes nothing it returns", {
  repo <- tempfile("ccm_capsteps_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgCap", repo, versions = c("1.0", "1.1"))
  dir.create(file.path(repo, "vignettes"))
  writeLines(c("---", "title: Intro", "vignette: >",
               "  %\\VignetteEngine{knitr::rmarkdown}", "---", "Text."),
             file.path(repo, "vignettes", "intro.Rmd"))
  writeLines('old <- function() .Deprecated("hello")', file.path(repo, "R", "old.R"))
  system2("git", c("-C", repo, "add", "-A"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "extras"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "tag", "-f", "1.1"), stdout = FALSE, stderr = FALSE)

  metrics <- structure(list(loc_r = 1L),
                       functions = .empty_functions_df()[, -(1:2), drop = FALSE],
                       edges     = .empty_edges_df()[, -(1:2), drop = FALSE],
                       datasets  = .datasets_frame(list()))
  .local_global("analyze_with_binary", function(dir, kind = ANALYZER_INPUT_KIND, memo = NULL) metrics)
  # read_at is the clock at each reading, so it differs between any two runs.
  analyse <- function() {
    res <- analyze_package(repo, "pkgCap")
    res$text$versions$read_at <- NULL
    res
  }
  want <- analyse()
  expect_gt(nrow(want$vignettes), 0L)

  at_1.1 <- function(ctx) identical(ctx$version, "1.1")
  .local_global("metrics_vignettes", .fires_cap_once(metrics_vignettes, when = at_1.1))
  .local_global("deprecation_signals", .fires_cap_once(deprecation_signals, when = at_1.1))
  .local_global("parse_namespace", .fires_cap_once(parse_namespace))
  expect_identical(analyse(), want)
})

# ---------------------------------------------------------------------------
# A git killed at GIT_TIMEOUT is a timeout, not a fetch failure
# ---------------------------------------------------------------------------

# Put a git in front of PATH that exits 124 on `archive`, as system2 reports a
# kill at GIT_TIMEOUT, and runs the real git for everything else.
.local_git_archive_124 <- function(frame = parent.frame()) {
  dir  <- withr::local_tempdir(.local_envir = frame)
  real <- Sys.which("git")
  writeLines(c("#!/bin/sh",
               'for a in "$@"; do [ "$a" = archive ] && exit 124; done',
               sprintf('exec %s "$@"', shQuote(real))), file.path(dir, "git"))
  Sys.chmod(file.path(dir, "git"), mode = "0755")
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
                      .local_envir = frame)
}

test_that("an archive killed at GIT_TIMEOUT parks as a timeout and is released by a new build", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(stub_dir, "0.4.0-test",
                                                             reads = "1.0"))
  .local_git_archive_124()
  io <- .fake_io(data.frame(package = "pkgT", latest_version = "1.0",
                            stringsAsFactors = FALSE))
  failures <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, "SELECT stage, fetch_failures, timeout_failures
                            FROM cran_metrics_failures WHERE package = 'pkgT'")
  }

  for (i in seq_len(MAX_TIMEOUT_FAILURES)) run_update(io, out_dir, shard_size = 10L)
  expect_identical(failures(), data.frame(stage = "git_timeout", fetch_failures = 0L,
                                          timeout_failures = MAX_TIMEOUT_FAILURES,
                                          stringsAsFactors = FALSE))
  expect_identical(run_update(io, out_dir, shard_size = 10L)$n_shard, 0L)

  .stub_analyzer_bin(stub_dir, "0.4.1-test", reads = "1.0")
  expect_identical(run_update(io, out_dir, shard_size = 10L)$n_shard, 1L)
  expect_identical(failures()$timeout_failures, 1L)
})

# ---------------------------------------------------------------------------
# The shard loop ends although a package failed this run
# ---------------------------------------------------------------------------

# The workflow's loop: run_update as the shard, then shard_loop_done read
# through bash, as update.yml runs them. Returns each shard's run status.
.run_shard_loop <- function(io, out_dir, max_shards = 10L) {
  script <- normalizePath(test_path("..", "..", "scripts", "publish.sh"))
  status_path <- file.path(out_dir, "run-status.json")
  statuses <- list()
  for (i in seq_len(max_shards)) {
    run_update(io, out_dir, shard_size = 2L)
    statuses[[i]] <- jsonlite::read_json(status_path)
    rc <- system2("bash", c("-c", shQuote(sprintf("source %s && shard_loop_done %s",
                                                  shQuote(script), shQuote(status_path)))),
                  stdout = FALSE, stderr = FALSE)
    if (identical(rc, 0L)) break
  }
  statuses
}

test_that("the shard loop stops at the shard that drains its queue and publishes no empty shard", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("jq")), "jq is not installed")
  withr::local_envvar(c(PIPELINE_RUN_ID = "r1", PREV_CODE_TAG = "", PREV_DATA_TAG = "",
                        PREV_TEXT_TAG = ""))
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.4.0-test", reads = "1.0"))
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  pkgs <- c("pkgA", "pkgB", "pkgC", "pkgD", "pkgE", "pkgF")
  io <- .fake_io(data.frame(package = pkgs, latest_version = rep("1.0", 6L),
                            stringsAsFactors = FALSE), fail_clones = "pkgC")
  # A baseline whose fingerprint the first shard moves, so every later shard of
  # the run reads as changed against it.
  write_manifest(file.path(out_dir, "prev-code-manifest.json"),
                 list(schema_version = 1L, series = "code", fingerprint = strrep("0", 64L)))

  statuses <- .run_shard_loop(io, out_dir)

  expect_lte(length(statuses), 3L)
  published <- Filter(function(s) isTRUE(s$changed), statuses)
  expect_true(all(vapply(published, function(s) s$n_shard > 0L, logical(1L))))
  expect_false(statuses[[length(statuses)]]$bootstrap_complete)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM cran_metrics_failures WHERE package = 'pkgC'")[[1L]], 1L)
})
