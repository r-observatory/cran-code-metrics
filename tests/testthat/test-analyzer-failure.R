# tests/testthat/test-analyzer-failure.R: what an analyzer that was killed, or
# that exited non-zero, does to the version it was reading and to the rows the
# package already has.

# A stub analyzer answering --version with `version`. On a package version named
# in `first` it runs that shell line before printing anything; on one named in
# `endings` it prints its summary and then runs that line; on any other it
# prints a summary and a dataset record and exits 0. With `stats` a complete run
# appends a line to RPKG_ANALYZER_STATS first, as 0.5.1 does, and a line in
# `first` or `endings` can do the same by calling the shell function stats.
.af_stub <- function(dir, version, endings = character(0L), stats = FALSE,
                     first = character(0L)) {
  stub <- file.path(dir, "stub-analyzer.sh")
  kind <- if (analyzer_at_least(version, "0.5.0")) ',"input_kind":"release"' else ""
  arms <- function(lines) {
    if (length(lines)) c('case "$v" in', sprintf("  %s) %s ;;", names(lines), lines), "esac")
  }
  writeLines(c(
    "#!/bin/sh",
    sprintf('if [ "$1" = "--version" ]; then echo "rpkg-analyzer %s"; exit 0; fi', version),
    if (stats) {
      sprintf(paste0("stats() { if [ -n \"${RPKG_ANALYZER_STATS:-}\" ]; then ",
                     "echo '{\"build\":\"%s\",\"ms\":1}' >> \"$RPKG_ANALYZER_STATS\"; fi; }"),
              version)
    } else {
      "stats() { :; }"
    },
    'v=$(sed -n "s/^Version: *//p" "$1/DESCRIPTION" | head -1)',
    arms(first),
    sprintf("echo '{\"rec\":\"summary\",\"loc_r\":1,\"n_fns_r\":1%s}'", kind),
    arms(endings),
    paste0("echo '{\"rec\":\"dataset\",\"name\":\"d\",\"file\":\"data/d.rda\",",
           "\"class\":\"data.frame\",\"kind\":\"table\",\"confidence\":\"exact\",",
           "\"content_fp\":\"cf\",\"schema_fp\":\"sf\"}'"),
    "stats",
    "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# An extracted package tree at `version`, as analyze_with_binary is handed one.
.af_tree <- function(version, frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  writeLines(c("Package: pkgA", paste("Version:", version)), file.path(dir, "DESCRIPTION"))
  dir
}

# A real git repository with one tag per version, for the real analyze_package.
.af_clone <- function(pkg, dest, versions) {
  git <- function(...) {
    system2("git", c("-C", dest, "-c", "user.name=Test", "-c", "user.email=t@example.com", ...),
            stdout = FALSE, stderr = FALSE)
  }
  dir.create(file.path(dest, "R"), recursive = TRUE, showWarnings = FALSE)
  system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
  writeLines("export(hello)", file.path(dest, "NAMESPACE"))
  writeLines("hello <- function() 'hello'", file.path(dest, "R", "hello.R"))
  for (v in versions) {
    writeLines(c(paste("Package:", pkg), paste("Version:", v), "Title: Test Package",
                 "Description: Minimal package for the analyzer failure tests.",
                 "Author: Test Bot", "Maintainer: Test Bot <t@example.com>",
                 "License: MIT"), file.path(dest, "DESCRIPTION"))
    git("add", "-A")
    git("commit", "-m", v)
    git("tag", v)
  }
  TRUE
}

.af_io <- function(versions, pkgs = "pkgA") list(
  package_list = function() data.frame(package = pkgs,
                                       latest_version = versions[[length(versions)]],
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) .af_clone(pkg, dest, versions))

# One run_update over `versions`, on one core so the worker runs in this
# process. Returns the manifest, with the lines the run printed as $logged.
.af_run <- function(out, versions, pkgs = "pkgA") {
  .local_global("WORK_DIR", withr::local_tempdir())
  .local_global("ANALYSIS_CORES", 1L)
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = ""))
  m <- NULL
  logged <- capture.output(
    m <- suppressWarnings(run_update(.af_io(versions, pkgs), out, shard_size = 10L)))
  c(m, list(logged = logged))
}

.af_query <- function(out, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql)
}

# The stored summary rows: version, the build that wrote each (NA for the R
# fallback) and n_fns_r, which only the analyzer fills.
.af_summary <- function(out) {
  has <- .af_query(out, "SELECT name FROM sqlite_master WHERE type = 'table'")$name
  df  <- if (SUMMARY_TABLE %in% has) {
    .af_query(out, sprintf('SELECT * FROM "%s" ORDER BY version', SUMMARY_TABLE))
  } else {
    data.frame(version = character(0L), stringsAsFactors = FALSE)
  }
  col <- function(name) if (is.null(df[[name]])) rep(NA, nrow(df)) else df[[name]]
  data.frame(version = as.character(df$version),
             analyzer_version = as.character(col("analyzer_version")),
             n_fns_r = as.integer(col("n_fns_r")), stringsAsFactors = FALSE)
}

.af_verdicts <- function(out) {
  .af_query(out, "SELECT package, stage, analyze_failures, timeout_failures, reason
                    FROM cran_metrics_failures ORDER BY package")
}

# How each stub ends on the version it fails, the build it reports, whether
# that build writes a statistics line, and the exits the worker's line names.
# 0.4.0 writes no statistics line, as the pinned build.
.AF_CASES <- list(
  abort         = list(ending = "exit 134", build = "0.4.0-test", stats = FALSE,
                       exits = " \\[analyzer exit 134 x1\\]",
                       reason = "analyzer exited with status 134"),
  killed        = list(ending = "exit 137", build = "0.4.0-test", stats = FALSE,
                       exits = " \\[analyzer exit 137 x1\\]",
                       reason = "analyzer exited with status 137"),
  failed        = list(ending = "exit 101", build = "0.4.0-test", stats = FALSE,
                       exits = " \\[analyzer exit 101 x1\\]",
                       reason = "analyzer exited with status 101 on a version with analyzer rows"),
  no_statistics = list(ending = "exit 0", build = "0.5.1-test", stats = TRUE, exits = "",
                       reason = "analyzer exited 0 without its statistics line"))

# How a stub gives no usable result without being killed: the shell line it
# runs before printing anything, and what the failure then says. Each line
# calls stats first, so a build that writes a statistics line has written it.
.AF_UNUSABLE <- list(
  not_found  = list(first = "stats; /nonexistent/rpkg-analyzer; exit $?",
                    reason = "analyzer could not be run (error in running command)"),
  no_output  = list(first = "stats; exit 0",
                    reason = "analyzer exited 0 with no output"),
  no_summary = list(first = "stats; echo '{\"rec\":\"dcf\",\"Package\":\"pkgA\"}'; exit 0",
                    reason = "analyzer exited 0 with no summary record"))

# What every analyzer_failed message ends with.
.AF_PROTECTED <- " on a version with analyzer rows"

# `code`, run with no analyzer binary to be found.
.af_without_binary <- function(code) {
  .local_global("rpkg_analyzer_bin", function() "")
  force(code)
}

# ---------------------------------------------------------------------------
# What analyze_with_binary makes of an exit status
# ---------------------------------------------------------------------------

test_that("an analyzer ended by a signal raises analyzer_killed, on a protected version or not", {
  skip_on_os("windows")
  tree <- .af_tree("1.0")
  endings <- c("exit 134" = 134L, "exit 137" = 137L, "kill -9 $$" = 137L,
               "exit 143" = 143L, "kill -15 $$" = 143L, "exit 128" = 128L)
  for (ending in names(endings)) {
    withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
      withr::local_tempdir(), "0.4.0-test", endings = c("1.0" = ending)))
    for (protect in c(TRUE, FALSE)) {
      err <- tryCatch(analyze_with_binary(tree, protect = protect), error = function(e) e)
      info <- sprintf("%s, protect = %s", ending, protect)
      expect_s3_class(err, c("analyzer_killed", "error", "condition"), exact = TRUE)
      expect_identical(err$status, endings[[ending]], info = info)
      expect_identical(conditionMessage(err),
                       sprintf("analyzer exited with status %d", endings[[ending]]), info = info)
    }
  }
})

test_that("any other non-zero exit raises analyzer_failed on a protected version and gives the R fallback on another", {
  skip_on_os("windows")
  tree <- .af_tree("1.0")
  for (status in c(1L, 2L, 101L, 126L)) {
    withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
      withr::local_tempdir(), "0.4.0-test", endings = c("1.0" = sprintf("exit %d", status))))
    expect_null(analyze_with_binary(tree), info = status)
    expect_null(analyze_with_binary(tree, protect = FALSE), info = status)
    err <- tryCatch(analyze_with_binary(tree, protect = TRUE), error = function(e) e)
    expect_s3_class(err, c("analyzer_failed", "error", "condition"), exact = TRUE)
    expect_identical(err$status, status)
    expect_identical(conditionMessage(err), sprintf(
      "analyzer exited with status %d on a version with analyzer rows", status))
  }
})

test_that("an analyzer that gives no usable result raises analyzer_failed on a protected version and gives the R fallback on another", {
  skip_on_os("windows")
  tree <- .af_tree("1.0")
  dir  <- withr::local_tempdir()
  expect_unusable <- function(reason, info) {
    expect_null(analyze_with_binary(tree), info = info)
    expect_null(analyze_with_binary(tree, protect = FALSE), info = info)
    err <- tryCatch(analyze_with_binary(tree, protect = TRUE), error = function(e) e)
    expect_s3_class(err, c("analyzer_failed", "error", "condition"), exact = TRUE)
    expect_identical(conditionMessage(err), paste0(reason, .AF_PROTECTED), info = info)
  }
  # With a statistics line written, and from a build that writes none.
  stats <- file.path(withr::local_tempdir(), "stats.ndjson")
  for (build in list(list("0.4.0-test", FALSE), list("0.5.1-test", TRUE))) {
    for (case in names(.AF_UNUSABLE)) {
      k <- .AF_UNUSABLE[[case]]
      withr::local_envvar(RPKG_ANALYZER_STATS = stats, RPKG_ANALYZER_BIN = .af_stub(
        dir, build[[1L]], first = c("1.0" = k$first), stats = build[[2L]]))
      expect_unusable(k$reason, paste(case, build[[1L]]))
    }
  }
  expect_length(readLines(stats), 9L)

  # Status 127 after partial output: R raises its own error and reports no status.
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(
    dir, "0.4.0-test", endings = c("1.0" = "exit 127")))
  expect_unusable(.AF_UNUSABLE$not_found$reason, "exit 127")

  # A command that is not there when it is run.
  .local_global("rpkg_analyzer_bin", function() "/nonexistent/rpkg-analyzer")
  expect_unusable(.AF_UNUSABLE$not_found$reason, "nonexistent command")

  # No binary at all.
  .local_global("rpkg_analyzer_bin", function() "")
  expect_unusable("analyzer binary not found", "no binary")
})

test_that("a zero exit is accepted on a protected version and on another", {
  skip_on_os("windows")
  tree <- .af_tree("1.0")
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "0.4.0-test"))
  want <- analyze_with_binary(tree)
  expect_identical(want$n_fns_r, 1L)
  expect_identical(analyze_with_binary(tree, protect = TRUE), want)
})

test_that("a zero exit without the statistics line is analyzer_killed when the build writes one", {
  skip_on_os("windows")
  tree  <- .af_tree("1.0")
  dir   <- withr::local_tempdir()
  stats <- file.path(withr::local_tempdir(), "stats.ndjson")
  withr::local_envvar(RPKG_ANALYZER_STATS = stats,
                      RPKG_ANALYZER_BIN = .af_stub(dir, "0.5.1-test", stats = TRUE))
  # A run that appends its line is accepted, the first into a file not yet there.
  for (protect in c(TRUE, FALSE)) {
    expect_identical(analyze_with_binary(tree, protect = protect)$n_fns_r, 1L)
  }
  expect_length(readLines(stats), 2L)

  .af_stub(dir, "0.5.1-test", endings = c("1.0" = "exit 0"), stats = TRUE)
  for (protect in c(TRUE, FALSE)) {
    err <- tryCatch(analyze_with_binary(tree, protect = protect), error = function(e) e)
    expect_s3_class(err, c("analyzer_killed", "error", "condition"), exact = TRUE)
    expect_identical(err$status, 0L)
    expect_identical(conditionMessage(err), "analyzer exited 0 without its statistics line")
  }
  expect_length(readLines(stats), 2L)
})

test_that("a zero exit needs no statistics line from a build or a run that writes none", {
  skip_on_os("windows")
  tree  <- .af_tree("1.0")
  dir   <- withr::local_tempdir()
  stats <- file.path(withr::local_tempdir(), "stats.ndjson")
  # 0.4.0 and 0.5.0 ignore RPKG_ANALYZER_STATS.
  for (build in c("0.4.0", "0.4.0-test", "0.5.0")) {
    withr::local_envvar(RPKG_ANALYZER_STATS = stats,
                        RPKG_ANALYZER_BIN = .af_stub(dir, build))
    for (protect in c(TRUE, FALSE)) {
      expect_identical(analyze_with_binary(tree, protect = protect)$n_fns_r, 1L,
                       info = build)
    }
  }
  expect_false(file.exists(stats))
  # 0.5.1 writes no line when the variable is unset or empty.
  for (unset in list(NA, "")) {
    withr::local_envvar(RPKG_ANALYZER_STATS = unset,
                        RPKG_ANALYZER_BIN = .af_stub(dir, "0.5.1-test", stats = TRUE))
    expect_identical(analyze_with_binary(tree, protect = TRUE)$n_fns_r, 1L)
  }
})

test_that("both conditions are a crash, whatever the elapsed time", {
  for (e in list(.analyzer_killed(137L), .analyzer_killed(0L),
                 .analyzer_failed("analyzer exited with status 101", 101L),
                 .analyzer_failed("analyzer binary not found"))) {
    expect_identical(.classify_failure(e, 1, worker_timeout = 600L), "crash")
    expect_identical(.classify_failure(e, 900, worker_timeout = 600L), "crash")
  }
  expect_identical(.failure_class("crash"), "timeout")
})

test_that("the self-check reads a killed analyzer and a missing statistics line as a failed check", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, PREV_CODE_TAG = "", PREV_DATA_TAG = "",
                      PREV_TEXT_TAG = "",
                      RPKG_ANALYZER_BIN = .af_stub(dir, "0.5.1-test", stats = TRUE))
  expect_true(rpkg_analyzer_selfcheck("release"))

  .af_stub(dir, "0.5.1-test", endings = c("0.0.1" = "exit 137"), stats = TRUE)
  expect_false(rpkg_analyzer_selfcheck("release"))
  out <- withr::local_tempdir()
  expect_error(run_update(.af_io("1.0"), out, shard_size = 10L), "--input-kind release")
  expect_false(file.exists(file.path(out, DB_FILENAME)))

  withr::local_envvar(RPKG_ANALYZER_STATS = file.path(withr::local_tempdir(), "stats.ndjson"))
  .af_stub(dir, "0.5.1-test", stats = TRUE)
  expect_true(rpkg_analyzer_selfcheck("release"))
  .af_stub(dir, "0.5.1-test", endings = c("0.0.1" = "exit 0"), stats = TRUE)
  expect_false(rpkg_analyzer_selfcheck("release"))
})

# ---------------------------------------------------------------------------
# Which versions are protected
# ---------------------------------------------------------------------------

test_that("the stamped versions are the stored rows that name an analyzer build, any build", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.stamped_versions(con, "pkgA"), list())

  DBI::dbWriteTable(con, SUMMARY_TABLE, data.frame(
    package = c("pkgA", "pkgB"), version = c("1.0", "1.0"), stringsAsFactors = FALSE))
  expect_identical(.stamped_versions(con, c("pkgA", "pkgB")), list())

  DBI::dbRemoveTable(con, SUMMARY_TABLE)
  DBI::dbWriteTable(con, SUMMARY_TABLE, data.frame(
    package = c("pkgA", "pkgA", "pkgA", "pkgA", "pkgB", "pkgC", "pkgD"),
    version = c("1.0", "1.1", "1.2", "1.3", "2.0", "3.0", "4.0"),
    analyzer_version = c("0.3.1", NA, "", "0.4.0", "0.5.1", NA, "0.4.0"),
    stringsAsFactors = FALSE))
  expect_identical(.stamped_versions(con, c("pkgA", "pkgB", "pkgC", "pkgNew")),
                   list(pkgA = c("1.0", "1.3"), pkgB = "2.0"))
  expect_identical(.stamped_versions(con, character(0L)), list())
  # More packages than one statement is given.
  many <- c(sprintf("pkg%04d", seq_len(1200L)), "pkgD")
  expect_identical(.stamped_versions(con, many), list(pkgD = "4.0"))
})

test_that("analyze_package protects the versions it is told have analyzer rows, and no other", {
  skip_on_os("windows")
  repo <- file.path(withr::local_tempdir(), "pkgA")
  .af_clone("pkgA", repo, c("1.0", "2.0"))
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "0.4.0-test", endings = c("2.0" = "exit 101")))

  res <- suppressWarnings(analyze_package(repo, "pkgA"))
  expect_identical(res$binary_versions, "1.0")
  expect_identical(res$summary$version, c("1.0", "2.0"))
  res <- suppressWarnings(analyze_package(repo, "pkgA", stamped = "1.0"))
  expect_identical(res$binary_versions, "1.0")

  err <- tryCatch(suppressWarnings(analyze_package(repo, "pkgA", stamped = c("1.0", "2.0"))),
                  error = function(e) e)
  expect_s3_class(err, "analyzer_failed")
  expect_identical(err$status, 101L)
})

test_that("the worker hands analyze_package the stamped versions of its own package", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  out      <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
    stub_dir, "0.4.0-test", endings = c("2.0" = "exit 1")))
  # pkgA keeps an analyzer row for 1.0 and a fallback row for 2.0; pkgB has none.
  expect_identical(.af_run(out, c("1.0", "2.0"))$n_fresh, 1L)
  expect_identical(.af_summary(out)$analyzer_version, c("0.4.0-test", NA))

  seen <- new.env(parent = emptyenv())
  .local_global("analyze_package", function(dest, pkg, stamped = "not passed") {
    assign(pkg, stamped, envir = seen)
    stop("seen")
  })
  .af_run(out, c("1.0", "2.0", "3.0"), pkgs = c("pkgA", "pkgB"))
  expect_identical(mget(c("pkgA", "pkgB"), envir = seen),
                   list(pkgA = "1.0", pkgB = character(0L)))
})

# ---------------------------------------------------------------------------
# A failed analyzer leaves the package's stored rows alone
# ---------------------------------------------------------------------------

test_that("a failed analyzer on a version with an analyzer row fails the package as a crash and changes no row", {
  skip_on_os("windows")
  for (case in names(.AF_CASES)) {
    k        <- .AF_CASES[[case]]
    stub_dir <- withr::local_tempdir()
    out      <- withr::local_tempdir()
    withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(stub_dir, k$build, stats = k$stats))
    expect_identical(.af_run(out, "1.0")$n_fresh, 1L, info = case)
    expect_identical(.af_summary(out)$analyzer_version, k$build, info = case)
    before <- .package_rows(out, "pkgA")

    # A new release puts the package back in the queue; 1.0 is read first.
    .af_stub(stub_dir, k$build, endings = c("1.0" = k$ending), stats = k$stats)
    m <- .af_run(out, c("1.0", "2.0"))

    expect_identical(m$shard_failures$packages, "pkgA", info = case)
    expect_identical(m$n_fresh, 0L, info = case)
    expect_identical(.package_rows(out, "pkgA"), before, info = case)
    expect_identical(.af_summary(out),
                     data.frame(version = "1.0", analyzer_version = k$build, n_fns_r = 1L,
                                stringsAsFactors = FALSE), info = case)
    expect_identical(.af_verdicts(out),
                     data.frame(package = "pkgA", stage = "crash", analyze_failures = 0L,
                                timeout_failures = 1L, reason = k$reason,
                                stringsAsFactors = FALSE), info = case)
    expect_true(any(grepl(paste0("FAIL pkgA: crash after [0-9.]+s", k$exits, ": ",
                                 k$reason, "$"), m$logged)), info = case)
  }
})

test_that("a killed analyzer on a version with no analyzer row fails the package as a crash and changes no row", {
  skip_on_os("windows")
  for (case in setdiff(names(.AF_CASES), "failed")) {
    k        <- .AF_CASES[[case]]
    stub_dir <- withr::local_tempdir()
    out      <- withr::local_tempdir()
    # The analyzer cannot read 1.0, so the stored row is the R fallback's.
    withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
      stub_dir, k$build, endings = c("1.0" = "exit 1"), stats = k$stats))
    expect_identical(.af_run(out, "1.0")$n_fresh, 1L, info = case)
    expect_identical(.af_summary(out),
                     data.frame(version = "1.0", analyzer_version = NA_character_,
                                n_fns_r = NA_integer_, stringsAsFactors = FALSE), info = case)
    before <- .package_rows(out, "pkgA")

    .af_stub(stub_dir, k$build, endings = c("1.0" = k$ending), stats = k$stats)
    m <- .af_run(out, "1.0")

    expect_identical(m$shard_failures$packages, "pkgA", info = case)
    expect_identical(.package_rows(out, "pkgA"), before, info = case)
    expect_identical(.af_verdicts(out),
                     data.frame(package = "pkgA", stage = "crash", analyze_failures = 0L,
                                timeout_failures = 1L, reason = k$reason,
                                stringsAsFactors = FALSE), info = case)
  }
})

test_that("a killed analyzer on a package with no stored row stores none", {
  skip_on_os("windows")
  out <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "0.4.0-test", endings = c("1.0" = "kill -9 $$")))
  m <- .af_run(out, "1.0")
  expect_identical(m$shard_failures$packages, "pkgA")
  expect_identical(.af_verdicts(out)[c("stage", "timeout_failures", "reason")],
                   data.frame(stage = "crash", timeout_failures = 1L,
                              reason = "analyzer exited with status 137",
                              stringsAsFactors = FALSE))
  expect_identical(nrow(.af_summary(out)), 0L)
})

test_that("a non-zero exit on a version with no analyzer row still takes the R fallback", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  out      <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(stub_dir, "0.4.0-test"))
  expect_identical(.af_run(out, "1.0")$n_fresh, 1L)

  # The new release is a tree the analyzer cannot read; 1.0 is read as before.
  .af_stub(stub_dir, "0.4.0-test", endings = c("2.0" = "exit 101"))
  m <- .af_run(out, c("1.0", "2.0"))
  expect_identical(m$n_fresh, 1L)
  expect_identical(m$shard_failures$count, 0L)
  expect_identical(.af_summary(out),
                   data.frame(version = c("1.0", "2.0"),
                              analyzer_version = c("0.4.0-test", NA),
                              n_fns_r = c(1L, NA), stringsAsFactors = FALSE))
  expect_identical(nrow(.af_verdicts(out)), 0L)

  # Asked again, the fallback row is still not protected and 1.0 still is.
  m <- .af_run(out, c("1.0", "2.0"))
  expect_identical(m$shard_failures$count, 0L)
  expect_identical(.af_summary(out)$analyzer_version, c("0.4.0-test", NA))
})

test_that("an analyzer that gives no usable result on a version with an analyzer row fails the package as a crash and changes no row", {
  skip_on_os("windows")
  for (case in c(names(.AF_UNUSABLE), "no_binary")) {
    stub_dir <- withr::local_tempdir()
    out      <- withr::local_tempdir()
    withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(stub_dir, "0.4.0-test"))
    expect_identical(.af_run(out, "1.0")$n_fresh, 1L, info = case)
    before <- .package_rows(out, "pkgA")

    # A new release puts the package back in the queue; 1.0 is read first.
    if (identical(case, "no_binary")) {
      reason <- "analyzer binary not found"
      m <- .af_without_binary(.af_run(out, c("1.0", "2.0")))
    } else {
      reason <- .AF_UNUSABLE[[case]]$reason
      .af_stub(stub_dir, "0.4.0-test", first = c("1.0" = .AF_UNUSABLE[[case]]$first))
      m <- .af_run(out, c("1.0", "2.0"))
    }

    expect_identical(m$shard_failures$packages, "pkgA", info = case)
    expect_identical(m$n_fresh, 0L, info = case)
    expect_identical(.package_rows(out, "pkgA"), before, info = case)
    expect_identical(.af_summary(out),
                     data.frame(version = "1.0", analyzer_version = "0.4.0-test", n_fns_r = 1L,
                                stringsAsFactors = FALSE), info = case)
    expect_identical(.af_verdicts(out),
                     data.frame(package = "pkgA", stage = "crash", analyze_failures = 0L,
                                timeout_failures = 1L, reason = paste0(reason, .AF_PROTECTED),
                                stringsAsFactors = FALSE), info = case)
  }
})

test_that("an analyzer that gives no usable result on a version with no analyzer row still takes the R fallback", {
  skip_on_os("windows")
  for (case in c(names(.AF_UNUSABLE), "no_binary")) {
    stub_dir <- withr::local_tempdir()
    out      <- withr::local_tempdir()
    if (identical(case, "no_binary")) {
      m <- .af_without_binary(.af_run(out, "1.0"))
    } else {
      withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
        stub_dir, "0.4.0-test", first = c("1.0" = .AF_UNUSABLE[[case]]$first)))
      m <- .af_run(out, "1.0")
    }
    expect_identical(m$n_fresh, 1L, info = case)
    expect_identical(m$shard_failures$count, 0L, info = case)
    expect_identical(.af_summary(out),
                     data.frame(version = "1.0", analyzer_version = NA_character_,
                                n_fns_r = NA_integer_, stringsAsFactors = FALSE), info = case)
    expect_identical(nrow(.af_verdicts(out)), 0L, info = case)
  }
})

test_that("a package that failed for want of a binary is recorded under no build and is asked again once a binary is back", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  out      <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(stub_dir, "0.4.0-test"))
  .af_run(out, "1.0")
  before <- .package_rows(out, "pkgA")

  for (i in seq_len(MAX_TIMEOUT_FAILURES)) {
    m <- .af_without_binary(.af_run(out, c("1.0", "2.0")))
    expect_identical(m$n_shard, 1L)
    expect_identical(
      .af_query(out, "SELECT stage, analyzer_version, timeout_failures FROM cran_metrics_failures"),
      data.frame(stage = "crash", analyzer_version = "", timeout_failures = i,
                 stringsAsFactors = FALSE))
  }
  # Parked while there is still no binary, with every row as it was.
  m <- .af_without_binary(.af_run(out, c("1.0", "2.0")))
  expect_identical(m$n_shard, 0L)
  expect_equal(m$permanent_failures, 1L)
  expect_identical(.package_rows(out, "pkgA"), before)

  # The binary is back: no build ever parked the package, so it is read.
  m <- .af_run(out, c("1.0", "2.0"))
  expect_identical(m$n_shard, 1L)
  expect_identical(m$n_fresh, 1L)
  expect_identical(.af_summary(out)$analyzer_version, c("0.4.0-test", "0.4.0-test"))
  expect_identical(nrow(.af_verdicts(out)), 0L)
})

test_that("a package whose analyzer keeps failing parks after MAX_TIMEOUT_FAILURES under one build, and a new build retries it", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  out      <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(stub_dir, "0.4.0-test"))
  .af_run(out, "1.0")
  before <- .package_rows(out, "pkgA")

  .af_stub(stub_dir, "0.4.0-test", endings = c("1.0" = "exit 101"))
  for (i in seq_len(MAX_TIMEOUT_FAILURES)) {
    m <- .af_run(out, c("1.0", "2.0"))
    expect_identical(m$n_shard, 1L)
    expect_identical(.af_verdicts(out)[c("stage", "timeout_failures")],
                     data.frame(stage = "crash", timeout_failures = i,
                                stringsAsFactors = FALSE))
  }
  m <- .af_run(out, c("1.0", "2.0"))
  expect_identical(m$n_shard, 0L)
  expect_equal(m$permanent_failures, 1L)
  expect_identical(.package_rows(out, "pkgA"), before)

  .af_stub(stub_dir, "0.4.1-test")
  m <- .af_run(out, c("1.0", "2.0"))
  expect_identical(m$n_fresh, 1L)
  expect_identical(.af_summary(out)$analyzer_version, c("0.4.1-test", "0.4.1-test"))
  expect_identical(nrow(.af_verdicts(out)), 0L)
})
