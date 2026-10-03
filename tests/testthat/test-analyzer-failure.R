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

# What a failure with a non-zero exit says of the address-space limit a run of
# `build` gets: nothing for a build before 0.5.2, or where there is no prlimit,
# as off Linux.
.af_note <- function(build) .limit_note(.analyzer_limit(build)$limit_mb)

# How each stub ends on the version it fails, the build it reports, whether
# that build writes a statistics line, the exits the worker's line names, and
# whether the reason goes on to name the limit in force. 0.4.0 writes no
# statistics line, as the pinned build.
.AF_CASES <- list(
  abort         = list(ending = "exit 134", build = "0.4.0-test", stats = FALSE,
                       exits = " \\[analyzer exit 134 x1\\]", noted = TRUE,
                       reason = "analyzer exited with status 134"),
  killed        = list(ending = "exit 137", build = "0.4.0-test", stats = FALSE,
                       exits = " \\[analyzer exit 137 x1\\]", noted = TRUE,
                       reason = "analyzer exited with status 137"),
  failed        = list(ending = "exit 101", build = "0.4.0-test", stats = FALSE,
                       exits = " \\[analyzer exit 101 x1\\]", noted = TRUE,
                       reason = "analyzer exited with status 101 on a version with analyzer rows"),
  no_statistics = list(ending = "exit 0", build = "0.5.1-test", stats = TRUE, exits = "",
                       noted = FALSE,
                       reason = "analyzer exited 0 without its statistics line"))

# The reason a case stores and prints, under the limit its build gets.
.af_reason <- function(k) paste0(k$reason, if (k$noted) .af_note(k$build) else "")

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
                       sprintf("analyzer exited with status %d%s", endings[[ending]],
                               .af_note("0.4.0-test")),
                       info = info)
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
      "analyzer exited with status %d on a version with analyzer rows%s", status,
      .af_note("0.4.0-test")))
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
  .local_global("analyze_package", function(dest, pkg, stamped = "not passed", limit = NULL) {
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
                                timeout_failures = 1L, reason = .af_reason(k),
                                stringsAsFactors = FALSE), info = case)
    expect_true(any(grepl(paste0("FAIL pkgA: crash after [0-9.]+s", k$exits, ": ",
                                 .af_reason(k), "$"), m$logged)), info = case)
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
                                timeout_failures = 1L, reason = .af_reason(k),
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
                              reason = paste0("analyzer exited with status 137",
                                              .af_note("0.4.0-test")),
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

# ---------------------------------------------------------------------------
# A limit on the analyzer's address space
# ---------------------------------------------------------------------------

# A stand-in for prlimit, first on the path until the calling test ends. It
# appends its arguments to `log`, one line a call, and runs the command that
# follows its first argument with no limit at all.
.fake_prlimit <- function(log, frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  writeLines(c("#!/bin/sh", sprintf('echo "$*" >> "%s"', log), "shift", 'exec "$@"'),
             file.path(dir, "prlimit"))
  Sys.chmod(file.path(dir, "prlimit"), mode = "0755")
  withr::local_path(dir, .local_envir = frame)
  invisible(dir)
}

# A shell line for .af_stub: abort, as the analyzer does when an allocation
# fails, unless the process may take at least 1 GiB of address space.
.AF_NEEDS_1GIB <- paste0('lim=$(ulimit -v); if [ "$lim" != unlimited ] && ',
                         '[ "$lim" -lt 1048576 ]; then kill -ABRT $$; fi')

test_that("the limit is ANALYZER_MEMORY_LIMIT_MB, a whole number of MiB, and 3072 when it names none", {
  expect_identical(.memory_limit_mb(""), 3072)
  expect_identical(.memory_limit_mb("0"), 0)
  expect_identical(.memory_limit_mb("4096"), 4096)
  expect_identical(.memory_limit_mb(" 4096 "), 4096)
  expect_identical(.memory_limit_mb("2147483647"), 2147483647)
  # Only digits, within the integer range, name a limit. A fraction, a sign, an
  # exponent, hex and the words R reads as numbers all leave the default, never
  # 0 or a truncated figure.
  for (bad in c("", "  ", "-1", "0.5", "-0.5", "NaN", "3.5", "1e3", "0x800", "lots", "Inf",
                "2147483648", "64MiB")) {
    expect_identical(.memory_limit_mb(bad), 3072, info = bad)
  }
  # config.R reads the variable once, when it is sourced.
  expect_identical(ANALYZER_MEMORY_LIMIT_MB,
                   .memory_limit_mb(Sys.getenv("ANALYZER_MEMORY_LIMIT_MB", unset = "")))
})

test_that("a failure with a non-zero exit names the limit it ran under, and no other failure does", {
  expect_identical(.limit_note(512), "; its address-space limit was 512 MiB")
  expect_identical(.limit_note(4096), "; its address-space limit was 4096 MiB")
  expect_identical(.limit_note(0), "")
  expect_identical(conditionMessage(.analyzer_killed(134L, 512)),
                   "analyzer exited with status 134; its address-space limit was 512 MiB")
  expect_identical(conditionMessage(.analyzer_killed(134L)), "analyzer exited with status 134")
  expect_identical(conditionMessage(.analyzer_killed(0L, 512)),
                   "analyzer exited 0 without its statistics line")
  expect_identical(
    conditionMessage(.analyzer_failed("analyzer exited with status 101", 101L, 512)),
    paste0("analyzer exited with status 101 on a version with analyzer rows",
           "; its address-space limit was 512 MiB"))
  for (status in list(0L, NA_integer_)) {
    expect_identical(conditionMessage(.analyzer_failed("analyzer gave nothing", status, 512)),
                     "analyzer gave nothing on a version with analyzer rows")
  }
})

test_that("a build of 0.5.2 or later gets the limit where prlimit is found, and any other run none", {
  skip_on_os("windows")
  none <- list(limit_mb = 0, prlimit = "")
  log  <- file.path(withr::local_tempdir(), "prlimit.log")
  fake <- file.path(.fake_prlimit(log), "prlimit")
  limit <- .analyzer_limit("0.5.2", 4096)
  expect_identical(normalizePath(limit$prlimit), normalizePath(fake))
  expect_identical(limit$limit_mb, 4096)
  expect_identical(.analyzer_limit("0.6.0-test", 4096)$limit_mb, 4096)
  expect_identical(.analyzer_limit("0.5.2", 0), none)
  # A build before 0.5.2 can write a wrong record and exit 0 when an allocation fails.
  for (build in c("0.5.1", "0.5.1-test", "0.4.0-test", "dev", NA)) {
    expect_identical(.analyzer_limit(build, 4096), none, info = build)
  }
  # With no prlimit on the path there is nothing to hold a limit.
  withr::local_envvar(PATH = withr::local_tempdir())
  expect_identical(.analyzer_limit("0.5.2", 4096), none)
  expect_false(file.exists(log))
})

test_that("the analyzer command is prlimit with the limit in bytes, or the binary alone", {
  bin  <- "/opt/an analyzer/rpkg-analyzer"
  held <- function(mb) list(limit_mb = mb, prlimit = "/usr/bin/prlimit")
  expect_identical(.analyzer_command(bin, held(4096)),
                   list(command = "/usr/bin/prlimit",
                        args = c("--as=4294967296", shQuote(bin)), limit_mb = 4096))
  expect_identical(.analyzer_command(bin, held(1))$args, c("--as=1048576", shQuote(bin)))
  # With no limit the call is the one made before there was a limit.
  expect_identical(.analyzer_command(bin, list(limit_mb = 0, prlimit = "")),
                   list(command = bin, args = character(0L), limit_mb = 0))
})

test_that("a build before 0.5.2 is called without prlimit, by a package and by the self-check", {
  skip_on_os("windows")
  tree <- .af_tree("1.0")
  log  <- file.path(withr::local_tempdir(), "prlimit.log")
  .fake_prlimit(log)
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 4096)
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "0.5.1"))
  expect_identical(analyze_with_binary(tree, protect = TRUE)$n_fns_r, 1L)
  expect_true(rpkg_analyzer_selfcheck("release"))
  expect_false(file.exists(log))

  # The same calls with a 0.5.2 build go through prlimit.
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "0.5.2"))
  expect_identical(analyze_with_binary(tree, protect = TRUE)$n_fns_r, 1L)
  expect_true(rpkg_analyzer_selfcheck("release"))
  expect_length(readLines(log), 2L)
})

test_that("the analyzer runs under prlimit with the limit in bytes, and bare when the limit is 0", {
  skip_on_os("windows")
  tree <- .af_tree("1.0")
  log  <- file.path(withr::local_tempdir(), "prlimit.log")
  .fake_prlimit(log)
  stub <- .af_stub(withr::local_tempdir(), "0.5.2-test")
  withr::local_envvar(RPKG_ANALYZER_BIN = stub, RPKG_ANALYZER_STATS = NA)

  .local_global("ANALYZER_MEMORY_LIMIT_MB", 4096)
  expect_identical(analyze_with_binary(tree)$n_fns_r, 1L)
  expect_identical(readLines(log), sprintf("--as=4294967296 %s %s --input-kind %s",
                                           stub, tree, ANALYZER_INPUT_KIND))

  .local_global("ANALYZER_MEMORY_LIMIT_MB", 0)
  expect_identical(analyze_with_binary(tree)$n_fns_r, 1L)
  expect_length(readLines(log), 1L)

  # A run that fails under a limit says which, and one that fails with none does not.
  .af_stub(dirname(stub), "0.5.2-test", endings = c("1.0" = "exit 134"))
  expect_error(analyze_with_binary(tree), "^analyzer exited with status 134$",
               class = "analyzer_killed")
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 4096)
  expect_error(analyze_with_binary(tree),
               "^analyzer exited with status 134; its address-space limit was 4096 MiB$",
               class = "analyzer_killed")
  .af_stub(dirname(stub), "0.5.2-test", endings = c("1.0" = "exit 101"))
  expect_error(analyze_with_binary(tree, protect = TRUE), paste0(
    "^analyzer exited with status 101 on a version with analyzer rows; ",
    "its address-space limit was 4096 MiB$"), class = "analyzer_failed")
})

test_that("a run looks for prlimit once, not once an analyzer", {
  skip_on_os("windows")
  log <- file.path(withr::local_tempdir(), "prlimit.log")
  fake <- file.path(.fake_prlimit(log), "prlimit")
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "0.5.2-test", stats = TRUE))
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 2048)
  asked <- new.env(parent = emptyenv())
  asked$n <- 0L
  .local_global(".prlimit_bin", function() {
    asked$n <- asked$n + 1L
    fake
  })
  # .af_run uses one core, so each package's analyzer runs in this process.
  m <- .af_run(withr::local_tempdir(), c("1.0", "2.0", "3.0"), pkgs = c("pkgA", "pkgB"))
  expect_identical(m$n_fresh, 2L)
  expect_length(readLines(log), 7L)
  expect_identical(asked$n, 1L)
})

test_that("the self-check runs under the same limit", {
  skip_on_os("windows")
  log <- file.path(withr::local_tempdir(), "prlimit.log")
  .fake_prlimit(log)
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "0.5.2-test", stats = TRUE))
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 2048)
  expect_true(rpkg_analyzer_selfcheck("release"))
  expect_length(readLines(log), 1L)
  expect_match(readLines(log), "^--as=2147483648 .* --input-kind release$")
})

test_that("the shard plan and run-status.json say which limit is in force", {
  skip_on_os("windows")
  out <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "0.5.2-test",
                                                   stats = TRUE))
  limit_of <- function() {
    jsonlite::fromJSON(file.path(out, "run-status.json"))$analyzer_memory_limit_mb
  }

  .fake_prlimit(file.path(withr::local_tempdir(), "prlimit.log"))
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 4096)
  expect_identical(.analyzer_limit()$limit_mb, 4096)
  m <- .af_run(out, "1.0")
  expect_true("analyzer memory limit: 4096 MiB of address space for each analyzer" %in%
                m$logged)
  expect_equal(limit_of(), 4096)

  .local_global("ANALYZER_MEMORY_LIMIT_MB", 0)
  m <- .af_run(out, c("1.0", "2.0"))
  expect_true("analyzer memory limit: none (ANALYZER_MEMORY_LIMIT_MB is 0)" %in% m$logged)
  expect_equal(limit_of(), 0)

  # No prlimit to be found: the limit asked for is not in force, and the run says so.
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 4096)
  .local_global(".prlimit_bin", function() "")
  expect_identical(.analyzer_limit()$limit_mb, 0)
  m <- .af_run(out, c("1.0", "2.0", "3.0"))
  expect_true(paste("analyzer memory limit: none, prlimit was not found",
                    "(ANALYZER_MEMORY_LIMIT_MB is 4096)") %in% m$logged)
  expect_equal(limit_of(), 0)
  expect_identical(m$n_fresh, 1L)

  # A build that reports no version gets none either.
  expect_identical(.memory_limit_line(0, NA, 4096), paste(
    "analyzer memory limit: none, no rpkg-analyzer version was read",
    "(ANALYZER_MEMORY_LIMIT_MB is 4096)"))
})

test_that("a run with a build before 0.5.2 sets no limit, says why, and never calls prlimit", {
  skip_on_os("windows")
  log <- file.path(withr::local_tempdir(), "prlimit.log")
  .fake_prlimit(log)
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 4096)
  limit_of <- function(out) {
    jsonlite::fromJSON(file.path(out, "run-status.json"))$analyzer_memory_limit_mb
  }

  out <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "0.5.1", stats = TRUE))
  m <- .af_run(out, "1.0")
  expect_identical(m$n_fresh, 1L)
  expect_true(paste("analyzer memory limit: none, rpkg-analyzer 0.5.1 is older than 0.5.2",
                    "(ANALYZER_MEMORY_LIMIT_MB is 4096)") %in% m$logged)
  expect_equal(limit_of(out), 0)
  # Neither the self-check nor the package's analyzer went through prlimit.
  expect_false(file.exists(log))

  # The same run with a 0.5.2 build: both do, under the limit.
  out <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "0.5.2", stats = TRUE))
  m <- .af_run(out, "1.0")
  expect_identical(m$n_fresh, 1L)
  expect_true("analyzer memory limit: 4096 MiB of address space for each analyzer" %in%
                m$logged)
  expect_equal(limit_of(out), 4096)
  expect_length(readLines(log), 2L)
  expect_match(readLines(log), "^--as=4294967296 ")
})

test_that("an analyzer aborted by the limit fails the package as a crash, writes nothing and changes no row", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("prlimit")), "prlimit is not on the path")
  stub_dir <- withr::local_tempdir()
  out      <- withr::local_tempdir()
  # 1.0 needs 1 GiB; every other version, the self-check's included, needs little.
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
    stub_dir, "0.5.2-test", first = c("1.0" = .AF_NEEDS_1GIB), stats = TRUE))

  .local_global("ANALYZER_MEMORY_LIMIT_MB", 2048)
  expect_identical(.af_run(out, "1.0")$n_fresh, 1L)
  expect_identical(.af_summary(out)$analyzer_version, "0.5.2-test")
  before <- .package_rows(out, "pkgA")

  # A new release puts the package back in the queue, under a limit 1.0 no longer fits.
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 512)
  m <- .af_run(out, c("1.0", "2.0"))
  expect_identical(m$shard_failures$packages, "pkgA")
  expect_identical(m$n_fresh, 0L)
  expect_identical(m$n_versions, 0L)
  expect_identical(.package_rows(out, "pkgA"), before)
  expect_identical(.af_verdicts(out),
                   data.frame(package = "pkgA", stage = "crash", analyze_failures = 0L,
                              timeout_failures = 1L,
                              reason = paste0("analyzer exited with status 134; ",
                                              "its address-space limit was 512 MiB"),
                              stringsAsFactors = FALSE))
  expect_true(any(grepl(paste0(
    "FAIL pkgA: crash after [0-9.]+s \\[analyzer exit 134 x1\\]: analyzer exited with ",
    "status 134; its address-space limit was 512 MiB$"), m$logged)))

  # With no limit the same analyzer reads both versions.
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 0)
  m <- .af_run(out, c("1.0", "2.0"))
  expect_identical(m$n_fresh, 1L)
  expect_identical(.af_summary(out)$analyzer_version, c("0.5.2-test", "0.5.2-test"))
})

test_that("an analyzer aborted by the limit on a package with no stored row stores none", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("prlimit")), "prlimit is not on the path")
  out <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "0.5.2-test", first = c("1.0" = .AF_NEEDS_1GIB), stats = TRUE))
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 512)
  m <- .af_run(out, "1.0")
  expect_identical(m$shard_failures$packages, "pkgA")
  expect_identical(.af_verdicts(out)[c("stage", "timeout_failures", "reason")],
                   data.frame(stage = "crash", timeout_failures = 1L,
                              reason = paste0("analyzer exited with status 134; ",
                                              "its address-space limit was 512 MiB"),
                              stringsAsFactors = FALSE))
  expect_identical(nrow(.af_summary(out)), 0L)
})

test_that("a build before 0.5.2 is not held to the limit, so it reads a version the limit would abort", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("prlimit")), "prlimit is not on the path")
  out <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "0.5.1-test", first = c("1.0" = .AF_NEEDS_1GIB), stats = TRUE))
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 512)
  m <- .af_run(out, "1.0")
  expect_identical(m$n_fresh, 1L)
  expect_identical(.af_summary(out)$analyzer_version, "0.5.1-test")
  expect_true(paste("analyzer memory limit: none, rpkg-analyzer 0.5.1-test is older than 0.5.2",
                    "(ANALYZER_MEMORY_LIMIT_MB is 512)") %in% m$logged)
})

test_that("a self-check aborted by the limit stops the run before any shard", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("prlimit")), "prlimit is not on the path")
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, PREV_CODE_TAG = "", PREV_DATA_TAG = "",
                      PREV_TEXT_TAG = "", RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "0.5.2-test", first = c("0.0.1" = .AF_NEEDS_1GIB), stats = TRUE))
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 2048)
  expect_true(rpkg_analyzer_selfcheck("release"))
  .local_global("ANALYZER_MEMORY_LIMIT_MB", 512)
  expect_false(rpkg_analyzer_selfcheck("release"))
  out <- withr::local_tempdir()
  expect_error(run_update(.af_io("1.0"), out, shard_size = 10L), paste(
    "--input-kind release with a summary naming it under its 512 MiB address-space limit;",
    "stopping before any shard"), fixed = TRUE)
  expect_false(file.exists(file.path(out, DB_FILENAME)))
})
