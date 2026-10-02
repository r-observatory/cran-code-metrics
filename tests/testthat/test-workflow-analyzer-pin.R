# tests/testthat/test-workflow-analyzer-pin.R
# The tests and the daily update must run the same analyzer build, and the pin
# says what it does to the rows an older build wrote.
.analyzer_pins <- function(file) {
  yml <- readLines(file.path("..", "..", ".github", "workflows", file))
  hits <- regmatches(yml, regexpr(
    "gh release download v[0-9]+\\.[0-9]+\\.[0-9]+ --repo r-observatory/rpkg-analyzer", yml))
  sub("^gh release download (v[0-9.]+) .*$", "\\1", hits)
}

test_that("update.yml and test.yml install rpkg-analyzer v0.5.2", {
  expect_identical(.analyzer_pins("update.yml"), "v0.5.2")
  expect_identical(.analyzer_pins("test.yml"), "v0.5.2")
})

test_that("the pinned build is in ANALYZER_SAME_OUTPUT, so a pin change says what it re-queues", {
  expect_true(sub("^v", "", .analyzer_pins("update.yml")) %in% ANALYZER_SAME_OUTPUT)
})

test_that("0.5.0, 0.5.1 and 0.5.2 count as one build and 0.4.0 stands alone, so this pin rescans once", {
  expect_identical(ANALYZER_SAME_OUTPUT, c("0.5.0", "0.5.1", "0.5.2"))
  for (build in ANALYZER_SAME_OUTPUT) {
    expect_identical(.analyzer_output_class(build), ANALYZER_SAME_OUTPUT, info = build)
  }
  expect_identical(.analyzer_output_class("0.4.0"), "0.4.0")
})

test_that("the installed analyzer reports the pinned build, as the class names it", {
  skip_if(!nzchar(Sys.getenv("RPKG_ANALYZER_BIN")), "RPKG_ANALYZER_BIN is not set")
  v <- rpkg_analyzer_version()
  expect_identical(v, sub("^v", "", .analyzer_pins("update.yml")))
  expect_true(v %in% ANALYZER_SAME_OUTPUT)
})

# ---------------------------------------------------------------------------
# What the pin does to a database of 0.4.0 rows
# ---------------------------------------------------------------------------

# A universe of packages, each at one version of its own, and an io that
# clones a real git repository with that version tagged.
.pin_io <- function(versions) list(
  package_list = function() data.frame(package = names(versions),
                                       latest_version = unname(versions),
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) {
    git <- function(...) {
      system2("git", c("-C", dest, "-c", "user.name=Test", "-c", "user.email=t@example.com", ...),
              stdout = FALSE, stderr = FALSE)
    }
    dir.create(file.path(dest, "R"), recursive = TRUE, showWarnings = FALSE)
    system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
    writeLines("export(hello)", file.path(dest, "NAMESPACE"))
    writeLines("hello <- function() 'hello'", file.path(dest, "R", "hello.R"))
    writeLines(c(paste("Package:", pkg), paste("Version:", versions[[pkg]]),
                 "Title: Test Package", "Description: Minimal package for the pin tests.",
                 "Author: Test Bot", "Maintainer: Test Bot <t@example.com>",
                 "License: MIT"), file.path(dest, "DESCRIPTION"))
    git("add", "-A")
    git("commit", "-m", versions[[pkg]])
    git("tag", versions[[pkg]])
    TRUE
  })

# One run_update on one core, so the workers run in this process. Returns the
# manifest with the run's messages as $messages and its printed lines as $logged.
.pin_run <- function(io, out, shard_size = 10L) {
  .local_global("WORK_DIR", withr::local_tempdir())
  .local_global("ANALYSIS_CORES", 1L)
  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = "",
                        RPKG_ANALYZER_STATS = NA, RPA_CACHE = NA))
  seen <- new.env(parent = emptyenv())
  seen$messages <- character(0L)
  m <- NULL
  logged <- capture.output(
    m <- withCallingHandlers(
      suppressWarnings(run_update(io, out, shard_size = shard_size)),
      message = function(cond) {
        seen$messages <- c(seen$messages, trimws(conditionMessage(cond)))
        invokeRestart("muffleMessage")
      }))
  c(m, list(messages = seen$messages, logged = logged))
}

.pin_query <- function(out, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql)
}

.pin_execute <- function(out, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, sql)
}

# A package's stored rows in the columns `was` had, less the columns a 0.5.0
# build retires and the scan marker the pin clears to queue the package.
.pin_kept <- function(rows, was) {
  drop <- c(names(.RETIRED_SUMMARY_COLS), "datasets_scanned")
  Map(function(df, old) df[setdiff(names(old), drop)], rows[names(was)], was)
}

# One package's summary row.
.pin_summary_row <- function(out, pkg) {
  .pin_query(out, sprintf("SELECT * FROM \"%s\" WHERE package = '%s'", SUMMARY_TABLE, pkg))
}

# The build each package's latest row names, by package.
.pin_builds <- function(out) {
  df <- .pin_query(out, sprintf(
    'SELECT package, analyzer_version FROM "%s"
      WHERE latest_release_date IS NOT NULL ORDER BY package', SUMMARY_TABLE))
  stats::setNames(as.character(df$analyzer_version), df$package)
}

test_that("under the 0.5.2 pin every package of a 0.4.0 database is re-read, and one the new build cannot read keeps its rows", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  out      <- withr::local_tempdir()
  io <- .pin_io(c(pkgA = "1.0", pkgB = "2.0", pkgC = "3.0"))

  # The database as it stands today: every row written by 0.4.0, nothing queued.
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    stub_dir, "0.4.0", reads = c("1.0", "2.0", "3.0")))
  expect_identical(.pin_run(io, out)$n_fresh, 3L)
  settled <- .pin_run(io, out)
  expect_identical(settled$n_shard, 0L)
  expect_true("analyzer 0.4.0, output class 0.4.0; latest rows on class: 3 of 3" %in%
                settled$messages)
  expect_identical(.pin_builds(out), c(pkgA = "0.4.0", pkgB = "0.4.0", pkgC = "0.4.0"))
  # 0.4.0 also wrote the two columns a 0.5.0 build retires.
  for (col in names(.RETIRED_SUMMARY_COLS)) {
    .pin_execute(out, sprintf('ALTER TABLE "%s" ADD COLUMN "%s" INTEGER', SUMMARY_TABLE, col))
    .pin_execute(out, sprintf('UPDATE "%s" SET "%s" = 1', SUMMARY_TABLE, col))
  }
  before <- lapply(c(pkgA = "pkgA", pkgB = "pkgB", pkgC = "pkgC"),
                   function(p) .package_rows(out, p))
  cols_040 <- names(.pin_summary_row(out, "pkgC"))

  # The pinned build. It reads the self-check package, pkgA and pkgC, and exits
  # non-zero on pkgB. Two packages a shard, so the queue takes more than one.
  .stub_analyzer_bin(stub_dir, "0.5.2", reads = c("0.0.1", "1.0", "3.0"),
                     input_kind = ANALYZER_INPUT_KIND, stats = TRUE)
  first <- .pin_run(io, out, shard_size = 2L)

  # Every package is back in the queue.
  expect_true("dataset scans invalidated by analyzer change: 3" %in% first$messages)
  expect_true(paste("analyzer 0.5.2, output class 0.5.0 0.5.1 0.5.2;",
                    "latest rows on class: 0 of 3") %in% first$messages)
  expect_identical(first$n_shard, 2L)
  expect_identical(first$n_remaining, 1L)

  # pkgA was re-read. pkgC waits for the next shard with its 0.4.0 rows: every
  # value it had is as it was, in every table.
  expect_identical(first$n_fresh, 1L)
  expect_identical(.pin_builds(out), c(pkgA = "0.5.2", pkgB = "0.4.0", pkgC = "0.4.0"))
  expect_identical(.pin_kept(.package_rows(out, "pkgC"), before$pkgC),
                   .pin_kept(before$pkgC, before$pkgC))

  # What the first shard did change on a row not yet re-read: the scan marker is
  # cleared, the retired columns are gone from the table, and the columns a
  # 0.5.0 build adds are there and empty.
  waiting <- .pin_summary_row(out, "pkgC")
  expect_true(is.na(waiting$datasets_scanned))
  expect_false(any(names(.RETIRED_SUMMARY_COLS) %in% names(waiting)))
  added <- setdiff(names(waiting), cols_040)
  expect_setequal(added, names(.SUMMARY_050_COLS))
  expect_true(all(vapply(waiting[added], is.na, logical(1L))))

  # pkgB failed as a crash, and its 0.4.0 rows are still there.
  expect_identical(first$shard_failures$packages, "pkgB")
  expect_identical(.pin_kept(.package_rows(out, "pkgB"), before$pkgB),
                   .pin_kept(before$pkgB, before$pkgB))
  expect_identical(
    .pin_query(out, "SELECT package, stage, analyzer_version, timeout_failures, reason
                       FROM cran_metrics_failures"),
    data.frame(package = "pkgB", stage = "crash", analyzer_version = "0.5.2",
               timeout_failures = 1L,
               reason = paste0("analyzer exited with status 1 on a version with analyzer rows",
                               .limit_note(.memory_limit_in_force())),
               stringsAsFactors = FALSE))
  expect_true(any(grepl("FAIL pkgB: crash after [0-9.]+s \\[analyzer exit 1 x1\\]: ",
                        first$logged)))

  # The next shards take pkgC, and pkgB again until its verdict parks it.
  second <- .pin_run(io, out, shard_size = 2L)
  expect_true("dataset scans invalidated by analyzer change: 0" %in% second$messages)
  expect_identical(second$n_shard, 2L)
  expect_identical(second$n_fresh, 1L)
  expect_identical(.pin_builds(out), c(pkgA = "0.5.2", pkgB = "0.4.0", pkgC = "0.5.2"))
  for (i in seq_len(MAX_TIMEOUT_FAILURES - 2L)) {
    expect_identical(.pin_run(io, out, shard_size = 2L)$shard_failures$packages, "pkgB")
  }

  # The rescan is over: pkgB is parked under this build with its 0.4.0 rows.
  done <- .pin_run(io, out, shard_size = 2L)
  expect_identical(done$n_shard, 0L)
  expect_equal(done$permanent_failures, 1L)
  expect_true(paste("analyzer 0.5.2, output class 0.5.0 0.5.1 0.5.2;",
                    "latest rows on class: 2 of 3") %in% done$messages)
  expect_identical(.pin_kept(.package_rows(out, "pkgB"), before$pkgB),
                   .pin_kept(before$pkgB, before$pkgB))
  status <- jsonlite::fromJSON(file.path(out, "run-status.json"), simplifyVector = FALSE)
  expect_identical(status$latest_by_build, list(`0.4.0` = 1L, `0.5.2` = 2L))
  expect_identical(status$parked$timeout, 1L)
})

test_that("a move between 0.5.0, 0.5.1 and 0.5.2 re-queues nothing", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  out      <- withr::local_tempdir()
  io <- .pin_io(c(pkgA = "1.0"))
  stub <- function(build) {
    .stub_analyzer_bin(stub_dir, build, reads = c("0.0.1", "1.0"),
                       input_kind = ANALYZER_INPUT_KIND, stats = TRUE)
  }
  withr::local_envvar(RPKG_ANALYZER_BIN = stub("0.5.0"))
  expect_identical(.pin_run(io, out)$n_fresh, 1L)

  for (build in c("0.5.1", "0.5.2", "0.5.0")) {
    stub(build)
    moved <- .pin_run(io, out)
    expect_true("dataset scans invalidated by analyzer change: 0" %in% moved$messages,
                info = build)
    expect_true("packages to re-read under this analyzer: 0" %in% moved$messages, info = build)
    expect_true(sprintf("analyzer %s, output class 0.5.0 0.5.1 0.5.2; latest rows on class: 1 of 1",
                        build) %in% moved$messages, info = build)
    expect_identical(moved$n_shard, 0L, info = build)
  }
  expect_identical(.pin_builds(out), c(pkgA = "0.5.0"))
})
