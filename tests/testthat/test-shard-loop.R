# tests/testthat/test-shard-loop.R: when the workflow's shard loop stops, and
# what the run leaves on the Actions page. Both are shell functions in
# scripts/publish.sh, run here through bash as update.yml runs them.

.loop_script <- function() {
  normalizePath(test_path("..", "..", "scripts", "publish.sh"), mustWork = TRUE)
}

# Run `call` (a publish.sh call whose one %s is the file's path) through bash
# on a run-status.json holding `status`, or `raw` text, or no file at all.
.loop_bash <- function(call, status = NULL, raw = NULL, missing = FALSE) {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("jq")), "jq is not installed")
  path <- withr::local_tempfile(fileext = ".json")
  if (!missing) {
    if (is.null(raw)) write_manifest(path, status) else writeLines(raw, path)
  }
  out <- suppressWarnings(system2("bash", c("-c", shQuote(sprintf(
    "source %s && %s", shQuote(.loop_script()), sprintf(call, shQuote(path))))),
    stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, output = as.character(out))
}

# ---------------------------------------------------------------------------
# When the shard loop stops
# ---------------------------------------------------------------------------

.loop_done <- function(...) .loop_bash("shard_loop_done %s", ...)

.loop_status <- function(complete = FALSE, changed = TRUE, remaining = 5L, shard = 2L) {
  list(changed = changed, bootstrap_complete = complete, n_analyzed = 10L,
       n_universe = 20L, n_remaining = remaining, n_fresh = 2L, n_shard = shard)
}

test_that("the shard loop goes on while the shard changed something and work is left", {
  res <- .loop_done(.loop_status())
  expect_identical(res$status, 1L)
  expect_identical(res$output, character(0L))
})

test_that("the shard loop stops on a complete bootstrap, no change, a drained queue or an empty shard", {
  cases <- list(
    list(.loop_status(complete = TRUE),
         "Nothing left to do (complete=true, changed=true, remaining=5, shard=2)."),
    list(.loop_status(changed = FALSE),
         "Nothing left to do (complete=false, changed=false, remaining=5, shard=2)."),
    list(.loop_status(remaining = 0L),
         "Nothing left to do (complete=false, changed=true, remaining=0, shard=2)."),
    list(.loop_status(shard = 0L),
         "Nothing left to do (complete=false, changed=true, remaining=5, shard=0)."))
  for (case in cases) {
    res <- .loop_done(case[[1L]])
    expect_identical(res$status, 0L)
    expect_identical(res$output, case[[2L]])
  }
})

test_that("a run status that cannot be read stops the shard loop with a warning", {
  for (res in list(.loop_done(missing = TRUE), .loop_done(raw = "{not json"),
                   .loop_done(raw = ""))) {
    expect_identical(res$status, 0L)
    expect_true(any(grepl("^::warning::could not read ", res$output)))
  }
})

# ---------------------------------------------------------------------------
# The run's summary on the Actions page
# ---------------------------------------------------------------------------

.step_summary <- function(status, secs, start) {
  .loop_bash(sprintf("write_step_summary %%s %s %s", secs, start), status)
}

test_that("the step summary tabulates the run status and an ETA at this run's rate", {
  status <- list(
    changed = TRUE, bootstrap_complete = FALSE, n_remaining = 300L, n_shard = 400L,
    failed_this_run = 3L, failed_by_stage = list(clone = 1L, timeout = 2L),
    parked = list(fetch = 2L, analyze = 0L, timeout = 1L, legacy = 0L),
    over_cap_ok = list(count = 2L, packages = I(c("mzR", "HMP16SData"))),
    latest_by_build = list(`0.4.0` = 33000L, none = 5L))
  res <- .step_summary(status, 3600, 1500)
  expect_identical(res$status, 0L)
  expect_identical(res$output, c(
    "### Shard loop", "", "| | |", "|---|---|",
    "| Packages still queued | 300 |",
    "| Failed this run | 3 |",
    "| Failed in the last shard, by stage | clone 1, timeout 2 |",
    "| Parked | fetch 2, analyze 0, timeout 1, legacy 0 |",
    "| Standing over-cap list | 2 (mzR, HMP16SData) |",
    "| Latest rows by build | 0.4.0 33000, none 5 |",
    "| ETA at this run's rate | about 0.3 h |"))
})

test_that("the step summary says done, or n/a, when there is no rate to go on", {
  done <- .step_summary(list(n_remaining = 0L, n_shard = 0L), 60, 0)$output
  expect_identical(done[[length(done)]], "| ETA at this run's rate | done |")
  expect_true("| Parked | none |" %in% done)
  stuck <- .step_summary(list(n_remaining = 50L, n_shard = 50L), 60, 50)$output
  expect_identical(stuck[[length(stuck)]], "| ETA at this run's rate | n/a |")
})

# ---------------------------------------------------------------------------
# Starting the next run while the queue has work left
# ---------------------------------------------------------------------------

# chain_wanted on a run status whose last shard left `left` packages queued.
.chain_wanted <- function(end, start, depth, before = "", left = 300L, ...) {
  status <- if (is.na(left)) list(changed = TRUE) else .loop_status(remaining = left)
  .loop_bash(sprintf("chain_wanted %%s %s %s %s %s", shQuote(end), shQuote(start),
                     shQuote(depth), shQuote(before)), status, ...)
}

# A gh on PATH that writes its arguments to a log and exits `status`.
.chain_gh <- function(status = 0L, frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  log <- file.path(dir, "gh.log")
  writeLines(c("#!/bin/sh", sprintf('echo "$*" >> %s', shQuote(log)),
               sprintf("exit %d", as.integer(status))), file.path(dir, "gh"))
  Sys.chmod(file.path(dir, "gh"), mode = "0755")
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
                      .local_envir = frame)
  log
}

.chain_next <- function(end, start, depth, before = "", budget = "", left = 300L) {
  .loop_bash(sprintf("chain_next_run %%s %s %s %s %s %s", shQuote(end), shQuote(start),
                     shQuote(depth), shQuote(before), shQuote(budget)),
             .loop_status(remaining = left))
}

test_that("a run that ran out of time with a shrinking queue starts the next one", {
  res <- .chain_wanted("budget", 700, 0)
  expect_identical(res$status, 0L)
  expect_identical(res$output,
    "Starting the next run (chain depth 1 of 8): 300 packages left, 700 at the start of this run.")
  expect_identical(.chain_wanted("budget", 700, 3, before = "400")$status, 0L)
})

test_that("a loop that ended for any reason but the time budget starts no next run", {
  for (end in c("done", "")) {
    res <- .chain_wanted(end, 700, 0)
    expect_identical(res$status, 1L)
    expect_identical(res$output, "No next run: the shard loop did not stop on the time budget.")
  }
})

test_that("the chain stops at depth 8", {
  expect_identical(.chain_wanted("budget", 700, 7)$output,
    "Starting the next run (chain depth 8 of 8): 300 packages left, 700 at the start of this run.")
  for (depth in c("8", "9", "08")) {
    res <- .chain_wanted("budget", 700, depth)
    expect_identical(res$status, 1L)
    expect_identical(res$output,
                     sprintf("No next run: this run is number %s of a chain capped at 8.", depth))
  }
})

test_that("a queue that did not shrink this run or since the run before starts no next run", {
  res <- .chain_wanted("budget", 300, 0)
  expect_identical(res$status, 1L)
  expect_identical(res$output,
                   "No next run: the queue did not shrink this run (300 at the start, 300 left).")
  res <- .chain_wanted("budget", 700, 2, before = "300")
  expect_identical(res$status, 1L)
  expect_identical(res$output,
                   "No next run: the queue did not shrink since the run before (300 left then, 300 now).")
  res <- .chain_wanted("budget", 700, 0, left = 0L)
  expect_identical(res$status, 1L)
  expect_identical(res$output, "No next run: nothing is left in the queue.")
})

test_that("counts that are not whole numbers, or a status that cannot be read, start no next run", {
  for (args in list(list(depth = "abc", before = ""), list(depth = "-1", before = ""),
                    list(depth = "0", before = "x"), list(depth = "1e2", before = ""))) {
    res <- .chain_wanted("budget", 700, args$depth, before = args$before)
    expect_identical(res$status, 1L)
    expect_match(res$output, "^::warning::the chain's counts are not whole numbers", all = TRUE)
  }
  expect_match(.chain_wanted("budget", "", 0)$output, "^::warning::", all = TRUE)
  for (res in list(.chain_wanted("budget", 700, 0, missing = TRUE),
                   .chain_wanted("budget", 700, 0, raw = "{not json"),
                   .chain_wanted("budget", 700, 0, left = NA))) {
    expect_identical(res$status, 1L)
    expect_match(res$output, "^::warning::could not read n_remaining from ", all = TRUE)
  }
})

test_that("the next run carries the chain's counts and the time budget, and nothing else", {
  log <- .chain_gh()
  res <- .chain_next("budget", 700, 2, before = "400", budget = "7200")
  expect_identical(res$status, 0L)
  expect_identical(readLines(log), paste(
    "workflow run update.yml --ref main -f chain_depth=3 -f chain_remaining=300",
    "-f time_budget_seconds=7200"))
  unlink(log)
  .chain_next("budget", 700, 0)
  expect_identical(readLines(log),
                   "workflow run update.yml --ref main -f chain_depth=1 -f chain_remaining=300")
  # A depth written with a leading zero is still decimal.
  unlink(log)
  .chain_next("budget", 700, "07")
  expect_identical(readLines(log),
                   "workflow run update.yml --ref main -f chain_depth=8 -f chain_remaining=300")
})

test_that("no gh call is made when the chain is not wanted", {
  log <- .chain_gh()
  for (res in list(.chain_next("done", 700, 0), .chain_next("budget", 700, 8),
                   .chain_next("budget", 300, 0))) {
    expect_identical(res$status, 0L)
  }
  expect_false(file.exists(log))
})

test_that("a dispatch that fails warns and leaves the run green", {
  .chain_gh(status = 1L)
  res <- .chain_next("budget", 700, 0)
  expect_identical(res$status, 0L)
  expect_identical(res$output[[length(res$output)]],
                   "::warning::could not start the next run; the next scheduled run carries on.")
})
