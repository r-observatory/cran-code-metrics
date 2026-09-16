# Tests for the retention guard: which figures a run is allowed to publish,
# given the figures the previous release published.
#
# The numbers below are real. They are the manifests of metrics-2026-08-13 and
# metrics-2026-08-14, plus the two days the corpus actually shrank
# (cran_functions/cran_call_edges on 07-24, cran_datasets on 07-26). A guard
# that rejects any of those halts a pipeline that is working, so both
# directions are asserted throughout.

.code_manifest_0813 <- function() list(
  schema_version = 1L, series = "code", db_filename = "cran-code-metrics.db",
  db_bytes = 1256263680, fingerprint = strrep("a", 64L),
  n_packages = 33282L, n_versions = 207463L,
  tables = list(cran_code_summary = 207463L, cran_api_history = 207463L,
                cran_functions = 2445143L, cran_call_edges = 3075205L,
                cran_code_churn = 2647346L),
  bootstrap = list(n_analyzed = 33282L, n_universe = 33307L, n_remaining = 0L,
                   bootstrap_complete = TRUE))

.code_manifest_0814 <- function() list(
  schema_version = 1L, series = "code", db_filename = "cran-code-metrics.db",
  db_bytes = 1256263680, fingerprint = strrep("b", 64L),
  n_packages = 33282L, n_versions = 207464L,
  tables = list(cran_code_summary = 207464L, cran_api_history = 207464L,
                cran_functions = 2445143L, cran_call_edges = 3075205L,
                cran_code_churn = 2647355L),
  bootstrap = list(n_analyzed = 33282L, n_universe = 33307L, n_remaining = 0L,
                   bootstrap_complete = TRUE))

.data_manifest_0814 <- function() list(
  schema_version = 1L, series = "data", db_filename = "cran-data-metrics.db",
  db_bytes = 383324160, fingerprint = strrep("c", 64L),
  n_packages = 11487L, n_versions = 464302L,
  tables = list(cran_datasets = 53147L, cran_dataset_versions = 464302L,
                cran_dataset_contents = 65145L),
  bootstrap = list(n_analyzed = 33282L, n_universe = 33307L, n_remaining = 0L,
                   bootstrap_complete = TRUE))

# A 400-package first shard: what a lost prior download publishes.
.code_manifest_wiped <- function() list(
  schema_version = 1L, series = "code", db_filename = "cran-code-metrics.db",
  db_bytes = 41943040, fingerprint = strrep("d", 64L),
  n_packages = 400L, n_versions = 2489L,
  tables = list(cran_code_summary = 2489L, cran_api_history = 2489L,
                cran_functions = 31004L, cran_call_edges = 38112L,
                cran_code_churn = 29551L),
  bootstrap = list(n_analyzed = 400L, n_universe = 33307L, n_remaining = 32907L,
                   bootstrap_complete = FALSE))

# ---------------------------------------------------------------------------
# The ordinary day must pass
# ---------------------------------------------------------------------------

test_that("two consecutive real releases raise nothing", {
  expect_identical(
    retention_violations("code", .code_manifest_0814(), .code_manifest_0813()),
    character(0L))
  expect_identical(
    retention_violations("data", .data_manifest_0814(), .data_manifest_0814()),
    character(0L))
})

test_that("the largest measured one-day decreases are tolerated", {
  # 2026-07-24: cran_functions -1,283 (-0.053%) and cran_call_edges -678
  # (-0.022%), both from upsert_shard's per-package delete-then-insert.
  prev <- .code_manifest_0814()
  cur  <- prev
  cur$tables$cran_functions  <- prev$tables$cran_functions - 1283L
  cur$tables$cran_call_edges <- prev$tables$cran_call_edges - 678L
  expect_identical(retention_violations("code", cur, prev), character(0L))

  # 2026-07-26: cran_datasets -10, a package that stopped shipping a dataset.
  dprev <- .data_manifest_0814()
  dcur  <- dprev
  dcur$tables$cran_datasets <- dprev$tables$cran_datasets - 10L
  expect_identical(retention_violations("data", dcur, dprev), character(0L))

  # 2026-08-13: n_universe -1, one package leaving available.packages().
  uprev <- .code_manifest_0814()
  ucur  <- uprev
  ucur$bootstrap$n_universe <- uprev$bootstrap$n_universe - 1L
  expect_identical(retention_violations("code", ucur, uprev), character(0L))
})

# ---------------------------------------------------------------------------
# The wipe must not pass
# ---------------------------------------------------------------------------

test_that("a first-shard-from-empty code manifest is refused", {
  v <- retention_violations("code", .code_manifest_wiped(), .code_manifest_0814())
  expect_true(length(v) > 0L)
  expect_true(any(grepl("n_packages", v, fixed = TRUE)))
  expect_true(any(grepl("n_versions", v, fixed = TRUE)))
  expect_true(any(grepl("cran_functions", v, fixed = TRUE)))
  expect_true(any(grepl("db_bytes", v, fixed = TRUE)))
  # The message has to carry both numbers, or the log says only that something
  # is wrong.
  expect_true(any(grepl("33282", v, fixed = TRUE)))
  expect_true(any(grepl("400", v, fixed = TRUE)))
})

test_that("a gutted data series is refused on its own, with the code side healthy", {
  prev <- .data_manifest_0814()
  cur  <- prev
  cur$n_packages <- 400L
  cur$n_versions <- 2100L
  cur$tables <- list(cran_datasets = 1900L, cran_dataset_versions = 2100L,
                     cran_dataset_contents = 2000L)
  cur$db_bytes <- 12582912
  v <- retention_violations("data", cur, prev)
  expect_true(length(v) > 0L)
  expect_true(any(grepl("cran_dataset_versions", v, fixed = TRUE)))
  # And the code side of the same run stays clean, which is the case that has
  # no symptom today: `changed` is computed from the code fingerprint only.
  expect_identical(
    retention_violations("code", .code_manifest_0814(), .code_manifest_0813()),
    character(0L))
})

# ---------------------------------------------------------------------------
# Where each threshold sits
# ---------------------------------------------------------------------------

test_that("n_packages has no tolerance at all", {
  prev <- .code_manifest_0814()
  cur  <- prev
  cur$n_packages <- prev$n_packages - 1L
  expect_true(any(grepl("n_packages", retention_violations("code", cur, prev),
                        fixed = TRUE)))
  cur$n_packages <- prev$n_packages
  expect_identical(retention_violations("code", cur, prev), character(0L))
})

test_that("n_versions allows a whole large package's history and no more", {
  prev <- .code_manifest_0814()
  mk <- function(loss) {
    cur <- prev
    cur$n_versions <- prev$n_versions - loss
    cur$tables$cran_code_summary <- cur$n_versions
    cur$tables$cran_api_history  <- cur$n_versions
    cur
  }
  expect_identical(retention_violations("code", mk(250L), prev), character(0L))
  expect_true(any(grepl("n_versions", retention_violations("code", mk(251L), prev),
                        fixed = TRUE)))
})

test_that("the floor is the more permissive of the ratio and the row allowance", {
  # A small corpus must still get the flat 250-row allowance, or an early
  # bootstrap trips on one package.
  prev <- .code_manifest_0814()
  prev$n_versions <- 1000L
  prev$tables$cran_code_summary <- 1000L
  prev$tables$cran_api_history  <- 1000L
  cur <- prev
  cur$n_versions <- 750L
  cur$tables$cran_code_summary <- 750L
  cur$tables$cran_api_history  <- 750L
  expect_identical(retention_violations("code", cur, prev), character(0L))
})

test_that("detail tables tolerate 2 percent and refuse 5", {
  prev <- .code_manifest_0814()
  mk <- function(frac) {
    cur <- prev
    cur$tables$cran_call_edges <- as.integer(prev$tables$cran_call_edges * frac)
    cur
  }
  expect_identical(retention_violations("code", mk(0.985), prev), character(0L))
  expect_true(any(grepl("cran_call_edges", retention_violations("code", mk(0.95), prev),
                        fixed = TRUE)))
})

test_that("db_bytes refuses a shrunken file even when the counts look plausible", {
  # SQLite never shrinks a file on DELETE and nothing in the scheduled path
  # VACUUMs, so a smaller file is a different file.
  prev <- .code_manifest_0814()
  cur  <- prev
  cur$db_bytes <- prev$db_bytes * 0.5
  v <- retention_violations("code", cur, prev)
  expect_true(any(grepl("db_bytes", v, fixed = TRUE)))
  cur$db_bytes <- prev$db_bytes * 0.95
  expect_identical(retention_violations("code", cur, prev), character(0L))
})

test_that("a collapsed universe is refused", {
  # available.packages() failing leaves an archived-only universe (~8.6k of
  # 33.3k) and makes bootstrap_complete trivially true.
  prev <- .code_manifest_0814()
  cur  <- prev
  cur$bootstrap$n_universe <- 8600L
  expect_true(any(grepl("n_universe", retention_violations("code", cur, prev),
                        fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# The baseline itself
# ---------------------------------------------------------------------------

test_that("a missing baseline fails when a prior release exists and passes when none does", {
  cur <- .code_manifest_wiped()
  v <- retention_violations("code", cur, NULL, prior_tag = "metrics-2026-08-14")
  expect_true(length(v) > 0L)
  expect_true(any(grepl("metrics-2026-08-14", v, fixed = TRUE)))
  # A genuine cold start (no prior release at all) must still run.
  expect_identical(retention_violations("code", cur, NULL, prior_tag = ""),
                   character(0L))
})

test_that("a deliberate full rebuild is exempt but a failed download cannot imitate it", {
  cur  <- .code_manifest_wiped()
  prev <- .code_manifest_0814()
  expect_identical(
    retention_violations("code", cur, prev, force_full = TRUE), character(0L))
  # Same figures without the operator's flag: refused.
  expect_true(length(retention_violations("code", cur, prev)) > 0L)
})

test_that("a field the prior manifest does not carry is skipped, not treated as zero", {
  prev <- .code_manifest_0814()
  prev$tables$cran_code_churn <- NULL          # older schema
  cur  <- .code_manifest_0814()
  expect_identical(retention_violations("code", cur, prev), character(0L))
})

# ---------------------------------------------------------------------------
# Warnings: real but not worth halting a pipeline over
# ---------------------------------------------------------------------------

test_that("a summary/api count mismatch warns and does not gate", {
  cur <- .code_manifest_0814()
  cur$tables$cran_api_history <- cur$tables$cran_api_history - 5L
  expect_identical(retention_violations("code", cur, .code_manifest_0813()),
                   character(0L))
  w <- retention_warnings("code", cur)
  expect_true(any(grepl("cran_api_history", w, fixed = TRUE)))
  expect_identical(retention_warnings("code", .code_manifest_0814()), character(0L))
})

# ---------------------------------------------------------------------------
# Pre-flight: the downloaded database against the manifest that shipped with it
# ---------------------------------------------------------------------------

test_that("prior_db_violations demands exact agreement with the shipped manifest", {
  m <- .code_manifest_0814()
  expect_identical(
    prior_db_violations("code", list(n_packages = 33282L, n_versions = 207464L), m),
    character(0L))
  v <- prior_db_violations("code", list(n_packages = 0L, n_versions = 0L), m)
  expect_true(length(v) > 0L)
  expect_true(any(grepl("207464", v, fixed = TRUE)))
  # One row short is a truncated file, not a rounding difference.
  expect_true(length(prior_db_violations(
    "code", list(n_packages = 33282L, n_versions = 207463L), m)) > 0L)
  # No manifest, nothing to check.
  expect_identical(
    prior_db_violations("code", list(n_packages = 1L, n_versions = 1L), NULL),
    character(0L))
})

test_that("prior_db_violations refuses a manifest of the wrong series", {
  # The legacy single manifest.json is the code series. Comparing a data DB
  # against it would compare cran_dataset_versions to a code n_versions.
  v <- prior_db_violations("data", list(n_packages = 11487L, n_versions = 464302L),
                           .code_manifest_0814())
  expect_true(any(grepl("series", v, fixed = TRUE)))
})

test_that("preflight_prior_dbs reads the real databases and reports a truncated one", {
  out <- withr::local_tempdir()
  con <- open_or_init_db(file.path(out, DB_FILENAME))
  DBI::dbWriteTable(con, "cran_code_summary", data.frame(
    package = c("a", "a", "b"), version = c("1.0", "1.1", "2.0"),
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(con)
  m <- .code_manifest_0814()
  m$n_packages <- 2L
  m$n_versions <- 3L
  write_manifest(file.path(out, "prev-code-manifest.json"), m)
  expect_identical(preflight_prior_dbs(out)$violations, character(0L))

  # Same manifest, a database that lost its rows.
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  DBI::dbExecute(con, "DELETE FROM cran_code_summary")
  DBI::dbDisconnect(con)
  v <- preflight_prior_dbs(out)$violations
  expect_true(any(grepl("cran_code_summary", v, fixed = TRUE)))
})

test_that("preflight_prior_dbs reports a baseline manifest whose database never arrived", {
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"), .code_manifest_0814())
  v <- preflight_prior_dbs(out)$violations
  expect_true(any(grepl(DB_FILENAME, v, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Wiring: the run must actually refuse
# ---------------------------------------------------------------------------

.ret_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

#' Stand in for analyze_package with one package the analyzer read.
#'
#' Installs a stub binary first, because the row below says the analyzer read
#' this package and a machine with no analyzer cannot be the one that did. The
#' stub is never asked to read anything: analyze_package is replaced. It is
#' asked for its build, which is what the row names and what the re-scan queue
#' compares that row against, so these tests settle for the reason production
#' settles rather than because a run with no binary can invalidate nothing.
#'
#' @param frame Where the stub binary and the environment variable naming it
#'   live: the caller's test, so both are cleaned up with it.
.ret_stub_analyze <- function(env, frame = parent.frame()) {
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(
      withr::local_tempdir(.local_envir = frame), "0.4.0-test"),
    .local_envir = frame)
  old <- get("analyze_package", envir = env)
  assign("analyze_package", function(dest, pkg) list(
    # A package the analyzer read. The scan marker, the build that earned it
    # and the version named as one the binary produced arrive together,
    # because that is the only combination analyze_package can return: the
    # reader that sets the marker is the producer that names the build. The
    # build is whatever this machine's analyzer answers, which the line above
    # makes the stub, so the row is one the re-scan queue reads as current
    # rather than as collected by somebody else.
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = TRUE, detail_scanned = TRUE,
      analyzer_version = rpkg_analyzer_version(), stringsAsFactors = FALSE),
    # One api row per version row, as the real analyzer emits: a stub that
    # omitted it would trip the summary/api warning on every test here.
    api = data.frame(package = pkg, version = "1.0", exports_added = "[]",
      exports_removed = "[]", n_exports = 1L, stringsAsFactors = FALSE),
    churn = NULL, functions = NULL, edges = NULL, datasets = NULL,
    binary_versions = "1.0"),
    envir = env)
  old
}

test_that("the stand-in shard is a shard the pipeline could have collected", {
  # The converged-no-op guard further down settles because the stored row names
  # the build that is running, so the re-scan queue can show it current and
  # leaves it alone. On a machine with no analyzer the stub wrote a scanned row
  # naming nobody, and that settles for the opposite reason: nothing can be
  # shown stale when nothing can be named. analyze_package cannot produce that
  # pair there, because the reader that sets the marker is the binary whose
  # build the run then reads, so the guard was holding against a shape it never
  # has to hold against.
  withr::local_envvar(RPKG_ANALYZER_BIN = "")
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  row <- analyze_package("ignored", "pkgA")$summary
  expect_true(isTRUE(row$datasets_scanned))
  expect_false(is.na(rpkg_analyzer_version()))
  expect_identical(row$analyzer_version, rpkg_analyzer_version())
})

test_that("run_update refuses to finish a shard that would drop history", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"), .code_manifest_0814())
  write_manifest(file.path(out, "prev-data-manifest.json"), .data_manifest_0814())

  expect_error(run_update(.ret_io(), out, shard_size = 10L), "n_packages")
})

test_that("run_update refuses when a prior release exists but its manifest does not", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  withr::local_envvar(c(PREV_CODE_TAG = "metrics-2026-08-14",
                        PREV_DATA_TAG = "metrics-2026-08-14"))
  out <- withr::local_tempdir()
  expect_error(run_update(.ret_io(), out, shard_size = 10L), "metrics-2026-08-14")
})

test_that("a deliberate rebuild stays exempt after the first shard", {
  # The workflow passes --bootstrap to the first shard only, because a second
  # one would wipe what the first just collected. The exemption therefore
  # cannot ride on the flag alone, or shard 2 of the operator's own rebuild is
  # refused for holding 400 packages instead of 33,282.
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  withr::local_envvar(c(FORCE_FULL_REBUILD = "true"))
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"), .code_manifest_0814())
  write_manifest(file.path(out, "prev-data-manifest.json"), .data_manifest_0814())
  expect_no_error(run_update(.ret_io(), out, shard_size = 10L, force_full = FALSE))
})

test_that("a scheduled run cannot inherit the rebuild exemption", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  # workflow_dispatch leaves the input empty on a scheduled run, and GitHub
  # renders an unchecked box as "false".
  for (val in c("", "false")) {
    withr::local_envvar(c(FORCE_FULL_REBUILD = val))
    out <- withr::local_tempdir()
    write_manifest(file.path(out, "prev-code-manifest.json"), .code_manifest_0814())
    write_manifest(file.path(out, "prev-data-manifest.json"), .data_manifest_0814())
    expect_error(run_update(.ret_io(), out, shard_size = 10L), "n_packages")
  }
})

test_that("run_update publishes a cold start and a deliberate rebuild", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))
  out <- withr::local_tempdir()
  expect_no_error(run_update(.ret_io(), out, shard_size = 10L))

  out2 <- withr::local_tempdir()
  write_manifest(file.path(out2, "prev-code-manifest.json"), .code_manifest_0814())
  write_manifest(file.path(out2, "prev-data-manifest.json"), .data_manifest_0814())
  expect_no_error(run_update(.ret_io(), out2, shard_size = 10L, force_full = TRUE))
})

test_that("a run that reclaims free pages is not read as a run that lost rows", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  # The 64 MB threshold is a judgement about the wall clock of rewriting a
  # 1.8 GB file. The mechanism it guards is the same at 4 MB, which is a test
  # that finishes.
  orig_min <- VACUUM_MIN_RECLAIM_BYTES
  VACUUM_MIN_RECLAIM_BYTES <<- 1024^2
  on.exit(VACUUM_MIN_RECLAIM_BYTES <<- orig_min, add = TRUE)

  # Two packages, one per shard, because the reclaim only runs on a shard that
  # is going to publish: a run with nothing to report ends the loop without
  # uploading, and rewriting the database there would be work thrown away.
  io <- list(
    package_list = function() data.frame(
      package = c("pkgA", "pkgB"), latest_version = c("1.0", "1.0"),
      stringsAsFactors = FALSE),
    clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))
  out <- withr::local_tempdir()
  run_update(io, out, shard_size = 1L)   # cold start builds both DBs

  # Put the code database in the state a month of delete-and-reinsert leaves
  # it in: pages on the free list that the file is still paying for.
  db  <- file.path(out, DB_FILENAME)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE junk (payload TEXT)")
  DBI::dbAppendTable(con, "junk",
                     data.frame(payload = rep(strrep("x", 4096L), 1024L),
                                stringsAsFactors = FALSE))
  DBI::dbExecute(con, "DROP TABLE junk")
  DBI::dbDisconnect(con)
  bloated <- as.numeric(file.info(db)$size)
  expect_gt(bloated, 4 * 1024^2)

  # The baseline the workflow would have downloaded: the manifest of the
  # release that published this file, describing it at this size.
  code <- read_manifest_file(file.path(out, "code-manifest.json"))
  code$db_bytes <- bloated
  write_manifest(file.path(out, "prev-code-manifest.json"), code)
  write_manifest(file.path(out, "prev-data-manifest.json"),
                 read_manifest_file(file.path(out, "data-manifest.json")))

  expect_no_error(run_update(io, out, shard_size = 1L))

  # The space is genuinely back, and the manifest this run publishes describes
  # the file it publishes rather than the one it inherited.
  reclaimed_size <- as.numeric(file.info(db)$size)
  expect_lt(reclaimed_size, bloated / 2)
  expect_equal(read_manifest_file(file.path(out, "code-manifest.json"))$db_bytes,
               round(reclaimed_size))
  # And the baseline every later shard of this run compares against was
  # restated by exactly what came back.
  expect_equal(read_manifest_file(file.path(out, "prev-code-manifest.json"))$db_bytes,
               round(bloated - (bloated - reclaimed_size)))
})

test_that("a shard with nothing to publish does not rewrite the database", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  orig_min <- VACUUM_MIN_RECLAIM_BYTES
  VACUUM_MIN_RECLAIM_BYTES <<- 1024^2
  on.exit(VACUUM_MIN_RECLAIM_BYTES <<- orig_min, add = TRUE)

  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))
  out <- withr::local_tempdir()
  run_update(.ret_io(), out, shard_size = 10L)

  db  <- file.path(out, DB_FILENAME)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE junk (payload TEXT)")
  DBI::dbAppendTable(con, "junk",
                     data.frame(payload = rep(strrep("x", 4096L), 1024L),
                                stringsAsFactors = FALSE))
  DBI::dbExecute(con, "DROP TABLE junk")
  DBI::dbDisconnect(con)
  bloated <- as.numeric(file.info(db)$size)

  # The universe is already analysed, so this shard reports no change and the
  # workflow ends its loop without uploading. Rewriting 1.8 GB for a file
  # nobody will publish is minutes of a run's wall clock spent on nothing.
  m <- run_update(.ret_io(), out, shard_size = 10L)
  expect_false(m$changed)
  expect_equal(as.numeric(file.info(db)$size), bloated)
})

# ---------------------------------------------------------------------------
# The workflow half: the download that must not swallow its failure
# ---------------------------------------------------------------------------

test_that("update.yml fails the run when a prior asset does not arrive", {
  workflow_path <- file.path("..", "..", ".github", "workflows", "update.yml")
  yml <- readLines(workflow_path)
  dl  <- grep("gh release download", yml, value = TRUE, fixed = TRUE)
  expect_true(length(dl) > 0L)
  # Not one of the prior-state fetches may end in `|| true`: that is the line
  # that let cran-queue republish an empty database as latest.
  expect_false(any(grepl("|| true", dl, fixed = TRUE)))
  expect_false(any(grepl("2>/dev/null", dl, fixed = TRUE)))

  y <- paste(yml, collapse = "\n")
  expect_true(grepl("sleep", y, fixed = TRUE))          # retry with backoff
  expect_true(grepl("preflight.R", y, fixed = TRUE))    # content check
  expect_true(grepl("-s \"out/$name\"", y, fixed = TRUE))  # zero-length is a failure
  # The shard loop must carry the rebuild exemption for the whole run, not
  # just for the shard that gets --bootstrap.
  expect_true(grepl("FORCE_FULL_REBUILD", y, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# The publish is not atomic, and the guard must not turn that into an outage
# ---------------------------------------------------------------------------
# A same-day publish_metrics() replaces four assets, one at a time. Each one
# goes up under a temporary name and is given its own by a rename, so no reader
# meets a half-written asset under the name it asked for, but the four still
# land one after another: a 502, a dropped connection, the 350-minute job
# timeout or an operator cancel can leave a release carrying shard N's database
# next to shard N-1's manifest. A run that died between the two renames of one
# asset leaves that name on nothing, its bytes under NAME.prev, until the next
# run's repair puts the name back, and until then the release reads as carrying
# no manifest. Both states are read by every later run, because the same
# release stays `latest_tag metrics` tomorrow and the day after.

test_that("a database ahead of its manifest proceeds, and one short of it refuses", {
  m <- .code_manifest_0814()
  # Shard N's database against shard N-1's manifest: a stale record, not a
  # loss. A smaller baseline only makes the retention floor more permissive.
  ahead <- list(n_packages = m$n_packages + 3L, n_versions = m$n_versions + 412L)
  expect_identical(prior_db_violations("code", ahead, m), character(0L))
  n <- prior_db_notes("code", ahead, m)
  expect_true(length(n) > 0L)
  expect_true(any(grepl("207876", n, fixed = TRUE)))
  expect_true(any(grepl("207464", n, fixed = TRUE)))

  # One row short is still a truncated file.
  short <- list(n_packages = m$n_packages, n_versions = m$n_versions - 1L)
  expect_true(length(prior_db_violations("code", short, m)) > 0L)
  expect_identical(prior_db_notes("code", short, m), character(0L))
})

test_that("preflight_prior_dbs notes a stale manifest instead of failing the run", {
  out <- withr::local_tempdir()
  con <- open_or_init_db(file.path(out, DB_FILENAME))
  DBI::dbWriteTable(con, "cran_code_summary", data.frame(
    package = c("a", "a", "b"), version = c("1.0", "1.1", "2.0"),
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(con)
  m <- .code_manifest_0814()
  m$n_packages <- 1L
  m$n_versions <- 1L
  write_manifest(file.path(out, "prev-code-manifest.json"), m)

  res <- preflight_prior_dbs(out)
  expect_identical(res$violations, character(0L))
  expect_true(any(grepl("cran_code_summary", res$notes, fixed = TRUE)))
})

test_that("a prior release that published no manifest still yields a baseline", {
  out <- withr::local_tempdir()
  con <- open_or_init_db(file.path(out, DB_FILENAME))
  DBI::dbWriteTable(con, "cran_code_summary", data.frame(
    package = c("a", "a", "b"), version = c("1.0", "1.1", "2.0"),
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(con)

  notes <- ensure_prior_baseline(out)
  expect_true(any(grepl(DB_FILENAME, notes, fixed = TRUE)))
  m <- read_manifest_file(file.path(out, "prev-code-manifest.json"))
  expect_identical(as.character(m$series), "code")
  expect_equal(as.numeric(m$n_versions), 3)
  expect_equal(as.numeric(m$n_packages), 2)
  expect_true(as.numeric(m$db_bytes) > 0)

  # It is a real floor, not a formality.
  shrunk <- .code_manifest_0814()
  shrunk$n_packages <- 1L
  shrunk$n_versions <- 1L
  expect_true(length(retention_violations("code", shrunk, m)) > 0L)
  # And the check that runs next must not trip on the file just written.
  expect_identical(preflight_prior_dbs(out)$violations, character(0L))
})

test_that("a missing manifest with no usable database is still the wipe state", {
  out <- withr::local_tempdir()
  expect_identical(ensure_prior_baseline(out), character(0L))
  expect_false(file.exists(file.path(out, "prev-code-manifest.json")))

  # An empty database is exactly what a lost download leaves, so it is not a
  # baseline either.
  con <- open_or_init_db(file.path(out, DB_FILENAME))
  DBI::dbDisconnect(con)
  expect_identical(ensure_prior_baseline(out), character(0L))
  expect_false(file.exists(file.path(out, "prev-code-manifest.json")))

  expect_true(length(retention_violations(
    "code", .code_manifest_wiped(), NULL, prior_tag = "metrics-2026-08-14")) > 0L)
})

test_that("a published manifest is never replaced by a derived one", {
  out <- withr::local_tempdir()
  con <- open_or_init_db(file.path(out, DB_FILENAME))
  DBI::dbWriteTable(con, "cran_code_summary", data.frame(
    package = "a", version = "1.0", stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(con)
  write_manifest(file.path(out, "prev-code-manifest.json"), .code_manifest_0814())

  expect_identical(ensure_prior_baseline(out), character(0L))
  expect_equal(as.numeric(read_manifest_file(
    file.path(out, "prev-code-manifest.json"))$n_versions), 207464)
})

# ---------------------------------------------------------------------------
# The message is part of the mechanism
# ---------------------------------------------------------------------------

test_that("the refusal names a repair and does not offer force_full as one", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"), .code_manifest_0814())
  write_manifest(file.path(out, "prev-data-manifest.json"), .data_manifest_0814())

  msg <- tryCatch({ run_update(.ret_io(), out, shard_size = 10L); "" },
                  error = function(e) conditionMessage(e))
  expect_true(nzchar(msg))
  # force_full IS the wipe: it DELETEs cran_code_summary, cran_code_churn and
  # cran_api_history and republishes 400 packages as latest, and the exemption
  # then covers every later shard and every re-dispatch.
  expect_false(grepl("re-run with force_full", msg, fixed = TRUE))
  expect_true(grepl("force_full is not the repair", msg, fixed = TRUE))
  # An operator following the message has to end up somewhere better.
  expect_true(grepl("re-upload", msg, fixed = TRUE))
  expect_true(grepl("delete", msg, fixed = TRUE))
})

test_that("the repair deletes a draft beside the resolved release by id, never by tag", {
  # The 09-13 draft that wedged the pipeline carried manifests and no
  # databases, and it was never published. Resolution now skips drafts, so the
  # only draft this advice can meet shares its tag with the published release
  # the download step resolved. `gh release delete TAG` looks the tag up both
  # ways at once and deletes whichever answer arrives first, so following
  # advice to run it could take the published release and keep the draft.
  advice <- retention_repair_advice()
  expect_true(grepl("Draft", advice, fixed = TRUE))
  expect_true(grepl("never published", advice, fixed = TRUE))
  expect_true(grepl(paste0(
    "gh api 'repos/{owner}/{repo}/releases?per_page=100' --paginate ",
    "-q '.[] | select(.tag_name == \"<tag>\") | \"\\(.id) draft=\\(.draft)\"'"),
    advice, fixed = TRUE))
  expect_true(grepl("gh api -X DELETE repos/{owner}/{repo}/releases/<id>",
                    advice, fixed = TRUE))
  expect_false(grepl("gh release delete", advice, fixed = TRUE))
  expect_true(nchar(retention_refusal(c(floor = "a", ceiling = "b"))) < 8000L)
})

test_that("preflight names a database that never came back instead of calling it smaller", {
  # Every refused run after 09-13 said the prior database "holds less" than its
  # manifest, about a database that was not there at all.
  absent <- withr::local_tempdir()
  write_manifest(file.path(absent, "prev-code-manifest.json"), .code_manifest_0814())
  msg <- preflight_refusal(preflight_prior_dbs(absent)$violations)
  expect_false(grepl("holds less", msg, fixed = TRUE))
  expect_true(grepl("without the database", msg, fixed = TRUE))
  expect_true(grepl("re-upload", msg, fixed = TRUE))
  expect_true(grepl("Draft", msg, fixed = TRUE))

  # A database that did come back short keeps the headline that describes it.
  short <- withr::local_tempdir()
  con <- open_or_init_db(file.path(short, DB_FILENAME))
  DBI::dbWriteTable(con, "cran_code_summary", data.frame(
    package = "a", version = "1.0", stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(con)
  write_manifest(file.path(short, "prev-code-manifest.json"), .code_manifest_0814())
  msg <- preflight_refusal(preflight_prior_dbs(short)$violations)
  expect_true(grepl("holds less", msg, fixed = TRUE))
  expect_false(grepl("without the database", msg, fixed = TRUE))

  # Both at once gets both.
  both <- preflight_refusal(c(absent = "x", "y"))
  expect_true(grepl("without the database", both, fixed = TRUE))
  expect_true(grepl("holds less", both, fixed = TRUE))
})

test_that("preflight.R stops with the headline for what came back", {
  skip_if(!nzchar(Sys.which("Rscript")), "Rscript is not on PATH")
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"), .code_manifest_0814())
  res <- suppressWarnings(system2(
    "Rscript", c(normalizePath(file.path("..", "..", "scripts", "preflight.R")), out),
    stdout = TRUE, stderr = TRUE))
  expect_false(is.null(attr(res, "status")))
  expect_true(any(grepl("without the database", res, fixed = TRUE)))
  expect_false(any(grepl("holds less", res, fixed = TRUE)))
})

test_that("update.yml does not reach for a manifest preflight cannot read", {
  yml <- paste(readLines(file.path("..", "..", ".github", "workflows",
                                   "update.yml")), collapse = "\n")
  # The pre-split manifest.json carries no series and its n_versions is one
  # shard's row count, so preflight refused it on sight. A tag whose manifest
  # cannot be read now gets its baseline measured from the database instead.
  expect_false(grepl('have_asset "$CODE_SRC" manifest.json', yml, fixed = TRUE))
  expect_false(grepl("mv out/manifest.json", yml, fixed = TRUE))
})

test_that("the refusal survives R's error-printing limit", {
  # R prints at most getOption("warning.length") bytes of an error and drops
  # the rest. At the default 1000 the advice was cut off mid-sentence, which
  # leaves the operator with the refusal and none of the repair.
  expect_true(nchar(retention_repair_advice()) > 500L)
  expect_true(nchar(retention_failure_advice()) > 500L)
  # A run that trips both guards carries both texts, and that whole message
  # still has to arrive intact.
  expect_true(nchar(retention_refusal(c(floor = "a", ceiling = "b"))) < 8000L)
  for (f in c("update.R", "preflight.R")) {
    src <- paste(readLines(file.path("..", "..", "scripts", f)), collapse = "\n")
    expect_true(grepl("options(warning.length", src, fixed = TRUE))
  }
})

# ---------------------------------------------------------------------------
# The one count where growth is the bad direction
# ---------------------------------------------------------------------------

test_that("a burst of new failures is refused", {
  # Every other check in this file is a floor. cran_metrics_failures is the
  # count that means the opposite: it grows when packages stop being
  # analyzable, and it grew from 3 rows to 200 in a month without one guard
  # noticing.
  prev <- .code_manifest_0814()
  prev$tables$cran_metrics_failures <- 3L
  cur  <- prev
  cur$tables$cran_metrics_failures <- 200L
  v <- retention_violations("code", cur, prev)
  expect_length(v, 1L)
  expect_true(grepl("cran_metrics_failures", v, fixed = TRUE))
  expect_true(grepl("rose to 200 from 3", v, fixed = TRUE))
})

test_that("failures accumulating a few at a time, and failures clearing, both pass", {
  prev <- .code_manifest_0814()
  prev$tables$cran_metrics_failures <- 180L
  cur  <- prev

  # A shard of 400 with a handful of bad clones in it.
  cur$tables$cran_metrics_failures <- 187L
  expect_identical(retention_violations("code", cur, prev), character(0L))

  # And the direction the guard must never object to.
  cur$tables$cran_metrics_failures <- 12L
  expect_identical(retention_violations("code", cur, prev), character(0L))
})

test_that("a baseline that predates the failures count skips the ceiling", {
  # Every release published before this check existed carries no such field,
  # and reading its absence as zero would refuse the first run that saw one.
  prev <- .code_manifest_0814()          # no cran_metrics_failures in tables
  cur  <- prev
  cur$tables$cran_metrics_failures <- 5000L
  expect_identical(retention_violations("code", cur, prev), character(0L))
})

test_that("a standing pile of failures is said out loud without halting the run", {
  # The burst ceiling above compares one release to the next, so a table that
  # creeps up two packages at a time passes it every single time. The level
  # itself is the other half of the finding.
  cur <- .code_manifest_0814()
  cur$tables$cran_metrics_failures <- 200L
  expect_identical(retention_violations("code", cur, cur), character(0L))
  w <- retention_warnings("code", cur)
  expect_true(any(grepl("cran_metrics_failures", w, fixed = TRUE)))
  expect_true(any(grepl("200", w, fixed = TRUE)))

  # A handful is the ordinary state and says nothing.
  cur$tables$cran_metrics_failures <- 3L
  expect_identical(retention_warnings("code", cur), character(0L))
})

test_that("a handful of failures on a small corpus says nothing", {
  # A share on its own would make one failing package out of five a 20%
  # finding, so the test suite and any small run would warn on every shard.
  cur <- .code_manifest_0814()
  cur$bootstrap$n_universe <- 5L
  cur$tables$cran_metrics_failures <- 1L
  expect_identical(retention_warnings("code", cur), character(0L))
})

test_that("a floor and a ceiling violation say which guard produced them", {
  # The two are opposite failures reported through one vector, and the refusal
  # has to word itself for the cause. Reading them apart from the message text
  # would be guessing, so each one carries its guard kind as its name.
  prev <- .code_manifest_0814()
  prev$tables$cran_metrics_failures <- 3L

  burst <- prev
  burst$tables$cran_metrics_failures <- 250L
  expect_identical(names(retention_violations("code", burst, prev)), "ceiling")

  shrunk <- prev
  shrunk$n_packages <- 100L
  expect_identical(names(retention_violations("code", shrunk, prev)), "floor")

  # A release we could not read is the lost-download state, which is the floor
  # family however it is spelled.
  expect_identical(
    names(retention_violations("code", prev, NULL,
                               prior_tag = "metrics-2026-08-14")),
    "floor")

  expect_null(names(retention_violations("code", prev, prev)))
})

test_that("a burst of failures is not reported as history being dropped", {
  # A mirror outage in the middle of a shard fails 250 packages and drops
  # nothing: their stored rows are all still there. Told that this run "would
  # drop history the previous release carried" and handed the release-level
  # repair, an operator deletes a release that is fine while the real cause
  # goes unnamed.
  prev <- .code_manifest_0814()
  prev$tables$cran_metrics_failures <- 3L
  cur  <- prev
  cur$tables$cran_metrics_failures <- 250L

  msg <- retention_refusal(retention_violations("code", cur, prev))
  expect_false(grepl("drop history", msg, fixed = TRUE))
  expect_false(grepl("delete that release", msg, fixed = TRUE))
  expect_true(grepl("rose to 250 from 3", msg, fixed = TRUE))
  expect_true(grepl("cran_metrics_failures", msg, fixed = TRUE))
  # And it has to send the operator somewhere useful.
  expect_true(grepl("Nothing was dropped", msg, fixed = TRUE))
  expect_true(grepl("re-run", msg, fixed = TRUE))
})

test_that("a lost download still gets the headline and repair written for it", {
  prev <- .code_manifest_0814()
  cur  <- prev
  cur$n_packages <- 100L

  msg <- retention_refusal(retention_violations("code", cur, prev))
  expect_true(grepl("drop history the previous release carried", msg,
                    fixed = TRUE))
  expect_true(grepl("delete that release", msg, fixed = TRUE))
  expect_false(grepl("Nothing was dropped", msg, fixed = TRUE))
})

test_that("a run that both lost rows and failed a shard is told both", {
  prev <- .code_manifest_0814()
  prev$tables$cran_metrics_failures <- 3L
  cur  <- prev
  cur$n_packages <- 100L
  cur$tables$cran_metrics_failures <- 250L

  msg <- retention_refusal(retention_violations("code", cur, prev))
  expect_true(grepl("drop history the previous release carried", msg,
                    fixed = TRUE))
  expect_true(grepl("failed far more packages", msg, fixed = TRUE))
  expect_true(grepl("delete that release", msg, fixed = TRUE))
  expect_true(grepl("Nothing was dropped", msg, fixed = TRUE))
})

test_that("update.R refuses through the wording built for the violations", {
  # The refusal is assembled in one place so a new guard kind cannot be
  # published under the previous one's headline.
  src <- paste(readLines(file.path("..", "..", "scripts", "update.R")),
               collapse = "\n")
  expect_true(grepl("retention_refusal(violations)", src, fixed = TRUE))
  expect_false(grepl("this run would drop history the previous ", src,
                     fixed = TRUE))
})

# ---------------------------------------------------------------------------
# The ceiling measures one shard, and a run is many shards
# ---------------------------------------------------------------------------
# prev-code-manifest.json is downloaded once per run, by the step before the
# shard loop, and every shard of that run re-reads that same file. A ceiling
# read straight off it therefore measures the whole day, however it is
# calibrated: shard 5 is charged for the packages shards 1 to 4 already failed,
# published and were passed for. The re-scan behind generation 3 walks 33,000
# packages over weeks, so a day whose shards each fail a handful is the
# ordinary shape of it rather than an incident.

# A universe whose clone fails for whatever `failing()` answers at the time the
# shard asks, so a test can widen an outage between one shard and the next.
.ret_clone_io <- function(pkgs, failing) list(
  package_list = function() data.frame(
    package = pkgs, latest_version = rep("1.0", length(pkgs)),
    stringsAsFactors = FALSE),
  clone = function(pkg, dest) {
    if (pkg %in% failing()) return(FALSE)
    dir.create(dest, showWarnings = FALSE)
    TRUE
  })

# A baseline a small run can be measured against at all: every floor at or
# below what its first shard publishes, so the only guard these tests leave
# standing is the ceiling they are about.
.ret_shard_baseline <- function(failures = 0L) list(
  schema_version = 1L, series = "code", db_filename = DB_FILENAME,
  db_bytes = 4096, fingerprint = strrep("e", 64L),
  n_packages = 1L, n_versions = 1L,
  tables = list(cran_code_summary = 1L, cran_api_history = 1L,
                cran_functions = 0L, cran_call_edges = 0L,
                cran_code_churn = 0L, cran_metrics_failures = failures),
  bootstrap = list(n_analyzed = 1L, n_universe = 5L, n_remaining = 0L,
                   bootstrap_complete = FALSE))

# What the shard just published, read where the ceiling reads it.
.ret_published_failures <- function(out) as.numeric(read_manifest_file(
  file.path(out, "code-manifest.json"))$tables$cran_metrics_failures)

test_that("shards that each fail a few keep publishing past the day's ceiling", {
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"), .ret_shard_baseline())

  # Paired packages, p001a failing to clone and p001b analysing, so every
  # shard below carries collection as well as failures and is therefore a
  # shard the workflow publishes rather than one it skips.
  n    <- 130L
  pkgs <- sort(c(sprintf("p%03da", seq_len(n)), sprintf("p%03db", seq_len(n))))
  io   <- .ret_clone_io(pkgs, function() sprintf("p%03da", seq_len(n)))

  # The standing pile is said out loud from here on, which is the guard that
  # covers the creep this one deliberately does not.
  shard <- function() suppressWarnings(run_update(io, out, shard_size = 150L))

  # Shard 1: 75 of the 150 packages it took would not clone. A bad afternoon,
  # under the ceiling, published.
  expect_true(shard()$changed)
  expect_equal(.ret_published_failures(out), 75)

  # Shard 2 fails 38 packages this run had not failed before, which is well
  # under the ceiling and is what the ceiling is calibrated for. The day's
  # total has now passed it.
  expect_true(shard()$changed)
  expect_equal(.ret_published_failures(out), 113)

  # Shard 3 adds 17 more, still its own handful.
  expect_true(shard()$changed)
  expect_equal(.ret_published_failures(out), 130)

  # And a shard that newly fails nothing at all must not be refused for
  # standing where the shards before it left the table.
  expect_no_error(shard())
  expect_equal(.ret_published_failures(out), 130)

  # The level itself is not silent about any of this: it is reported every
  # shard, as a warning, which is the half of the design that catches a table
  # creeping up a few packages at a time.
  w <- retention_warnings(
    "code", read_manifest_file(file.path(out, "code-manifest.json")))
  expect_true(any(grepl("cran_metrics_failures", w, fixed = TRUE)))
})

test_that("one shard failing past the ceiling is refused however far the run got", {
  # Measuring a shard cannot become an exemption for the shards after the
  # first. A run whose second shard really does fail 130 packages at once is
  # the outage this guard exists for, and moving the baseline must not hide it.
  env <- environment(run_update)
  old <- .ret_stub_analyze(env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)

  withr::local_envvar(c(PREV_CODE_TAG = "", PREV_DATA_TAG = ""))
  out <- withr::local_tempdir()
  prev <- file.path(out, "prev-code-manifest.json")
  write_manifest(prev, .ret_shard_baseline())

  pkgs   <- sprintf("p%03d", seq_len(300L))
  outage <- pkgs[seq_len(20L)]
  io     <- .ret_clone_io(pkgs, function() outage)

  # A shard with a handful of bad clones in it, published.
  expect_true(suppressWarnings(run_update(io, out, shard_size = 150L))$changed)
  expect_equal(.ret_published_failures(out), 20)

  # Then the mirror stops answering, and the next shard fails 130 packages it
  # had never failed before.
  outage <- c(outage, pkgs[151:300])
  msg <- tryCatch({
    suppressWarnings(run_update(io, out, shard_size = 150L))
    ""
  }, error = function(e) conditionMessage(e))

  expect_true(grepl("cran_metrics_failures", msg, fixed = TRUE))
  # Named against what this shard inherited, so the log says how many packages
  # this shard failed rather than how many the day has failed.
  expect_true(grepl("rose to 150 from 20", msg, fixed = TRUE))
  expect_true(grepl("Nothing was dropped", msg, fixed = TRUE))
  # And the headline says what was compared. An operator told this shard
  # failed more than "the previous release" goes looking at a release that had
  # nothing to do with it, and the 20 in the line above came from this run.
  expect_true(grepl("than the shard before it", msg, fixed = TRUE))
  expect_false(grepl("than the previous release did", msg, fixed = TRUE))

  # And the refused shard did not move the baseline on its way out: a re-run
  # of it has to meet the same ceiling, not one raised by its own failure.
  expect_equal(
    as.numeric(read_manifest_file(prev)$tables$cran_metrics_failures), 20)
})

test_that("the ceiling baseline moves with the shard and nothing else does", {
  out  <- withr::local_tempdir()
  path <- file.path(out, "prev-code-manifest.json")
  write_manifest(path, .ret_shard_baseline(failures = 12L))

  cur <- .ret_shard_baseline(failures = 61L)
  cur$db_bytes              <- 999
  cur$n_packages            <- 400L
  cur$tables$cran_functions <- 7L
  expect_true(advance_ceiling_baseline(path, "code", cur))

  moved <- read_manifest_file(path)
  expect_equal(as.numeric(moved$tables$cran_metrics_failures), 61)
  # Every floor still measures the release the run started from. A shard that
  # loses rows is refused against yesterday's release, not against whatever
  # the shard before it happened to hold.
  expect_equal(as.numeric(moved$n_packages), 1)
  expect_equal(as.numeric(moved$db_bytes), 4096)
  expect_equal(as.numeric(moved$tables$cran_functions), 0)

  # The next shard is now measured from where this one left the table.
  nxt <- .ret_shard_baseline(failures = 61L + 101L)
  expect_true(length(retention_violations("code", nxt, moved)) > 0L)
  nxt$tables$cran_metrics_failures <- 61L + 100L
  expect_identical(retention_violations("code", nxt, moved), character(0L))
})

test_that("failures that cleared do not buy the next shard a burst", {
  # The advance runs both ways. A shard that finally analysed 150 failing
  # packages empties their rows, and a baseline left where the day started
  # would then let the shard after it fail 130 new ones without a word.
  out  <- withr::local_tempdir()
  path <- file.path(out, "prev-code-manifest.json")
  write_manifest(path, .ret_shard_baseline(failures = 200L))

  expect_true(advance_ceiling_baseline(
    path, "code", .ret_shard_baseline(failures = 50L)))
  moved <- read_manifest_file(path)
  expect_equal(as.numeric(moved$tables$cran_metrics_failures), 50)

  burst <- .ret_shard_baseline(failures = 180L)
  expect_true(length(retention_violations("code", burst, moved)) > 0L)
})

test_that("a baseline carrying no failures count is not given one mid-run", {
  # .ret_at() skips a field the baseline does not carry, which is what keeps
  # the ceiling off for a release published before the count existed. Writing
  # one in at shard 1 would switch the guard on at shard 2 of that same run,
  # measured against a number no release ever published.
  out  <- withr::local_tempdir()
  path <- file.path(out, "prev-code-manifest.json")
  write_manifest(path, .code_manifest_0814())   # no cran_metrics_failures
  expect_false(advance_ceiling_baseline(
    path, "code", .ret_shard_baseline(failures = 5000L)))
  expect_null(read_manifest_file(path)$tables$cran_metrics_failures)

  # A cold start has no baseline at all, and must not be handed one here: the
  # first release of a series is the one with nothing to compare against.
  cold <- file.path(out, "prev-data-manifest.json")
  expect_false(advance_ceiling_baseline(cold, "data", .data_manifest_0814()))
  expect_false(file.exists(cold))
})
