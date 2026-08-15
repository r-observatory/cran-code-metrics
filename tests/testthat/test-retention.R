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

.ret_stub_analyze <- function(env) {
  old <- get("analyze_package", envir = env)
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = 1L, detail_scanned = 1L,
      stringsAsFactors = FALSE),
    # One api row per version row, as the real analyzer emits: a stub that
    # omitted it would trip the summary/api warning on every test here.
    api = data.frame(package = pkg, version = "1.0", exports_added = "[]",
      exports_removed = "[]", n_exports = 1L, stringsAsFactors = FALSE),
    churn = NULL, functions = NULL, edges = NULL, datasets = NULL),
    envir = env)
  old
}

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
# publish_metrics() uploads four assets in one `gh release upload --clobber`,
# which deletes each existing asset before uploading its replacement. gh cannot
# do that atomically, so a 502, a dropped connection, the 350-minute job
# timeout or an operator cancel can leave a release carrying shard N's database
# next to shard N-1's manifest, or no manifest at all. Both states are read by
# every later run, because the same release stays `latest_tag metrics`
# tomorrow and the day after.

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
  for (f in c("update.R", "preflight.R")) {
    src <- paste(readLines(file.path("..", "..", "scripts", f)), collapse = "\n")
    expect_true(grepl("options(warning.length", src, fixed = TRUE))
  }
})
