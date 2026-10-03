# tests/testthat/test-version-state.R: the commit, tree and deprecation signals
# each stored version was read from, kept in cran_version_state.

# git in a fixture repository, with the identity and both dates in the
# environment, so no configuration is read or written.
.vst_git <- function(dir, ..., date = "2026-01-01T00:00:00Z") {
  suppressWarnings(system2(
    "git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = FALSE,
    env = c("GIT_AUTHOR_NAME=T", "GIT_AUTHOR_EMAIL=t@t.test",
            "GIT_COMMITTER_NAME=T", "GIT_COMMITTER_EMAIL=t@t.test",
            paste0("GIT_AUTHOR_DATE=", date), paste0("GIT_COMMITTER_DATE=", date))))
}

# What git itself resolves each revision to, one call per revision.
.vst_rev_parse <- function(repo, revs) {
  vapply(revs, function(s) .vst_git(repo, "rev-parse", shQuote(s))[[1L]],
         character(1L), USE.NAMES = FALSE)
}

# The releases of the fixture package: the date each was tagged and its R code.
# Tagged by date, so the walk's order (1.9 before 1.10) is not the tags' order.
.VST_RELEASES <- list(
  "1.0"  = list("2026-01-01T00:00:00Z", "a <- function() 1"),
  "1.9"  = list("2026-02-01T00:00:00Z",
                c("a <- function() 1", "b <- function() .Deprecated(\"a\")")),
  "1.10" = list("2026-03-01T00:00:00Z",
                c("a <- function() lifecycle::deprecate_warn(\"1.10\", \"a()\")",
                  "b <- function() .Defunct(\"b\")")),
  "2.0"  = list("2026-04-01T00:00:00Z", "z <- function() 2"))

# Commit and tag one release of `pkg` in `dir`.
.vst_release <- function(dir, pkg, ver) {
  rel <- .VST_RELEASES[[ver]]
  writeLines(c(paste("Package:", pkg), paste("Version:", ver), "Title: T",
               "Description: T.", "Author: T", "Maintainer: T <t@t.test>",
               "License: MIT"), file.path(dir, "DESCRIPTION"))
  writeLines("export(a)", file.path(dir, "NAMESPACE"))
  dir.create(file.path(dir, "R"), showWarnings = FALSE)
  writeLines(rel[[2L]], file.path(dir, "R", "code.R"))
  .vst_git(dir, "add", "-A", date = rel[[1L]])
  .vst_git(dir, "commit", "-q", "-m", shQuote(paste("version", ver)), date = rel[[1L]])
  .vst_git(dir, "tag", ver, date = rel[[1L]])
  invisible(dir)
}

# A source repository named the way clone_package finds it, <root>/<pkg>.git.
.vst_source <- function(root, pkg, versions) {
  dir <- file.path(root, paste0(pkg, ".git"))
  dir.create(dir, recursive = TRUE)
  .vst_git(dir, "init", "-q")
  for (v in versions) .vst_release(dir, pkg, v)
  dir
}

# One run_update over `latest` (package -> latest version), cloning from `root`.
.vst_run <- function(out_dir, root, latest, ...) {
  io <- list(
    package_list = function() data.frame(package = names(latest),
                                         latest_version = unname(latest),
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) clone_package(pkg, dest, base = root))
  m <- NULL
  utils::capture.output(m <- suppressMessages(suppressWarnings(
    run_update(io, out_dir, shard_size = 10L, ...))))
  m
}

# One worker, run in this process, in a work directory of its own.
.vst_local_run_env <- function(frame = parent.frame()) {
  .local_global("WORK_DIR", withr::local_tempdir(.local_envir = frame), frame = frame)
  .local_global("ANALYSIS_CORES", 1L, frame = frame)
}

.vst_query <- function(out_dir, sql, params = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql, params = params)
}

.vst_state <- function(out_dir, pkg) {
  .vst_query(out_dir, sprintf(
    'SELECT * FROM "%s" WHERE package = ? ORDER BY version', VERSION_STATE_TABLE),
    params = list(pkg))
}

.vst_summary_keys <- function(out_dir, pkg) {
  .vst_query(out_dir, sprintf(
    'SELECT package, version FROM "%s" WHERE package = ? ORDER BY version', SUMMARY_TABLE),
    params = list(pkg))
}

.VST_COLUMNS <- c(package = "TEXT", version = "TEXT", commit_sha = "TEXT",
                  tree_sha = "TEXT", prev_version = "TEXT", prev_commit = "TEXT",
                  deprecated = "TEXT", uses_lifecycle = "INTEGER", read_at = "TEXT")

.vst_expect_schema <- function(con) {
  expect_true(VERSION_STATE_TABLE %in% DBI::dbListTables(con))
  info <- DBI::dbGetQuery(con, sprintf('PRAGMA table_info("%s")', VERSION_STATE_TABLE))
  expect_identical(stats::setNames(info$type, info$name), .VST_COLUMNS)
  expect_identical(info$name[order(info$pk)][info$pk[order(info$pk)] > 0L],
                   c("package", "version"))
  sql <- DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = ?",
                         params = list(VERSION_STATE_TABLE))$sql
  expect_match(sql, "WITHOUT ROWID", fixed = TRUE)
}

# ---- The table ---------------------------------------------------------------

test_that("a new code database holds an empty cran_version_state", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(VERSION_STATE_TABLE, "cran_version_state")
  .vst_expect_schema(con)
  expect_identical(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_version_state")$n, 0L)
})

test_that("a code database written before the table existed gains it on open", {
  path <- withr::local_tempfile(fileext = ".db")
  old <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(old, SUMMARY_TABLE,
                    data.frame(package = "pkgA", version = "1.0", loc_r = 3L))
  DBI::dbExecute(old, "CREATE TABLE cran_code_churn (package TEXT, version TEXT,
                       file TEXT, added INTEGER, deleted INTEGER)")
  DBI::dbDisconnect(old)

  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .vst_expect_schema(con)
  expect_identical(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_version_state")$n, 0L)
  expect_identical(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", SUMMARY_TABLE)),
                   data.frame(package = "pkgA", version = "1.0", loc_r = 3L))
  # Opening again changes nothing.
  DBI::dbDisconnect(con)
  con <- open_or_init_db(path)
  .vst_expect_schema(con)
})

# ---- What one walk records ---------------------------------------------------

test_that("analyze_package gives every version its commit, tree, predecessor and deprecations", {
  repo <- .vst_source(withr::local_tempdir(), "vstpkg", names(.VST_RELEASES))
  .local_global("analyze_with_binary", function(dir, ...) NULL)
  # What the walk itself computed for each version.
  seen <- new.env()
  real_signals <- deprecation_signals
  .local_global("deprecation_signals", function(ctx) {
    out <- real_signals(ctx)
    assign(ctx$version, out, envir = seen)
    out
  })

  res <- suppressWarnings(analyze_package(repo, "vstpkg"))
  st  <- res$state

  expect_identical(st$version, c("1.0", "1.9", "1.10", "2.0"))
  expect_identical(st$version, list_versions(repo)$version)
  expect_identical(st$version, res$summary$version)
  expect_identical(st$package, rep("vstpkg", 4L))
  expect_identical(st$commit_sha, .vst_rev_parse(repo, st$version))
  expect_identical(st$tree_sha, .vst_rev_parse(repo, paste0(st$version, "^{tree}")))
  expect_identical(st$prev_version, c(NA, "1.0", "1.9", "1.10"))
  expect_identical(st$prev_commit, c(NA, st$commit_sha[1:3]))

  walked <- lapply(st$version, get, envir = seen)
  expect_identical(st$deprecated, vapply(walked, function(s) {
    as.character(jsonlite::toJSON(as.character(s$symbols)))
  }, character(1L)))
  expect_identical(st$uses_lifecycle,
                   vapply(walked, function(s) as.integer(s$uses_lifecycle), integer(1L)))
  expect_identical(st$deprecated, c("[]", "[\"a\"]", "[\"b\",\"a\"]", "[]"))
  expect_identical(st$uses_lifecycle, c(0L, 0L, 1L, 0L))
})

test_that("a package with no version tags records no versions", {
  repo <- withr::local_tempdir()
  .vst_git(repo, "init", "-q")
  writeLines("x", file.path(repo, "README"))
  .vst_git(repo, "add", "-A")
  .vst_git(repo, "commit", "-q", "-m", "init")
  res <- suppressWarnings(analyze_package(repo, "vstpkg"))
  expect_identical(nrow(res$state), 0L)
  expect_identical(names(res$state), setdiff(names(.VST_COLUMNS), "read_at"))
})

# ---- What a run writes -------------------------------------------------------

test_that("run_update writes one state row for every summary row, from the clone it read", {
  .vst_local_run_env()
  root <- withr::local_tempdir()
  src  <- .vst_source(root, "vstpkg", c("1.0", "1.9"))
  out  <- withr::local_tempdir()
  t0   <- format(Sys.time() - 1, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  m <- .vst_run(out, root, c(vstpkg = "1.9"))

  st <- .vst_state(out, "vstpkg")
  expect_identical(st[c("package", "version")], .vst_summary_keys(out, "vstpkg"))
  expect_identical(st$version, c("1.0", "1.9"))
  expect_identical(st$commit_sha, .vst_rev_parse(src, st$version))
  expect_identical(st$tree_sha, .vst_rev_parse(src, paste0(st$version, "^{tree}")))
  expect_identical(st$prev_version, c(NA, "1.0"))
  expect_identical(st$prev_commit, c(NA, st$commit_sha[[1L]]))
  expect_identical(st$deprecated, c("[]", "[\"a\"]"))
  expect_identical(st$uses_lifecycle, c(0L, 0L))
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", st$read_at)))
  expect_true(all(st$read_at >= t0))
  # Pipeline state, not published: no manifest counts it.
  manifest <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  expect_false(VERSION_STATE_TABLE %in% names(manifest$tables))
  expect_identical(m$n_versions, 2L)
})

test_that("a re-run replaces the package's state rows with the new walk's", {
  .vst_local_run_env()
  root <- withr::local_tempdir()
  src  <- .vst_source(root, "vstpkg", c("1.0", "1.9"))
  out  <- withr::local_tempdir()
  .vst_run(out, root, c(vstpkg = "1.9"))

  # Rows the next walk does not produce, and one it produces differently.
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  DBI::dbExecute(con, "UPDATE cran_version_state SET commit_sha = 'stale', read_at = 'then'
                       WHERE package = 'vstpkg' AND version = '1.0'")
  DBI::dbExecute(con, "INSERT INTO cran_version_state (package, version, commit_sha)
                       VALUES ('vstpkg', '0.1', 'gone')")
  DBI::dbDisconnect(con)

  .vst_release(src, "vstpkg", "1.10")
  .vst_run(out, root, c(vstpkg = "1.10"))

  st <- .vst_state(out, "vstpkg")
  st <- st[match(c("1.0", "1.9", "1.10"), st$version), ]
  expect_identical(sort(st$version), sort(.vst_summary_keys(out, "vstpkg")$version))
  expect_identical(nrow(.vst_state(out, "vstpkg")), 3L)
  expect_identical(st$commit_sha, .vst_rev_parse(src, c("1.0", "1.9", "1.10")))
  expect_identical(st$prev_version, c(NA, "1.0", "1.9"))
  expect_identical(st$prev_commit, c(NA, st$commit_sha[1:2]))
  expect_identical(st$deprecated[[3L]], "[\"b\",\"a\"]")
  expect_identical(st$uses_lifecycle[[3L]], 1L)
  expect_false(any(st$read_at == "then"))
})

test_that("a package that fails keeps its state rows, and the one beside it gets new ones", {
  .vst_local_run_env()
  root <- withr::local_tempdir()
  src  <- .vst_source(root, "vstpkg", c("1.0", "1.9"))
  .vst_source(root, "okpkg", "1.0")
  out  <- withr::local_tempdir()
  .vst_run(out, root, c(vstpkg = "1.9", okpkg = "1.0"))
  before <- .vst_state(out, "vstpkg")
  expect_identical(nrow(before), 2L)

  .vst_release(src, "vstpkg", "1.10")
  .vst_release(file.path(root, "okpkg.git"), "okpkg", "1.9")
  real_extract <- extract_version
  .local_global("extract_version", function(repo, ref, dest) {
    if (identical(ref, "1.10")) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
    real_extract(repo, ref, dest)
  })
  m <- .vst_run(out, root, c(vstpkg = "1.10", okpkg = "1.9"))

  expect_identical(m$shard_failures$packages, "vstpkg")
  expect_identical(.vst_state(out, "vstpkg"), before)
  ok <- .vst_state(out, "okpkg")
  expect_identical(ok$version, c("1.0", "1.9"))
  expect_identical(ok[c("package", "version")], .vst_summary_keys(out, "okpkg"))
})

test_that("--bootstrap starts the state table over with the summary", {
  .vst_local_run_env()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.9"))
  .vst_source(root, "okpkg", "1.0")
  out  <- withr::local_tempdir()
  .vst_run(out, root, c(vstpkg = "1.9", okpkg = "1.0"))
  expect_identical(nrow(.vst_state(out, "okpkg")), 1L)

  .vst_run(out, root, c(vstpkg = "1.9"), force_full = TRUE)

  expect_identical(nrow(.vst_state(out, "okpkg")), 0L)
  all_state <- .vst_query(out, "SELECT package, version FROM cran_version_state
                                ORDER BY package, version")
  all_summary <- .vst_query(out, sprintf("SELECT package, version FROM %s
                                          ORDER BY package, version", SUMMARY_TABLE))
  expect_identical(all_state, all_summary)
  expect_identical(all_state$package, c("vstpkg", "vstpkg"))
})

# ---- Nothing else moves --------------------------------------------------------

# Every table of every database a run writes but the state table, with its
# schema, in stored order.
.vst_other_tables <- function(out_dir) {
  out <- list()
  for (db in unique(c(DB_FILENAME, DATA_DB_FILENAME, RELEASE_TEXT_DB_FILENAME))) {
    path <- file.path(out_dir, db)
    if (!file.exists(path)) next
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    for (t in setdiff(sort(DBI::dbListTables(con)), VERSION_STATE_TABLE)) {
      out[[paste(db, t)]] <- DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s"', t))
    }
    out[[paste(db, "sqlite_master")]] <- DBI::dbGetQuery(con,
      "SELECT type, name, tbl_name, sql FROM sqlite_master WHERE tbl_name <> ? ORDER BY name",
      params = list(VERSION_STATE_TABLE))
    DBI::dbDisconnect(con)
  }
  out
}

test_that("every other table is the same whether or not the state rows are written", {
  .vst_local_run_env()
  # One clock for both runs, so the comparison can be exact.
  .local_global("Sys.time", function() as.POSIXct("2026-10-03 12:00:00", tz = "UTC"))
  .local_global("Sys.Date", function() as.Date("2026-10-03"))
  root <- withr::local_tempdir()
  src  <- .vst_source(root, "vstpkg", c("1.0", "1.9"))
  .vst_source(root, "okpkg", c("1.0", "1.9", "1.10"))

  with_state <- withr::local_tempdir()
  .vst_run(with_state, root, c(vstpkg = "1.9", okpkg = "1.10"))

  without <- withr::local_tempdir()
  real_upsert <- upsert_shard
  .local_global("upsert_shard", function(..., state_df = NULL) real_upsert(...))
  .vst_run(without, root, c(vstpkg = "1.9", okpkg = "1.10"))

  expect_identical(nrow(.vst_query(with_state, "SELECT * FROM cran_version_state")), 5L)
  expect_identical(nrow(.vst_query(without, "SELECT * FROM cran_version_state")), 0L)
  got  <- .vst_other_tables(with_state)
  want <- .vst_other_tables(without)
  expect_identical(names(got), names(want))
  for (t in names(want)) expect_identical(got[[t]], want[[t]], info = t)
})
