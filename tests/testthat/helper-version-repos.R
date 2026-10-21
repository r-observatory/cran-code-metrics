# tests/testthat/helper-version-repos.R: fixture repositories and runs shared by the version tests.

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
# Its manifest, with what it printed as the "log" attribute.
.vst_run <- function(out_dir, root, latest, shard_size = 10L, ...) {
  io <- list(
    package_list = function() data.frame(package = names(latest),
                                         latest_version = unname(latest),
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) clone_package(pkg, dest, base = root))
  m <- NULL
  log <- utils::capture.output(m <- suppressMessages(suppressWarnings(
    run_update(io, out_dir, shard_size = shard_size, ...))))
  structure(m, log = log)
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

# The stub analyzer, reading `versions`, for the calling test. Each version's
# dataset has a content of its own, so a link to the wrong one shows.
.vs_stub <- function(versions = names(.VST_RELEASES), detail = FALSE, frame = parent.frame()) {
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(withr::local_tempdir(.local_envir = frame),
                                           "0.4.0-test", reads = versions, detail = detail,
                                           content_by_version = TRUE),
    .local_envir = frame)
}
