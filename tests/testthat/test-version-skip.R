# tests/testthat/test-version-skip.R: reading only the versions whose stored rows are not current.

.vs_versions <- function(v, commits = paste0("c", seq_along(v))) {
  data.frame(version = v, ref = v, date = "2026-01-01", commit = commits, stringsAsFactors = FALSE)
}
.vs_trees <- function(v) paste0("t", seq_along(v))

# A prior as .prior_state() gives it, every stored version current.
.vs_prior <- function(v, commits = paste0("c", seq_along(v)), build = "0.5.2") {
  n <- length(v)
  newest <- seq_len(n) == n
  list(
    stored = data.frame(version = v, analyzer_version = build, n_fns_r_null = FALSE,
                        detail_scanned = ifelse(newest, 1L, NA_integer_),
                        datasets_scanned = ifelse(newest, 1L, NA_integer_), latest = newest,
                        released = "2026-01-01", dep_list = "[]", authors = "[]",
                        stringsAsFactors = FALSE),
    api = data.frame(version = v, exports_added = "[]", exports_removed = "[]",
                     n_exports = 0L, stringsAsFactors = FALSE),
    state = data.frame(version = v, commit_sha = commits, tree_sha = .vs_trees(v),
                       prev_version = c(NA, v[-n]), prev_commit = c(NA, commits[-n]),
                       deprecated = "[]", uses_lifecycle = 0L,
                       read_at = "2026-09-01T00:00:00Z", rows_generation = 1L,
                       stringsAsFactors = FALSE),
    text_ok = v, class = c("0.5.0", "0.5.1", "0.5.2"))
}
.vs_plan <- function(v, prior, commits = paste0("c", seq_along(v)), trees = .vs_trees(v), ...) {
  .read_plan(.vs_versions(v, commits), trees, prior, ...)
}
v3 <- c("1.0", "1.1", "1.2")

.vs_git <- function(dir, ...) {
  suppressWarnings(system2(
    "git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = FALSE,
    env = c("GIT_AUTHOR_NAME=T", "GIT_AUTHOR_EMAIL=t@t.test",
            "GIT_COMMITTER_NAME=T", "GIT_COMMITTER_EMAIL=t@t.test")))
}

# A repository of tagged releases whose NAMESPACE differs per tag; a NULL one has none.
.vs_repo <- function(dir, namespaces) {
  dir.create(dir, recursive = TRUE)
  .vs_git(dir, "init", "-q")
  for (v in names(namespaces)) {
    unlink(file.path(dir, "NAMESPACE"))
    writeLines(c(paste("Package: p", v), paste("Version:", v)), file.path(dir, "DESCRIPTION"))
    if (!is.null(namespaces[[v]])) writeLines(namespaces[[v]], file.path(dir, "NAMESPACE"))
    .vs_git(dir, "add", "-A")
    .vs_git(dir, "commit", "-q", "--allow-empty", "-m", shQuote(v))
    .vs_git(dir, "tag", v)
  }
  dir
}

test_that("with no prior every version is read", {
  p <- .vs_plan(c("1.0", "1.1"), NULL)
  expect_identical(p$read, c(TRUE, TRUE)); expect_identical(p$reason, c("all", "all"))
})
test_that("a current package reads nothing and redoes no diff", {
  p <- .vs_plan(v3, .vs_prior(v3))
  expect_false(any(p$read)); expect_false(any(p$pred_changed))
})
test_that("a new tag is read, and only it", {
  p <- .vs_plan(v3, .vs_prior(v3[1:2]))
  expect_identical(p$reason, c(NA, NA, "new")); expect_false(any(p$pred_changed))
})
test_that("a moved tag is read and its successor's diff is redone", {
  p <- .vs_plan(v3, .vs_prior(v3), commits = c("c1", "c2b", "c3"))
  expect_identical(p$reason, c(NA, "moved", NA)); expect_identical(p$pred_changed, c(FALSE, FALSE, TRUE))
})
test_that("a version with no state row, or no commit in it, is read as unrecorded", {
  pr <- .vs_prior(v3); pr$state <- pr$state[-1L, ]; pr$state$commit_sha[[1L]] <- NA
  expect_identical(.vs_plan(v3, pr)$reason, c("unrecorded", "unrecorded", NA))
})
test_that("an equal commit with another tree is read, and an unknown tree on either side is not", {
  pr <- .vs_prior(v3); pr$state$tree_sha[[3L]] <- NA
  expect_identical(.vs_plan(v3, pr, trees = c(NA, "tX", "t3"))$reason, c(NA, "tree", NA))
})
test_that("rows outside the output class are read, a fallback row included", {
  pr <- .vs_prior(v3); pr$stored$analyzer_version <- c("0.4.0", NA, "0.5.1")
  expect_identical(.vs_plan(v3, pr)$reason, c("class", "class", NA))
})
test_that("an older rows_generation is read, and a NULL one counts as 1", {
  pr <- .vs_prior(v3); pr$state$rows_generation <- c(NA, 0L, 1L)
  expect_identical(.vs_plan(v3, pr)$reason, c(NA, "generation", NA))
  expect_identical(.vs_plan(v3, pr, generation = 2L)$reason, rep("generation", 3L))
})
test_that("a version missing n_fns_r or its text reading is read", {
  pr <- .vs_prior(v3); pr$stored$n_fns_r_null[[1L]] <- TRUE; pr$text_ok <- c("1.0", "1.2")
  expect_identical(.vs_plan(v3, pr)$reason, c("metrics", "text", NA))
})
test_that("a text reading is asked only of rows a 0.5.0 or later build wrote", {
  pr <- .vs_prior(v3, build = "0.4.0-test"); pr$class <- "0.4.0-test"; pr$text_ok <- character(0L)
  expect_false(any(.vs_plan(v3, pr)$read))
})
test_that("a missing detail marker reads the newest, where it lives, and no older version", {
  pr <- .vs_prior(v3); pr$stored$detail_scanned <- NA_integer_
  expect_identical(.vs_plan(v3, pr)$reason, c(NA, NA, "detail"))
})
test_that("the newest is read when it is not the stored newest or has lost its dataset marker", {
  v2 <- c("1.0", "1.1")
  pr <- .vs_prior(v2); pr$stored$datasets_scanned[[2L]] <- NA
  expect_identical(.vs_plan(v2, pr)$reason, c(NA, "newest"))
  pr <- .vs_prior(v3); pr$stored$detail_scanned <- 1L; pr$stored$datasets_scanned <- 1L   # 1.2 is gone, so 1.1 is the newest again
  expect_identical(.vs_plan(v2, pr)$reason, c(NA, "newest"))
})
test_that("a tag inserted before a stored one redoes the stored one's diff", {
  p <- .vs_plan(v3, .vs_prior(c("1.0", "1.2")), commits = c("c1", "cx", "c2"),
                trees = c("t1", "tx", "t2"))
  expect_identical(p$reason, c(NA, "new", NA)); expect_identical(p$pred_changed, c(FALSE, FALSE, TRUE))
})
test_that("a skipped version with no stored API row has its diff redone", {
  pr <- .vs_prior(v3[1:2]); pr$api <- pr$api[1L, ]
  p <- .vs_plan(v3[1:2], pr)
  expect_false(any(p$read)); expect_identical(p$pred_changed, c(FALSE, TRUE))
})
test_that("the first rule a version fails names its reason", {
  pr <- .vs_prior(v3); pr$stored$analyzer_version[[2L]] <- "0.4.0"
  expect_identical(.vs_plan(v3, pr, commits = c("c1", "cx", "c3"))$reason[[2L]], "moved")
})
test_that("an API row is the walk's: exports added and removed against the version before", {
  expect_identical(.api_row("p", "1.1", c("a", "c"), c("a", "b")),
                   data.frame(package = "p", version = "1.1", exports_added = '["c"]',
                              exports_removed = '["b"]', n_exports = 2L, stringsAsFactors = FALSE))
  expect_identical(.api_row("p", "1.0", "a", NULL)$exports_added, '["a"]')
  expect_identical(.api_row("p", "1.0", character(0L), NULL)$exports_added, "[]")
})
test_that("a version's exports read alone are the ones a full extraction gives", {
  src <- .vs_repo(file.path(withr::local_tempdir(), "src"),
                  list("1.0" = "export(a)", "1.1" = c("export(a)", "export(b)"),
                       "1.2" = c("exportPattern(\"^z\")", "export(c)")))
  for (ver in c("1.0", "1.1", "1.2")) {
    tmp <- withr::local_tempdir()
    files <- extract_version(src, ver, tmp)
    ctx <- build_context("p", ver, ver, "2026-01-01", files,
                         function(path) paste(readLines(file.path(tmp, path), warn = FALSE), collapse = "\n"))
    full <- ctx$namespace$exports %||% character(0L)
    expect_true(length(full) > 0L)
    expect_identical(.exports_at(src, ver, "p", ver), full)
  }
})
test_that("a version without a NAMESPACE exports nothing", {
  repo <- .vs_repo(file.path(withr::local_tempdir(), "src"), list("0.1" = NULL))
  expect_identical(.exports_at(repo, "0.1", "p", "0.1"), character(0L))
})
test_that("a ref git cannot read fails as an extraction does", {
  src <- .vs_repo(file.path(withr::local_tempdir(), "src"), list("1.0" = "export(a)"))
  expect_error(.exports_at(src, "no-such-ref", "p", "x"), class = "extract_failure")
})

# ---- The stored state a plan reads ------------------------------------------

# A code database and a text database in a fresh directory, both open.
.vs_dbs <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  con <- open_or_init_db(file.path(dir, DB_FILENAME))
  text_con <- open_or_init_release_text_db(file.path(dir, RELEASE_TEXT_DB_FILENAME))
  withr::defer({ DBI::dbDisconnect(con); DBI::dbDisconnect(text_con) }, envir = frame)
  list(con = con, text_con = text_con)
}

test_that("the prior holds a package's stored rows, API rows, state rows and text readings", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  out <- withr::local_tempdir()
  .vst_run(out, root, c(vstpkg = "1.10"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  text_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, RELEASE_TEXT_DB_FILENAME))
  withr::defer({ DBI::dbDisconnect(con); DBI::dbDisconnect(text_con) })
  p <- .prior_state(con, text_con, "vstpkg", "0.4.0-test")[["vstpkg"]]
  o <- order(numeric_version(p$stored$version))
  expect_identical(p$stored$version[o], c("1.0", "1.9", "1.10"))
  expect_identical(as.logical(p$stored$latest[o]), c(FALSE, FALSE, TRUE))
  expect_false(any(as.logical(p$stored$n_fns_r_null)))
  expect_identical(sort(p$state$commit_sha), sort(.vst_rev_parse(src, c("1.0", "1.9", "1.10"))))
  expect_identical(p$state$rows_generation, rep(1L, 3L))
  expect_identical(sort(p$api$version), sort(c("1.0", "1.9", "1.10")))
  expect_identical(p$class, "0.4.0-test")
  lv <- list_versions(src)
  expect_false(any(.read_plan(lv, .version_trees(src, lv$commit), p)$read))
})
test_that("a package with nothing stored gets an empty prior, and every version reads as new", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.9"))
  out <- withr::local_tempdir()
  .vst_run(out, root, c(vstpkg = "1.9"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  text_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, RELEASE_TEXT_DB_FILENAME))
  withr::defer({ DBI::dbDisconnect(con); DBI::dbDisconnect(text_con) })
  prior <- .prior_state(con, text_con, c("vstpkg", "newpkg"), "0.4.0-test")
  expect_named(prior, c("vstpkg", "newpkg"))
  e <- prior[["newpkg"]]
  expect_identical(vapply(e[c("stored", "api", "state")], nrow, integer(1L)),
                   c(stored = 0L, api = 0L, state = 0L))
  expect_identical(names(e$state), setdiff(.VERSION_STATE_COLS, "package"))
  expect_identical(e$text_ok, character(0L))
  lv <- list_versions(src)
  expect_identical(.read_plan(lv, .version_trees(src, lv$commit), e)$reason,
                   rep("new", nrow(lv)))
})
test_that("a summary column the database lacks reads as NULL", {
  d <- .vs_dbs()
  DBI::dbWriteTable(d$con, SUMMARY_TABLE, data.frame(
    package = "p", version = c("1.0", "1.1"), analyzer_version = "0.5.2",
    released = "2026-01-01", stringsAsFactors = FALSE))
  p <- .prior_state(d$con, d$text_con, "p", "0.5.2")[["p"]]
  expect_identical(p$stored$version, c("1.0", "1.1"))
  expect_identical(p$stored$released, c("2026-01-01", "2026-01-01"))
  expect_true(all(is.na(p$stored$detail_scanned)))
  expect_true(all(is.na(p$stored$datasets_scanned)))
  expect_false(any(p$stored$latest))
  expect_true(all(as.logical(p$stored$n_fns_r_null)))
  # With state rows matching the listing, the metrics rule is the first one they fail.
  DBI::dbWriteTable(d$con, VERSION_STATE_TABLE, data.frame(
    package = "p", version = c("1.0", "1.1"), commit_sha = c("c1", "c2"),
    tree_sha = c("t1", "t2"), rows_generation = 1L, stringsAsFactors = FALSE), append = TRUE)
  p <- .prior_state(d$con, d$text_con, "p", "0.5.2")[["p"]]
  lv <- .vs_versions(c("1.0", "1.1"))
  expect_identical(.read_plan(lv, .vs_trees(lv$version), p)$reason, c("metrics", "metrics"))
})
test_that("only text readings by 0.5.0 or later count", {
  d <- .vs_dbs()
  DBI::dbAppendTable(d$text_con, RELEASE_TEXT_VERSIONS_TABLE, data.frame(
    package = "p", version = c("1.0", "1.1"), analyzer_version = c("0.4.0", "0.5.2"),
    n_fields = 5L, has_release_notes = 0L, read_at = "2026-09-01T00:00:00Z",
    stringsAsFactors = FALSE))
  p <- .prior_state(d$con, d$text_con, "p", "0.5.2")[["p"]]
  expect_identical(p$text_ok, "1.1")
})
test_that("the prior is the same whatever the chunk size", {
  d <- .vs_dbs()
  for (k in c("a", "b", "c")) {
    DBI::dbWriteTable(d$con, SUMMARY_TABLE, data.frame(
      package = k, version = c("1.0", "1.1"), analyzer_version = "0.5.2",
      stringsAsFactors = FALSE), append = (k != "a"))
  }
  DBI::dbAppendTable(d$con, VERSION_STATE_TABLE, data.frame(
    package = "b", version = "1.1", commit_sha = "c1", stringsAsFactors = FALSE))
  DBI::dbAppendTable(d$text_con, RELEASE_TEXT_VERSIONS_TABLE, data.frame(
    package = "c", version = "1.0", analyzer_version = "0.5.2", n_fields = 5L,
    stringsAsFactors = FALSE))
  expect_identical(.prior_state(d$con, d$text_con, c("a", "b", "c", "d"), "0.5.2", chunk = 1L),
                   .prior_state(d$con, d$text_con, c("a", "b", "c", "d"), "0.5.2"))
})

# ---- A walk with a prior ------------------------------------------------------

# One run over `versions` under the stub, and the prior it leaves.
.vs_walked <- function(versions = c("1.0", "1.9", "1.10"), frame = parent.frame()) {
  .vst_local_run_env(frame); .vs_stub(frame = frame)
  root <- withr::local_tempdir(.local_envir = frame)
  src  <- .vst_source(root, "vstpkg", versions)
  out  <- withr::local_tempdir(.local_envir = frame)
  .vst_run(out, root, c(vstpkg = versions[[length(versions)]]))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  text_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, RELEASE_TEXT_DB_FILENAME))
  on.exit({ DBI::dbDisconnect(con); DBI::dbDisconnect(text_con) })
  list(root = root, src = src, prior = .prior_state(con, text_con, "vstpkg", "0.4.0-test")[["vstpkg"]])
}

# A fresh clone of the fixture's source, as the next run makes it.
.vs_clone <- function(w, frame = parent.frame()) {
  dest <- file.path(withr::local_tempdir(.local_envir = frame), "vstpkg")
  stopifnot(isTRUE(clone_package("vstpkg", dest, base = w$root)))
  dest
}

# Re-point tag 1.9 at a commit dated 2026-02-02 whose NAMESPACE adds export(b).
.vs_move_1_9 <- function(src) {
  .vst_git(src, "checkout", "-q", "-b", "moved", "1.9")
  writeLines(c("export(a)", "export(b)"), file.path(src, "NAMESPACE"))
  .vst_git(src, "commit", "-q", "-am", shQuote("move 1.9"), date = "2026-02-02T00:00:00Z")
  .vst_git(src, "tag", "-f", "1.9", date = "2026-02-02T00:00:00Z")
  .vst_git(src, "checkout", "-q", "-")
  invisible(src)
}

.vs_no_rownames <- function(df) {
  rownames(df) <- NULL
  df
}

test_that("a new tag is read alone, and the rest come back as a full walk's derived columns", {
  w <- .vs_walked(); p <- w$prior
  .vst_release(w$src, "vstpkg", "2.0")
  clone <- .vs_clone(w)
  res  <- analyze_package(clone, "vstpkg", prior = p)
  full <- analyze_package(clone, "vstpkg")
  expect_true(res$skip$partial); expect_false(res$skip$unchanged)
  expect_identical(res$summary$version, "2.0")
  expect_identical(res$skip$row_keys, "2.0"); expect_identical(res$skip$api_keys, "2.0")
  expect_identical(names(res$skip$derived), c("package", "version", .CROSS_VERSION_COLS))
  expect_identical(res$skip$derived$version, c("1.0", "1.9", "1.10"))
  k <- match(res$skip$derived$version, full$summary$version)
  expect_identical(.vs_no_rownames(res$skip$derived[, .CROSS_VERSION_COLS]),
                   .vs_no_rownames(full$summary[k, .CROSS_VERSION_COLS]))
  expect_identical(.vs_no_rownames(res$summary),
                   .vs_no_rownames(full$summary[full$summary$version == "2.0", ]))
  expect_identical(res$state$version, "2.0")
  expect_identical(res$state$rows_generation, ROWS_GENERATION)
})
test_that("a moved tag is read, and its successor's diff is redone from the two NAMESPACE files", {
  w <- .vs_walked(); p <- w$prior; src <- w$src
  .vs_move_1_9(src)
  clone <- .vs_clone(w)
  seen <- new.env(); seen$v <- character(0L)
  real <- .exports_at
  .local_global(".exports_at", function(repo, ref, package, version) {
    seen$v <- c(seen$v, version)
    real(repo, ref, package, version)
  })
  res  <- analyze_package(clone, "vstpkg", prior = p)
  full <- analyze_package(clone, "vstpkg")
  expect_identical(res$skip$plan$reason, c(NA, "moved", NA))
  expect_identical(res$summary$version, "1.9")
  # 1.0's exports for the diff of 1.9 read after it, then 1.10's for its own.
  expect_identical(seen$v, c("1.0", "1.10"))
  expect_setequal(res$skip$api_keys, c("1.9", "1.10"))
  expect_identical(res$skip$row_keys, "1.9")
  expect_identical(.vs_no_rownames(res$api[res$api$version == "1.10", ]),
                   .vs_no_rownames(full$api[full$api$version == "1.10", ]))
  expect_identical(.vs_no_rownames(res$api),
                   .vs_no_rownames(full$api[full$api$version %in% c("1.9", "1.10"), ]))
  expect_identical(res$api$exports_removed[res$api$version == "1.10"], '["b"]')
  st <- res$state[res$state$version == "1.10", ]
  expect_identical(st$prev_commit, .vst_rev_parse(src, "1.9"))
  expect_identical(st$read_at, p$state$read_at[p$state$version == "1.10"])
  expect_identical(st$commit_sha, p$state$commit_sha[p$state$version == "1.10"])
})
test_that("a package with nothing to read comes back unchanged", {
  w <- .vs_walked()
  res <- analyze_package(.vs_clone(w), "vstpkg", prior = w$prior)
  expect_true(res$skip$unchanged); expect_false(res$skip$partial)
  expect_false(any(res$skip$plan$read))
  expect_identical(nrow(res$summary), 0L)
  expect_identical(vapply(res[c("churn", "api", "state", "datasets", "vignettes")], nrow, integer(1L)),
                   c(churn = 0L, api = 0L, state = 0L, datasets = 0L, vignettes = 0L))
  expect_identical(res$skip$row_keys, character(0L)); expect_identical(res$skip$api_keys, character(0L))
})
test_that("an empty listing deletes nothing", {
  repo <- withr::local_tempdir()
  .vst_git(repo, "init", "-q")
  writeLines("x", file.path(repo, "README"))
  .vst_git(repo, "add", "-A")
  .vst_git(repo, "commit", "-q", "-m", "init")
  res <- analyze_package(repo, "vstpkg", prior = .vs_prior(c("1.0", "1.1")))
  expect_identical(res$skip$row_keys, character(0L)); expect_true(res$skip$unchanged)
  expect_identical(res$skip$api_keys, character(0L))
  expect_identical(res$skip$newest, NA_character_)
  expect_identical(names(res$skip$derived), c("package", "version", .CROSS_VERSION_COLS))
})
test_that("a tag gone from the listing is deleted with its rows", {
  w <- .vs_walked()
  .vst_git(w$src, "tag", "-d", "1.0")
  res <- analyze_package(.vs_clone(w), "vstpkg", prior = w$prior)
  expect_true("1.0" %in% res$skip$row_keys); expect_true("1.0" %in% res$skip$api_keys)
  expect_false(res$skip$unchanged)
  expect_identical(res$skip$plan$version, c("1.9", "1.10"))
})
test_that("a redone diff after a version skipped with its stored diff reads that version's exports first", {
  w <- .vs_walked()
  .vst_git(w$src, "tag", "-d", "1.9")
  clone <- .vs_clone(w)
  seen <- new.env(); seen$v <- character(0L)
  real <- .exports_at
  .local_global(".exports_at", function(repo, ref, package, version) {
    seen$v <- c(seen$v, version)
    real(repo, ref, package, version)
  })
  res  <- analyze_package(clone, "vstpkg", prior = w$prior)
  full <- analyze_package(clone, "vstpkg")
  expect_identical(res$skip$plan$pred_changed, c(FALSE, TRUE))
  expect_identical(nrow(res$summary), 0L)
  expect_identical(seen$v, c("1.0", "1.10"))
  expect_identical(.vs_no_rownames(res$api), .vs_no_rownames(full$api[full$api$version == "1.10", ]))
  expect_identical(res$state$prev_version, "1.0")
  expect_identical(res$skip$row_keys, "1.9"); expect_setequal(res$skip$api_keys, c("1.9", "1.10"))
})
test_that("without a prior the result has no skip part and every version is read", {
  .vs_stub()
  src <- .vst_source(withr::local_tempdir(), "vstpkg", c("1.0", "1.9", "1.10"))
  res <- analyze_package(src, "vstpkg")
  expect_null(res$skip); expect_false("skip" %in% names(res))
  expect_identical(nrow(res$summary), 3L)
  expect_identical(res$binary_versions, c("1.0", "1.9", "1.10"))
})
test_that("the read marker is the stored newest's dataset marker when the newest is skipped", {
  w <- .vs_walked()
  .vs_move_1_9(w$src)
  clone <- .vs_clone(w)
  # The stub no longer reads 1.10, so a read of it would leave no dataset marker.
  .vs_stub(c("1.0", "1.9"))
  full <- suppressWarnings(analyze_package(clone, "vstpkg"))
  expect_false(isTRUE(full$summary$datasets_scanned[[3L]]))
  res <- analyze_package(clone, "vstpkg", prior = w$prior)
  expect_identical(res$skip$newest, "1.10")
  expect_false(res$skip$newest_read); expect_true(res$skip$read_marker)
})
test_that("assessed_at and assessed_with are not among the columns written back", {
  w <- .vs_walked()
  .vst_release(w$src, "vstpkg", "2.0")
  res <- analyze_package(.vs_clone(w), "vstpkg", prior = w$prior)
  expect_identical(nrow(res$skip$derived), 3L)
  expect_false(any(c("assessed_at", "assessed_with") %in% names(res$skip$derived)))
})
test_that("functions and edges come only from a read newest", {
  w <- .vs_walked()
  .vs_move_1_9(w$src)
  clone <- .vs_clone(w)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.4.0-test", reads = names(.VST_RELEASES), detail = TRUE))
  full <- analyze_package(clone, "vstpkg")
  expect_identical(full$functions$version, "1.10"); expect_identical(full$edges$version, "1.10")
  res <- analyze_package(clone, "vstpkg", prior = w$prior)
  expect_identical(res$summary$version, "1.9"); expect_false(res$skip$newest_read)
  expect_identical(nrow(res$functions), 0L); expect_identical(nrow(res$edges), 0L)
})

# ---- run_update with the version skip ------------------------------------------

test_that("the version skip is on unless VERSION_SKIP is 0", {
  expect_true(.version_skip_on("")); expect_true(.version_skip_on("1"))
  expect_false(.version_skip_on("0")); expect_false(.version_skip_on(" 0 "))
})
test_that("a skip run after a new tag writes the rows a full walk writes", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  skip <- withr::local_tempdir(); full <- withr::local_tempdir()
  .vs_run(skip, root, c(vstpkg = "1.10"))
  .vst_release(src, "vstpkg", "2.0")
  .vs_run(skip, root, c(vstpkg = "2.0"))
  .vs_run(full, root, c(vstpkg = "2.0"), skip = FALSE)
  expect_identical(.vs_rows(skip, "vstpkg"), .vs_rows(full, "vstpkg"))
  rs <- jsonlite::fromJSON(file.path(skip, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(as.integer(rs$n_versions), 1L)
})
test_that("a package queued with nothing to read is ok, not fresh, and does not publish", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.10"))
  before <- .package_rows(out, "vstpkg")
  # Stored up to 1.10; the universe names 2.0, which the mirror does not have yet.
  .vs_run(out, root, c(vstpkg = "2.0"))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(c(rs$n_shard, rs$n_fresh, rs$shard_failures), c(1L, 0L, 0L))
  expect_false(rs$changed)
  expect_identical(.package_rows(out, "vstpkg"), before)
})
test_that("a requeued package that lands in a later shard still reads every version", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  apkg <- .vst_source(root, "apkg", c("1.0", "1.9", "1.10"))
  .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(apkg = "1.10", vstpkg = "1.10"))
  # apkg and vstpkg stored; apkg gains a tag so both are queued, apkg sorting first.
  .vst_release(apkg, "apkg", "2.0")
  latest <- c(apkg = "2.0", vstpkg = "1.10")
  .vs_run(out, root, latest, requeue = "vstpkg", shard_size = 1L)   # walks apkg
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  .vs_run(out, root, latest, shard_size = 1L)                        # walks vstpkg, no flag
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 3L)
})
test_that("--bootstrap and VERSION_SKIP off read every version and give a full walk's rows", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  boot <- withr::local_tempdir(); off <- withr::local_tempdir(); full <- withr::local_tempdir()
  .vs_run(boot, root, c(vstpkg = "1.10")); .vs_run(off, root, c(vstpkg = "1.10"))
  .vst_release(src, "vstpkg", "2.0")
  .vs_run(boot, root, c(vstpkg = "2.0"), force_full = TRUE)
  .vs_run(off, root, c(vstpkg = "2.0"), skip = FALSE)
  .vs_run(full, root, c(vstpkg = "2.0"), skip = FALSE)
  rs_boot <- jsonlite::fromJSON(file.path(boot, "run-status.json"))
  rs_off  <- jsonlite::fromJSON(file.path(off, "run-status.json"))
  expect_identical(as.integer(rs_boot$worker_phases$versions_read), 4L)
  expect_identical(as.integer(rs_off$worker_phases$versions_read), 4L)
  expect_identical(.vs_rows(boot, "vstpkg"), .vs_rows(full, "vstpkg"))
  expect_identical(.vs_rows(off, "vstpkg"), .vs_rows(full, "vstpkg"))
  expect_identical(unique(.vst_state(off, "vstpkg")$rows_generation), ROWS_GENERATION)
})
test_that("a shard that walks one package whole and skips versions of another writes both as full walks would", {
  .vst_local_run_env()
  # Detail records too, so both packages have latest text.
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_analyzer_bin(
    withr::local_tempdir(), "0.4.0-test", reads = names(.VST_RELEASES), detail = TRUE))
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  skip <- withr::local_tempdir(); full <- withr::local_tempdir()
  .vs_run(skip, root, c(vstpkg = "1.10"))
  # newpkg is new and read whole; vstpkg gains 2.0 and reads it alone.
  .vst_source(root, "newpkg", c("1.0", "1.9"))
  .vst_release(src, "vstpkg", "2.0")
  latest <- c(newpkg = "1.9", vstpkg = "2.0")
  .vs_run(skip, root, latest)
  .vs_run(full, root, latest, skip = FALSE)
  rs <- jsonlite::fromJSON(file.path(skip, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 3L)
  for (p in c("newpkg", "vstpkg")) {
    rows <- .vs_rows(skip, p)
    expect_gt(nrow(rows[[paste(DB_FILENAME, DESCRIPTION_FIELDS_TABLE)]]), 0L)
    expect_identical(rows, .vs_rows(full, p), info = p)
  }
})
test_that("a package whose one read version fails keeps every stored row and reads it again next run", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.10"))
  before <- .package_rows(out, "vstpkg")
  .vst_release(src, "vstpkg", "2.0")
  # extract_version fails on ref 2.0 for one run
  real <- extract_version
  local({
    .local_global("extract_version", function(repo, ref, dest, ...) {
      if (identical(ref, "2.0")) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
      real(repo, ref, dest, ...)
    })
    .vs_run(out, root, c(vstpkg = "2.0"))
  })
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$shard_failures), 1L)
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(.package_rows(out, "vstpkg"), before)
  # the mock removed, the next run reads 2.0 alone
  .vs_run(out, root, c(vstpkg = "2.0"))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$shard_failures), 0L)
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(.vst_summary_keys(out, "vstpkg")$version, sort(c("1.0", "1.9", "1.10", "2.0")))
})
test_that("a skipped row keeps its assessed_at and assessed_with", {
  .vst_local_run_env(); .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.9", "1.10"))
  out <- withr::local_tempdir()
  # Sys.Date() is 2026-10-01 for the first run and 2026-10-20 for the second,
  # and the first runs under another R.
  local({
    .local_global("Sys.Date", function() as.Date("2026-10-01"))
    .local_global("getRversion", function() numeric_version("4.5.0"))
    .vs_run(out, root, c(vstpkg = "1.10"))
  })
  .vst_release(src, "vstpkg", "2.0")
  local({
    .local_global("Sys.Date", function() as.Date("2026-10-20"))
    .vs_run(out, root, c(vstpkg = "2.0"))
  })
  got <- .vst_query(out, paste("SELECT version, assessed_at, assessed_with FROM cran_code_summary",
                               "WHERE package = 'vstpkg'"))
  k <- match(c("1.0", "1.9", "1.10", "2.0"), got$version)
  expect_identical(got$assessed_at[k], c(rep("2026-10-01", 3L), "2026-10-20"))
  expect_identical(got$assessed_with[k],
                   c(rep("cran-code-metrics; R 4.5.0", 3L),
                     paste0("cran-code-metrics; R ", getRversion())))
})
