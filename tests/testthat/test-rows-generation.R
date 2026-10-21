# tests/testthat/test-rows-generation.R: the rows R writes for one version, held to
# a golden digest, and the cross-version columns held to the inputs a skipped
# version carries. The fixture package and the digest below make the golden, so
# a change to either regenerates it.

.rg_golden <- function() {
  jsonlite::read_json(test_path("fixtures", "rows-golden.json"), simplifyVector = TRUE)
}

# The fix a digest that moved asks for: a bump, unless the golden is a generation behind.
.rg_fix <- function(g) {
  if (identical(as.integer(g$rows_generation), ROWS_GENERATION)) {
    "rows changed: bump ROWS_GENERATION in scripts/config.R and regenerate the golden"
  } else {
    "ROWS_GENERATION changed: regenerate tests/testthat/fixtures/rows-golden.json in the same pull request"
  }
}

# Git with no global or system config, so a developer's config cannot change the fixture.
.rg_git <- function(dir, date, ...) {
  err <- tempfile("rg_git_")
  on.exit(unlink(err), add = TRUE)
  out <- suppressWarnings(system2(
    "git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = err,
    env = c("GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_NOSYSTEM=1",
            "GIT_AUTHOR_NAME=T", "GIT_AUTHOR_EMAIL=t@t.test",
            "GIT_COMMITTER_NAME=T", "GIT_COMMITTER_EMAIL=t@t.test",
            paste0("GIT_AUTHOR_DATE=", date), paste0("GIT_COMMITTER_DATE=", date))))
  status <- attr(out, "status") %||% 0L
  if (status != 0L) {
    stop(sprintf("rows golden fixture: git %s exited with status %d: %s",
                 ..1, status, paste(readLines(err, warn = FALSE), collapse = " ")),
         call. = FALSE)
  }
  out
}

.RG_ADA   <- 'person("Ada", "Lovelace", email = "ada@example.org", role = c("aut", "cre"))'
.RG_GRACE <- 'person("Grace", "Hopper", email = "grace@example.org", role = "aut")'
.RG_ALAN  <- 'person("Alan", "Turing", role = "ctb")'

# Each release: its date, the DESCRIPTION fields that move, its exports, its R
# code and the committed data file it ships as data/x.rda.
.RG_RELEASES <- list(
  "1.0" = list(
    date = "2026-01-01T00:00:00Z",
    desc = c(paste0("Authors@R: ", .RG_ADA),
             "Author: Ada Lovelace [aut, cre]",
             "Maintainer: Ada Lovelace <ada@example.org>",
             "Depends: R (>= 3.5.0)", "Imports: stats", "Suggests: testthat"),
    exports = c("add_one", "abs_val"),
    code = c("add_one <- function(x) x + 1",
             "abs_val <- function(x) if (x < 0) -x else x"),
    data = "x1.rda"),
  "1.1" = list(
    date = "2026-02-01T00:00:00Z",
    desc = c(paste0("Authors@R: c(", .RG_ADA, ", ", .RG_GRACE, ")"),
             "Author: Ada Lovelace [aut, cre], Grace Hopper [aut]",
             "Maintainer: Ada Lovelace <ada@example.org>",
             "Depends: R (>= 3.5.0), methods", "Imports: stats, utils",
             "Suggests: testthat, knitr, rmarkdown", "LinkingTo: Rcpp",
             "VignetteBuilder: knitr"),
    exports = c("add_one", "abs_val", "clamp"),
    code = c("add_one <- function(x) x + 1",
             "abs_val <- function(x) {", "  .Deprecated(\"clamp\")", "  clamp(abs(x))", "}",
             "clamp <- function(x, lo = 0, hi = 1) {",
             "  if (x < lo) lo else if (x > hi) hi else x", "}"),
    data = "x1.rda"),
  "1.2" = list(
    date = "2026-03-01T00:00:00Z",
    desc = c(paste0("Authors@R: c(", .RG_ADA, ", ", .RG_GRACE, ", ", .RG_ALAN, ")"),
             "Author: Ada Lovelace [aut, cre], Grace Hopper [aut], Alan Turing [ctb]",
             "Maintainer: Ada Lovelace <ada@example.org>",
             "Depends: R (>= 4.0.0)", "Imports: stats, utils, lifecycle",
             "Suggests: knitr, rmarkdown", "LinkingTo: Rcpp, cpp11",
             "VignetteBuilder: knitr"),
    exports = c("add_one", "clamp", "scale01"),
    code = c("add_one <- function(x) {",
             "  lifecycle::deprecate_warn(\"1.2\", \"add_one()\", \"scale01()\")", "  x + 1", "}",
             "clamp <- function(x, lo = 0, hi = 1) {",
             "  if (x < lo) lo else if (x > hi) hi else x", "}",
             "scale01 <- function(x) (x - min(x)) / diff(range(x))"),
    data = "x2.rda"),
  "2.0" = list(
    date = "2026-04-01T00:00:00Z",
    desc = c(paste0("Authors@R: c(", sub("\"aut\"", "c(\"aut\", \"cre\")", .RG_GRACE), ", ",
                    .RG_ALAN, ")"),
             "Author: Grace Hopper [aut, cre], Alan Turing [ctb]",
             "Maintainer: Grace Hopper <grace@example.org>",
             "Depends: R (>= 4.1.0)", "Imports: utils, lifecycle",
             "Suggests: knitr, rmarkdown, covr", "LinkingTo: cpp11",
             "VignetteBuilder: knitr"),
    exports = c("clamp", "scale01", "tally"),
    code = c("add_one <- function(x) .Defunct(\"scale01\")",
             "clamp <- function(x, lo = 0, hi = 1) pmin(pmax(x, lo), hi)",
             "scale01 <- function(x) (x - min(x)) / diff(range(x))",
             "tally <- function(x, by) {",
             "  if (!missing(by)) lifecycle::deprecate_stop(\"2.0\", \"tally(by)\")",
             "  for (i in seq_along(x)) if (is.na(x[[i]])) x[[i]] <- 0",
             "  length(x)", "}"),
    data = "x2.rda"))

.RG_VIGNETTE <- c(
  "---", "title: \"Introduction to rgpkg\"", "author: \"Ada Lovelace\"",
  "output: rmarkdown::html_vignette", "vignette: >",
  "  %\\VignetteIndexEntry{Introduction}", "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}", "---", "", "```{r}", "library(rgpkg)", "clamp(2)", "```")

# Commit and tag one release of rgpkg in `dir`; NEWS.md keeps every release so far.
.rg_release <- function(dir, ver) {
  rel <- .RG_RELEASES[[ver]]
  writeLines(c("Package: rgpkg", paste("Version:", ver), "Title: Rows Golden Fixture",
               "Description: A package whose rows the golden test holds.", rel$desc,
               "License: GPL-3", "Encoding: UTF-8", "LazyData: true",
               paste0("Date/Publication: ", substr(rel$date, 1L, 10L), " 00:00:00 UTC"),
               "Repository: CRAN"),
             file.path(dir, "DESCRIPTION"))
  writeLines(c(paste0("export(", rel$exports, ")"), "importFrom(stats, median)"),
             file.path(dir, "NAMESPACE"))
  writeLines(rel$code, file.path(dir, "R", "code.R"))
  unlink(file.path(dir, "man"), recursive = TRUE)
  dir.create(file.path(dir, "man"))
  for (e in rel$exports) {
    writeLines(c(sprintf("\\name{%s}", e), sprintf("\\alias{%s}", e), sprintf("\\title{%s}", e),
                 sprintf("\\usage{%s(x)}", e), "\\arguments{\\item{x}{A number.}}",
                 "\\value{A number.}", sprintf("\\examples{%s(1)}", e)),
               file.path(dir, "man", paste0(e, ".Rd")))
  }
  if (ver != "1.0") writeLines(.RG_VIGNETTE, file.path(dir, "vignettes", "intro.Rmd"))
  file.copy(test_path("fixtures", "rows-golden", rel$data), file.path(dir, "data", "x.rda"),
            overwrite = TRUE)
  news <- file.path(dir, "NEWS.md")
  writeLines(c(sprintf("# rgpkg %s", ver), "", sprintf("* Exports %s.", toString(rel$exports)),
               "", if (file.exists(news)) readLines(news)), news)
  .rg_git(dir, rel$date, "add", "-A")
  .rg_git(dir, rel$date, "commit", "-q", "-m", shQuote(paste("version", ver)))
  .rg_git(dir, rel$date, "tag", ver)
  invisible(dir)
}

.rg_source <- function(root) {
  dir <- file.path(root, "rgpkg")
  for (d in c("R", "vignettes", "data", file.path("tests", "testthat"))) {
    dir.create(file.path(dir, d), recursive = TRUE)
  }
  writeLines(c("library(testthat)", "library(rgpkg)", "test_check(\"rgpkg\")"),
             file.path(dir, "tests", "testthat.R"))
  writeLines(c("test_that(\"clamp holds x to [0, 1]\", {", "  expect_equal(clamp(2), 1)", "})"),
             file.path(dir, "tests", "testthat", "test-clamp.R"))
  .rg_git(dir, .RG_RELEASES[[1L]]$date, "init", "-q")
  for (v in names(.RG_RELEASES)) .rg_release(dir, v)
  dir
}

.rg_analyze <- function() {
  root <- withr::local_tempdir()
  analyze_package(.rg_source(root), "rgpkg")
}

# Columns a read stamps with its own time, build or commit, or that the write fills later.
.RG_VOLATILE <- c("read_at", "assessed_at", "assessed_with", "analyzer_version", "commit_sha",
                  "tree_sha", "prev_commit", "rows_generation", "content_id")

# Every frame a version's read produces, less what a queued package recomputes
# whole (the cross-version columns and the churn) and the volatile columns.
.rg_frozen <- function(res) {
  frames <- c(res[c("summary", "api", "functions", "edges", "datasets", "vignettes")],
              stats::setNames(res$text, paste0("text_", names(res$text))),
              list(state = res$state))
  frames$summary <- frames$summary[, setdiff(names(frames$summary), .CROSS_VERSION_COLS),
                                   drop = FALSE]
  lapply(frames, function(df) df[, setdiff(names(df), .RG_VOLATILE), drop = FALSE])
}

# Radix sorts, so the order is the C locale's on every machine.
.rg_digest <- function(res) {
  frames <- lapply(.rg_frozen(res), function(df) {
    df <- df[, sort(names(df), method = "radix"), drop = FALSE]
    for (cn in names(df)) if (is.double(df[[cn]])) df[[cn]] <- signif(df[[cn]], 9)
    df <- df[do.call(order, c(unname(as.list(df)), method = "radix")), , drop = FALSE]
    rownames(df) <- NULL
    df
  })
  json <- jsonlite::toJSON(frames, digits = NA, na = "null")
  digest::digest(as.character(json), algo = "sha256", serialize = FALSE)
}

test_that("the fixture is built the same whatever git config is around, and stops on a git failure", {
  head_of <- function(src) .rg_git(src, "2026-01-01T00:00:00Z", "rev-parse", "2.0^{commit}")
  plain <- head_of(.rg_source(withr::local_tempdir()))
  # A commit encoding changes every commit id; a failing signer fails every commit.
  cfg <- withr::local_tempfile(lines = c("[i18n]", "  commitEncoding = ISO-8859-1"))
  withr::local_envvar(GIT_CONFIG_GLOBAL = cfg)
  expect_identical(head_of(.rg_source(withr::local_tempdir())), plain)
  writeLines(c("[commit]", "  gpgsign = true", "[gpg]", "  program = false"), cfg)
  expect_identical(head_of(.rg_source(withr::local_tempdir())), plain)
  expect_error(.rg_git(file.path(withr::local_tempdir(), "none"), "2026-01-01T00:00:00Z", "status"),
               "rows golden fixture: git status exited with status")
})

test_that("the golden names this generation and two digests", {
  g <- .rg_golden()
  expect_identical(as.integer(g$rows_generation), ROWS_GENERATION,
    info = "ROWS_GENERATION changed: regenerate tests/testthat/fixtures/rows-golden.json in the same pull request")
  expect_match(g$fallback, "^[0-9a-f]{64}$")
  expect_true(is.null(g$analyzer) || grepl("^[0-9a-f]{64}$", g$analyzer))
})

test_that("the rows R writes for a version match the golden on the fallback path", {
  .local_global("rpkg_analyzer_bin", function() "")
  got <- .rg_digest(.rg_analyze())
  g <- .rg_golden()
  if (!identical(got, g$fallback)) cat("fallback digest:", got, "\n")
  expect_identical(got, g$fallback, info = .rg_fix(g))
})

test_that("the rows R writes for a version match the golden under the pinned analyzer", {
  skip_if_not(nzchar(rpkg_analyzer_bin()), "needs rpkg-analyzer")
  got <- .rg_digest(.rg_analyze())
  cat("analyzer digest:", got, "\n")
  g <- .rg_golden()
  cls <- .analyzer_output_class(rpkg_analyzer_version())
  expect_identical(cls, as.character(g$output_class),
    info = "the output class changed: regenerate the golden, with no ROWS_GENERATION bump")
  expect_false(is.null(g$analyzer),
    info = paste("no analyzer digest recorded: set \"analyzer\" in",
                 "tests/testthat/fixtures/rows-golden.json to", got))
  if (identical(cls, as.character(g$output_class)) && !is.null(g$analyzer)) {
    expect_identical(got, g$analyzer, info = .rg_fix(g))
  }
})

test_that("the cross-version columns need only the inputs a skipped version carries", {
  .local_global("rpkg_analyzer_bin", function() "")
  res <- .rg_analyze()
  series <- lapply(seq_len(nrow(res$state)), function(i) list(
    symbols = .xv_json_to_chr(res$state$deprecated[[i]]),
    uses_lifecycle = identical(res$state$uses_lifecycle[[i]], 1L)))
  base <- res$summary[, setdiff(names(res$summary), .CROSS_VERSION_COLS), drop = FALSE]
  full <- add_cross_version_metrics(base, res$api, series)
  cut  <- add_cross_version_metrics(base[, .CROSS_VERSION_INPUTS, drop = FALSE], res$api, series)
  expect_identical(cut[, .CROSS_VERSION_COLS], full[, .CROSS_VERSION_COLS],
    info = paste("a cross-version column reads a summary column a skipped version does not",
                 "carry: add it to .CROSS_VERSION_INPUTS in scripts/analyze.R and to the",
                 "summary columns .prior_state() reads in scripts/update.R"))
})
