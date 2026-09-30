test_that("analyze_version returns flat named list with structure columns", {
  file_map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/foo.R"     = "foo <- function() 1\n"
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")

  result <- analyze_version(ctx)

  expect_true(is.list(result))
  # Structure metrics must be present
  expect_true("n_files"       %in% names(result))
  expect_true("loc_total"     %in% names(result))
  expect_true("loc_r"         %in% names(result))
  expect_true("has_src"       %in% names(result))
  expect_true("compiled_share" %in% names(result))
  expect_true("lang_breakdown" %in% names(result))
  # All values are scalar
  expect_true(all(vapply(result, length, integer(1L)) == 1L))
})

test_that("analyze_version: a failing group emits warning, yields NA sentinel, does not abort", {
  file_map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "R/a.R"       = "a <- 1\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")

  # Register a deliberately failing group in a local copy of the registry
  old_groups <- METRIC_GROUPS
  METRIC_GROUPS[["fail_test"]] <<- function(ctx) stop("deliberate failure")
  on.exit(METRIC_GROUPS[["fail_test"]] <<- NULL, add = TRUE)

  result <- withCallingHandlers(
    analyze_version(ctx),
    warning = function(w) {
      expect_true(grepl("fail_test", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )

  # Must return a list (not abort)
  expect_true(is.list(result))

  # Sentinel NA entry for the failed group
  sentinel_name <- ".error.fail_test"
  expect_true(sentinel_name %in% names(result))
  expect_true(is.na(result[[sentinel_name]]))

  # Successful groups still present
  expect_true("n_files" %in% names(result))
})

test_that("analyze_version: unknown/empty group registry still returns list", {
  file_map <- list("R/a.R" = "a <- 1\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")

  old_groups <- METRIC_GROUPS
  METRIC_GROUPS <<- list()
  on.exit(METRIC_GROUPS <<- old_groups, add = TRUE)

  result <- analyze_version(ctx)
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})

test_that("analyze_package produces summary/churn/api data.frames from a local repo", {
  repo <- tempfile("ccm_ap_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))

  writeLines(c("foo <- function() 1"), file.path(repo, "R", "foo.R"))
  writeLines("Package: mypkg\nVersion: 1.0\n", file.path(repo, "DESCRIPTION"))
  writeLines("export(foo)\n", file.path(repo, "NAMESPACE"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git",
          c("-C", repo, "-c", "user.email=t@t.test",
            "-c", "user.name=T", "commit", "-m", shQuote("version 1.0")),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "tag", "1.0"), stdout = FALSE, stderr = FALSE)

  writeLines(c("foo <- function() 1", "bar <- function() 2"),
             file.path(repo, "R", "foo.R"))
  writeLines("Package: mypkg\nVersion: 1.1\n", file.path(repo, "DESCRIPTION"))
  writeLines("export(foo)\nexport(bar)\n", file.path(repo, "NAMESPACE"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git",
          c("-C", repo, "-c", "user.email=t@t.test",
            "-c", "user.name=T", "commit", "-m", shQuote("version 1.1")),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "tag", "1.1"), stdout = FALSE, stderr = FALSE)

  result <- analyze_package(repo, "mypkg")

  # Three output components
  expect_true(all(c("summary", "churn", "api") %in% names(result)))

  # summary: two version rows
  expect_s3_class(result$summary, "data.frame")
  expect_equal(nrow(result$summary), 2L)
  expect_true("package" %in% colnames(result$summary))
  expect_true("version" %in% colnames(result$summary))

  # api: two rows, exports_added is JSON
  expect_s3_class(result$api, "data.frame")
  expect_equal(nrow(result$api), 2L)
  # First version: foo added; second: bar added (foo already present)
  v1_api <- result$api[result$api$version == "1.0", ]
  v2_api <- result$api[result$api$version == "1.1", ]
  expect_true(grepl("foo", v1_api$exports_added))
  expect_true(grepl("bar", v2_api$exports_added))

  # churn data.frame has package column
  expect_s3_class(result$churn, "data.frame")
  expect_true("package" %in% colnames(result$churn))
})

test_that("analyze_package persists the five typed dependency columns per version", {
  # Drive the production (binary) path with a stub analyzer whose summary carries
  # NO typed dependency fields, exactly like the real rpkg-analyzer. The columns
  # must therefore come from the per-version DESCRIPTION enrichment, not the R
  # metric groups, and must survive the round-trip through the SQLite export.
  skip_on_os("windows")

  repo <- tempfile("ccm_deps_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))

  desc <- paste0(
    "Package: depspkg\nVersion: %s\n",
    "Imports: methods, stats\n",
    "Suggests: testthat\n",
    "LinkingTo: Rcpp\n")            # no Depends, no Enhances -> honest NA

  commit_version <- function(ver) {
    writeLines("foo <- function() 1", file.path(repo, "R", "foo.R"))
    writeLines(sprintf(desc, ver), file.path(repo, "DESCRIPTION"))
    writeLines("export(foo)\n", file.path(repo, "NAMESPACE"))
    system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
    system2("git",
            c("-C", repo, "-c", "user.email=t@t.test", "-c", "user.name=T",
              "commit", "-m", shQuote(paste("version", ver))),
            stdout = FALSE, stderr = FALSE)
    system2("git", c("-C", repo, "tag", ver), stdout = FALSE, stderr = FALSE)
  }
  commit_version("1.0")
  commit_version("1.1")

  # Stub analyzer: ignores its argument, prints a summary with no dep fields.
  fixture <- file.path(repo, "fixture.ndjson")
  writeLines('{"rec":"summary","n_files":1}', fixture)
  stub <- file.path(repo, "stub-analyzer.sh")
  writeLines(c("#!/bin/sh", sprintf("cat %s", shQuote(fixture))), stub)
  Sys.chmod(stub, mode = "0755")
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)

  result <- analyze_package(repo, "depspkg")

  typed <- c("depends", "imports", "suggests", "linkingto", "enhances")
  expect_true(all(typed %in% colnames(result$summary)))

  # Round-trip through the real SQLite export and read the row back out.
  db <- tempfile(fileext = ".db")
  on.exit(unlink(db, force = TRUE), add = TRUE)
  export_metrics(db, result$summary, result$churn, result$api)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(all(typed %in% DBI::dbListFields(con, "cran_code_summary")))

  row <- DBI::dbGetQuery(con,
    "SELECT depends, imports, suggests, linkingto, enhances
     FROM cran_code_summary WHERE version = '1.1'")
  expect_equal(nrow(row), 1L)
  expect_equal(row$imports,   "methods, stats")   # verbatim DESCRIPTION text
  expect_equal(row$suggests,  "testthat")
  expect_equal(row$linkingto, "Rcpp")             # LinkingTo mapped to linkingto
  expect_true(is.na(row$depends))                 # absent field -> honest NA
  expect_true(is.na(row$enhances))
})

test_that("a cap inside one R-fallback group runs the group again rather than NA", {
  file_map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nImports: stats\n",
    "NAMESPACE"   = "export(f)\n",
    "R/f.R"       = "f <- function(x) x + 1\n"
  )
  mk  <- function() build_context("p", "1.0", "1.0", "2024-01-01",
                                  names(file_map), function(p) file_map[[p]] %||% "")
  want <- analyze_version(mk())
  groups <- METRIC_GROUPS
  groups$meta <- .fires_cap_once(groups$meta)
  .local_global("METRIC_GROUPS", groups)
  expect_identical(analyze_version(mk()), want)
})

# ---------------------------------------------------------------------------
# A cap on the first file a group reads, for every R-fallback group
# ---------------------------------------------------------------------------

.sweep_map <- list(
  "DESCRIPTION" = paste0(
    "Package: p\nVersion: 1.0\nTitle: P\nDescription: A package.\n",
    "Depends: R (>= 4.1)\nImports: stats\nSuggests: testthat\n",
    "Config/testthat/edition: 3\nLicense: MIT\nSystemRequirements: zlib\n"),
  "NAMESPACE"   = "export(f)\nexportPattern(\"^g\")\n",
  "R/f.R"       = paste0("#' F\n#' @export\nf <- function(x) {\n  eval(substitute(x))\n}\n",
                         "g1 <- function() stats:::median.default(1)\n"),
  "tests/testthat/test-f.R" = "test_that('f', expect_equal(f(1), 1))\n",
  "README.md"   = "# p\n\nInstall with `install.packages('p')`.\n",
  "NEWS.md"     = "# p 1.0\n\n* First.\n",
  "src/Makevars" = "PKG_CFLAGS = -O3\n",
  "vignettes/intro.Rmd" = paste0("---\ntitle: Intro\nvignette: >\n",
                                 "  %\\VignetteEngine{knitr::rmarkdown}\n---\n```{r}\n1\n```\n"))

.sweep_ctx <- function() {
  build_context("p", "1.0", "1.0", "2024-01-01", names(.sweep_map),
                function(p) .sweep_map[[p]] %||% "")
}

for (nm in c("structure", "functions", "docs", "tests", "security", "health", "portability")) {
  test_that(sprintf("a cap on the first file the %s group reads leaves its metrics unchanged", nm), {
    .local_global("METRIC_GROUPS", METRIC_GROUPS[nm])
    want <- analyze_version(.sweep_ctx())
    ctx <- .sweep_ctx()
    fired <- FALSE
    cap_first <- function(f) {
      force(f)
      function(p) {
        if (!fired) {
          fired <<- TRUE
          stop(.cap_error())
        }
        f(p)
      }
    }
    ctx$read  <- cap_first(ctx$read)
    ctx$lines <- cap_first(ctx$lines)
    expect_identical(analyze_version(ctx), want)
    expect_true(fired)
  })
}
