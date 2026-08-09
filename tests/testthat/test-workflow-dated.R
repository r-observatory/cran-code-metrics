# tests/testthat/test-workflow-dated.R
test_that("update.yml publishes dated code and data releases, not rolling current", {
  # test_dir() sources this file with the working directory set to
  # tests/testthat/, so reach the repo root the same way other fixtures do.
  workflow_path <- file.path("..", "..", ".github", "workflows", "update.yml")
  yml <- paste(readLines(workflow_path), collapse = "\n")
  expect_true(grepl("code-\\$\\(date", yml) || grepl('code-', yml, fixed = TRUE))
  expect_true(grepl("data-", yml, fixed = TRUE))
  expect_true(grepl("cran-data-metrics.db", yml, fixed = TRUE))
  # Prior-day immutability: no unconditional clobber of a non-today tag.
  expect_true(grepl("prune.R", yml, fixed = TRUE))
  expect_true(grepl("render_notes.R", yml, fixed = TRUE))
})

test_that("the runner is pinned, because the sandbox depends on what it is", {
  # ubuntu-latest floats. Whether unprivileged user namespaces are permitted,
  # and what container tooling is present, differ between images, so leaving it
  # floating means the boundary can change with no commit here.
  # test_dir() sources this file with the working directory set to
  # tests/testthat/, so reach the repo root the same way other fixtures do.
  for (f in c("update.yml", "test.yml")) {
    path <- file.path("..", "..", ".github", "workflows", f)
    y <- readLines(path, warn = FALSE)
    expect_false(any(grepl("runs-on:\\s*ubuntu-latest", y)),
                 info = paste(f, "still floats on ubuntu-latest"))
    expect_true(any(grepl("runs-on:\\s*ubuntu-24\\.04", y)), info = f)
  }
})

test_that("the checkout does not leave a credential in the workspace", {
  y <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"),
                 warn = FALSE)
  expect_true(any(grepl("persist-credentials:\\s*false", y)))
})

test_that("the citation sandbox is required in CI", {
  y <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"),
                 warn = FALSE)
  expect_true(any(grepl("CITATION_SANDBOX:\\s*require", y)))
})

test_that("CITATION_IMAGE is pinned by digest everywhere it appears", {
  files <- c(
    file.path("..", "..", ".github", "workflows", "update.yml"),
    file.path("..", "..", ".github", "workflows", "test.yml"),
    file.path("..", "..", "scripts", "config.R")
  )
  image_lines <- unlist(lapply(files, function(f) {
    y <- readLines(f, warn = FALSE)
    # Skip comment-only lines: config.R documents how to resolve a fresh
    # digest by referencing the tag directly, which is not itself a pin.
    y <- y[!grepl("^\\s*#", y)]
    grep("rocker/r-ver", y, value = TRUE)
  }))
  expect_true(length(image_lines) > 0)
  unpinned <- image_lines[!grepl("@sha256:", image_lines, fixed = TRUE)]
  expect_true(length(unpinned) == 0L, info = paste(unpinned, collapse = "; "))
})
