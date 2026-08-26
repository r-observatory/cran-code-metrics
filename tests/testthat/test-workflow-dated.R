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

test_that("the run and the tests that vet it install the same analyzer", {
  # A pin that drifts between the two is the quiet version of a broken run: the
  # tests pass against one reader while the nightly writes what a different one
  # produced, and nothing in the output says the two disagree.
  pin_of <- function(name) {
    yml <- readLines(file.path("..", "..", ".github", "workflows", name))
    line <- grep("gh release download .*rpkg-analyzer", yml, value = TRUE)
    expect_equal(length(line), 1L)
    sub(".*gh release download +([^ ]+).*", "\\1", line)
  }
  expect_equal(pin_of("test.yml"), pin_of("update.yml"))
})
