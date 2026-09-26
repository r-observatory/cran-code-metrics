# tests/testthat/test-config-split.R
test_that("the code and data DB filenames are distinct and correct", {
  expect_identical(DB_FILENAME, "cran-code-metrics.db")
  expect_identical(DATA_DB_FILENAME, "cran-data-metrics.db")
})

test_that("the text history has a database of its own on CRAN", {
  expect_identical(RELEASE_TEXT_DB_FILENAME, "cran-release-text.db")
  expect_false(identical(RELEASE_TEXT_DB_FILENAME, DB_FILENAME))
})
