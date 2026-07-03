# Helper: run git inside repo; system2 pipes through shell so shQuote commit
# messages that contain spaces to prevent the shell from splitting them.
.git <- function(repo, ...) {
  system2("git", c("-C", repo, ...), stdout = FALSE, stderr = FALSE)
}
.gitc <- function(repo, ...) {
  system2("git",
          c("-C", repo, "-c", "user.email=t@t.test", "-c", "user.name=T", ...),
          stdout = FALSE, stderr = FALSE)
}

test_that("list_versions returns versions in date order with correct fields", {
  repo <- tempfile("ccm_git_test_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)

  dir.create(file.path(repo, "R"))
  writeLines("a <- 1", file.path(repo, "R", "a.R"))
  writeLines("Package: mypkg\nVersion: 1.0\n",
             file.path(repo, "DESCRIPTION"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.0"))
  .git(repo, "tag", "1.0")

  writeLines("a <- 2", file.path(repo, "R", "a.R"))
  writeLines("b <- 3", file.path(repo, "R", "b.R"))
  writeLines("Package: mypkg\nVersion: 1.1\n",
             file.path(repo, "DESCRIPTION"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.1"))
  .git(repo, "tag", "1.1")

  v <- list_versions(repo)

  expect_s3_class(v, "data.frame")
  expect_equal(nrow(v), 2L)
  expect_equal(colnames(v), c("version", "ref", "date", "commit"))
  expect_equal(v$version, c("1.0", "1.1"))
  expect_equal(v$ref,     c("1.0", "1.1"))
  # Dates must be YYYY-MM-DD
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v$date)))
  # Commits must be non-empty strings
  expect_true(all(nzchar(v$commit)))
})

test_that("list_versions strips R- prefix from legacy tags", {
  repo <- tempfile("ccm_git_legacy_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))
  writeLines("x <- 1", file.path(repo, "R", "x.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 0.9"))
  .git(repo, "tag", "R-0.9")

  v <- list_versions(repo)
  expect_equal(v$version, "0.9")
  expect_equal(v$ref, "R-0.9")
})

test_that("list_versions deduplicates tags pointing at same commit", {
  repo <- tempfile("ccm_git_dedup_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))
  writeLines("x <- 1", file.path(repo, "R", "x.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.0"))
  # Two tags pointing at the same commit
  .git(repo, "tag", "1.0")
  .git(repo, "tag", "1.0.0")

  v <- list_versions(repo)
  expect_equal(nrow(v), 1L)
})

test_that("package_churn parses added/deleted per file across commits", {
  repo <- tempfile("ccm_churn_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))
  writeLines(c("a <- 1", "b <- 2"), file.path(repo, "R", "a.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.0"))

  writeLines(c("a <- 1", "b <- 99", "c <- 3"), file.path(repo, "R", "a.R"))
  writeLines("new <- TRUE", file.path(repo, "R", "new.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.1"))

  ch <- package_churn(repo)

  expect_s3_class(ch, "data.frame")
  expect_true(all(c("commit", "version", "file", "added", "deleted") %in%
                    colnames(ch)))
  # Both commits contribute rows
  expect_true(nrow(ch) >= 2L)
  # All commits should be non-empty strings
  expect_true(all(nzchar(ch$commit)))
  # added and deleted are integer or NA
  expect_true(is.integer(ch$added) || is.numeric(ch$added))
})

test_that("extract_version extracts correct file tree at a tag", {
  repo <- tempfile("ccm_extract_")
  dest <- tempfile("ccm_tree_")
  on.exit({
    unlink(repo, recursive = TRUE)
    unlink(dest, recursive = TRUE)
  }, add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))
  writeLines("v1 <- TRUE", file.path(repo, "R", "v1.R"))
  writeLines("Package: mypkg\nVersion: 1.0\n", file.path(repo, "DESCRIPTION"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.0"))
  .git(repo, "tag", "1.0")

  # Add a second version
  writeLines("v2 <- TRUE", file.path(repo, "R", "v2.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.1"))
  .git(repo, "tag", "1.1")

  extracted <- extract_version(repo, "1.0", dest)

  # Should include R/v1.R and DESCRIPTION but NOT R/v2.R
  expect_true("R/v1.R" %in% extracted)
  expect_true("DESCRIPTION" %in% extracted)
  expect_false("R/v2.R" %in% extracted)

  # The extracted file should have the v1 content
  content <- readLines(file.path(dest, "R", "v1.R"), warn = FALSE)
  expect_true(any(grepl("v1", content)))
})

test_that("read_at returns file content and empty string for absent paths", {
  repo <- tempfile("ccm_read_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))
  writeLines("hello_world <- 42", file.path(repo, "R", "hello.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.0"))
  .git(repo, "tag", "1.0")

  content <- read_at(repo, "1.0", "R/hello.R")
  expect_true(grepl("hello_world", content))

  absent <- read_at(repo, "1.0", "R/does_not_exist.R")
  expect_equal(absent, "")
})

test_that("clone_package returns FALSE for a non-existent repo (offline-safe)", {
  # Use a clearly invalid local path as base so this runs without network
  dest <- tempfile("ccm_clone_fail_")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  result <- clone_package(
    "this_package_definitely_does_not_exist_9999",
    dest,
    base = "file:///nonexistent/path"
  )
  expect_false(result)
})
