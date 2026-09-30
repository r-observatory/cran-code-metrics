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

test_that("package_churn parses dash-separated version strings fully", {
  repo <- tempfile("ccm_dashver_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))
  writeLines("x <- 1", file.path(repo, "R", "x.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.0-3"))
  .git(repo, "tag", "1.0-3")

  # Also verify that a dot-only version still parses whole
  writeLines("x <- 2", file.path(repo, "R", "x.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.14.8"))
  .git(repo, "tag", "1.14.8")

  ch <- package_churn(repo)

  expect_s3_class(ch, "data.frame")
  expect_true(nrow(ch) >= 1L)
  # Dash version must be captured whole, not truncated to "1.0"
  expect_true("1.0-3" %in% ch$version)
  # Dot version must still parse whole
  expect_true("1.14.8" %in% ch$version)
})

test_that("package_churn resolves renamed file paths to the new path", {
  repo <- tempfile("ccm_rename_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"))
  writeLines("old <- 1", file.path(repo, "R", "old.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", shQuote("version 1.0"))
  .git(repo, "tag", "1.0")

  # Rename the file and commit
  .git(repo, "mv", "R/old.R", "R/new.R")
  .gitc(repo, "commit", "-m", shQuote("version 1.1"))
  .git(repo, "tag", "1.1")

  ch <- package_churn(repo)

  expect_s3_class(ch, "data.frame")
  # No file path in any row should contain the literal " => " arrow
  expect_false(any(grepl(" => ", ch$file, fixed = TRUE)))
  # The rename commit rows must record the new path, not the old one
  rename_rows <- ch[!is.na(ch$version) & ch$version == "1.1", ]
  expect_true(nrow(rename_rows) >= 1L)
  expect_true(any(rename_rows$file == "R/new.R"))
  # added and deleted must be integer/numeric (not NA for a text rename)
  expect_true(is.integer(rename_rows$added) || is.numeric(rename_rows$added))
})

# ---------------------------------------------------------------------------
# Extraction that cannot be done is an error, not an empty version
# ---------------------------------------------------------------------------

# A stand-in for git-lfs that needs no git-lfs: its smudge passes a pointer
# through only when GIT_LFS_SKIP_SMUDGE=1, as git-lfs does, and otherwise fails
# the way a missing LFS object does. The global config is replaced for the
# test, since a real git-lfs filter.lfs.process there would take precedence.
.local_fake_lfs <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  smudge <- file.path(dir, "fake-lfs-smudge.sh")
  writeLines(c("#!/bin/sh",
               'if [ "$GIT_LFS_SKIP_SMUDGE" = "1" ]; then exec cat; fi',
               'echo "fake-lfs: object not available" >&2',
               "exit 1"), smudge)
  Sys.chmod(smudge, mode = "0755")
  cfg <- file.path(dir, "gitconfig")
  writeLines(c('[filter "lfs"]', "\tclean = cat", paste0("\tsmudge = ", smudge),
               "\trequired = true", "[user]", "\tname = T", "\temail = t@t.test"), cfg)
  withr::local_envvar(GIT_CONFIG_GLOBAL = cfg, GIT_CONFIG_NOSYSTEM = "1",
                      .local_envir = frame)
  invisible(cfg)
}

# A repo whose data/x.RData is tracked by LFS, tagged 1.0 on its default branch.
.lfs_repo <- function(path) {
  dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)
  system2("git", c("init", "-q", path), stdout = FALSE, stderr = FALSE)
  writeLines("*.RData filter=lfs diff=lfs merge=lfs -text", file.path(path, ".gitattributes"))
  writeLines(c("version https://git-lfs.github.com/spec/v1",
               paste0("oid sha256:", strrep("0", 64L)), "size 3"),
             file.path(path, "data", "x.RData"))
  writeLines(c("Package: lfspkg", "Version: 1.0"), file.path(path, "DESCRIPTION"))
  .git(path, "add", "-A")
  .git(path, "commit", "-q", "-m", "v1")
  .git(path, "tag", "1.0")
  path
}

# Put an executable named `name` in front of PATH until the calling test ends.
.local_shim <- function(name, body, frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  path <- file.path(dir, name)
  writeLines(c("#!/bin/sh", body), path)
  Sys.chmod(path, mode = "0755")
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
                      .local_envir = frame)
  path
}

.one_version_repo <- function(path) {
  system2("git", c("init", "-q", path), stdout = FALSE, stderr = FALSE)
  writeLines("Package: p\nVersion: 1.0", file.path(path, "DESCRIPTION"))
  .git(path, "add", ".")
  .gitc(path, "commit", "-q", "-m", "v1")
  .git(path, "tag", "1.0")
  path
}

test_that("an LFS-tracked file that cannot be smudged extracts as its pointer", {
  .local_fake_lfs()
  repo <- .lfs_repo(withr::local_tempdir())
  # Plain git archive fails on this repo, as it did on BioTIP 3.12.
  plain <- suppressWarnings(system2("git", c("-C", repo, "archive", "1.0"),
                                    stdout = FALSE, stderr = FALSE))
  expect_false(identical(plain, 0L))
  dest <- withr::local_tempdir()
  files <- extract_version(repo, "1.0", dest)
  expect_setequal(files, c(".gitattributes", "DESCRIPTION", "data/x.RData"))
  expect_identical(readBin(file.path(dest, "data", "x.RData"), "raw", 1000L),
                   readBin(file.path(repo, "data", "x.RData"), "raw", 1000L))
})

test_that("a tree that is really empty extracts to character(0) without an error", {
  repo <- withr::local_tempdir()
  system2("git", c("init", "-q", repo), stdout = FALSE, stderr = FALSE)
  .gitc(repo, "commit", "-q", "--allow-empty", "-m", "empty")
  .git(repo, "tag", "3.0")
  expect_identical(extract_version(repo, "3.0", withr::local_tempdir()), character(0L))
})

test_that("a ref git cannot archive raises extract_failure at the archive step", {
  repo <- .one_version_repo(withr::local_tempdir())
  err <- tryCatch(extract_version(repo, "no-such-ref", withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$step, "archive")
  expect_identical(err$ref, "no-such-ref")
  expect_false(identical(err$status, 0L))
  expect_match(err$stderr, "no-such-ref", fixed = TRUE)
  expect_match(conditionMessage(err), "git archive of no-such-ref exited", fixed = TRUE)
})

test_that("a tar that fails raises extract_failure at the tar step", {
  repo <- .one_version_repo(withr::local_tempdir())
  .local_shim("tar", c('echo "tar: shimmed failure" >&2', "exit 2"))
  err <- tryCatch(extract_version(repo, "1.0", withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$step, "tar")
  expect_identical(err$status, 2L)
  expect_identical(err$stderr, "tar: shimmed failure")
})

test_that("git archive killed at GIT_TIMEOUT carries status 124", {
  repo <- .one_version_repo(withr::local_tempdir())
  real_git <- unname(Sys.which("git"))
  .local_shim("git", c(
    'for a in "$@"; do',
    '  if [ "$a" = archive ]; then echo "fatal: shimmed timeout" >&2; exit 124; fi',
    "done",
    sprintf('exec %s "$@"', shQuote(real_git))))
  err <- tryCatch(extract_version(repo, "1.0", withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$step, "archive")
  expect_identical(err$status, 124L)
})

test_that("a token git prints on stderr never reaches the condition", {
  .local_shim("git", c(
    'echo "fatal: unable to access https://x-access-token:ghs_abcDEF123@github.com/cran/p.git/" >&2',
    "exit 128"))
  err <- tryCatch(extract_version(withr::local_tempdir(), "1.0", withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_false(grepl("ghs_abcDEF123", conditionMessage(err), fixed = TRUE))
  expect_identical(err$stderr,
                   "fatal: unable to access https://***github.com/cran/p.git/")
})
