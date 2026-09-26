# tests/testthat/test-analyze-release-text.R: the text history from a real
# analyze_package over two tagged CRAN versions.

# An analyzer that prints a summary, the DESCRIPTION it finds and a NEWS section
# for that DESCRIPTION's Version.
.art_stub <- function(dir) {
  stub <- file.path(dir, "stub-text.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then echo "rpkg-analyzer 0.4.0-test"; exit 0; fi',
    'dir=$(echo "$1" | tr -d "\'")',
    'v=$(sed -n "s/^Version: *//p" "$dir/DESCRIPTION" | head -1)',
    'echo "{\\"rec\\":\\"summary\\",\\"loc_r\\":1,\\"n_fns_r\\":1,\\"analyzer_version\\":\\"0.4.0-test\\"}"',
    'echo "{\\"rec\\":\\"dcf\\",\\"Package\\":\\"pkgA\\",\\"Version\\":\\"$v\\",\\"RoxygenNote\\":\\"7.3.2\\"}"',
    'echo "{\\"rec\\":\\"release_notes\\",\\"package_version\\":\\"$v\\",\\"news_file\\":\\"NEWS.md\\",\\"release_notes_source\\":\\"news_md\\",\\"release_notes\\":\\"- changes in $v\\",\\"release_notes_truncated\\":false}"'),
    stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# Two tagged versions, as the github.com/cran mirror holds them.
.art_repo <- function(dest) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  git <- function(...) system2("git", c("-C", dest, ...), stdout = FALSE, stderr = FALSE)
  system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
  git("config", "user.email", "t@example.com")
  git("config", "user.name", "Test Bot")
  for (ver in c("1.0", "1.1")) {
    writeLines(c("Package: pkgA", paste("Version:", ver), "Title: T",
                 "Description: D.", "License: MIT"), file.path(dest, "DESCRIPTION"))
    git("add", "-A"); git("commit", "-m", ver); git("tag", ver)
  }
  TRUE
}

test_that("each version keeps its own DESCRIPTION and NEWS, and the latest is marked", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .art_stub(withr::local_tempdir()))
  repo <- file.path(withr::local_tempdir(), "pkgA")
  .art_repo(repo)

  text <- analyze_package(repo, "pkgA")$text
  expect_identical(text$versions$version, c("1.0", "1.1"))
  expect_identical(text$release_notes$package_version, c("1.0", "1.1"))
  expect_identical(unique(text$description_latest$version), "1.1")
  expect_identical(text$description_latest$field, "RoxygenNote")
})
