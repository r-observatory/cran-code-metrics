# What a clone leaves behind on disk, and what it must never leave behind.
#
# This replaces a `git remote set-url` that ran after every clone to strip a
# credential out of .git/config. That strip was skipped whenever a clone timed
# out, and it is now a no-op besides, since the URL carries no credential to
# strip. A control that cannot fail is indistinguishable from one that works, so
# it is asserted here instead: the assertion keeps its meaning if anyone ever
# reintroduces authentication, which the strip would not have.

# A bare repo standing in for github.com/cran/<pkg>.git, reachable over file://
# so the test needs no network.
.local_cran_mirror <- function(base, pkg) {
  src <- file.path(base, "src")
  dir.create(src)
  writeLines("Package: p\nVersion: 1.0\n", file.path(src, "DESCRIPTION"))
  system2("git", c("init", "--quiet", src), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", src, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", src, "-c", "user.email=t@t.test", "-c", "user.name=T",
                   "commit", "--quiet", "-m", shQuote("initial")),
          stdout = FALSE, stderr = FALSE)
  bare <- file.path(base, paste0(pkg, ".git"))
  system2("git", c("clone", "--quiet", "--bare", src, bare),
          stdout = FALSE, stderr = FALSE)
  list(base = paste0("file://", base), bare = bare)
}

.recorded_origin <- function(dest) {
  out <- suppressWarnings(
    system2("git", c("-C", dest, "config", "--get", "remote.origin.url"),
            stdout = TRUE, stderr = FALSE))
  trimws(paste(out, collapse = ""))
}

test_that("the URL a clone records carries no credential", {
  skip_if(!nzchar(Sys.which("git")), "git is required")
  m <- .local_cran_mirror(withr::local_tempdir(), "pkgA")
  dest <- file.path(withr::local_tempdir(), "clone")

  expect_true(clone_package("pkgA", dest, base = m$base))

  url <- .recorded_origin(dest)
  # git writes the clone URL into .git/config verbatim, so a credential in the
  # URL is a credential on disk for the life of the analysis - and on the git
  # command line, which every process on the machine can read, for the length
  # of the clone.
  expect_false(grepl("@", url, fixed = TRUE))
  expect_false(grepl("x-access-token", url, fixed = TRUE))
  expect_identical(url, paste0(m$base, "/pkgA.git"))
})

test_that("clone_package has no token parameter to fall back on", {
  # clone_package() no longer has an authenticated branch to send a future
  # call down (scripts/git.R) - deleted, not merely unused. Asserted here as a
  # structural guard: a `token` argument reappearing in the signature is the
  # first step toward the leak this replaced, and this test catches that step
  # directly rather than relying on nobody calling it that way.
  expect_identical(names(formals(clone_package)), c("pkg", "dest", "base"))
})

test_that("the pipeline's own clone is unauthenticated", {
  # Pointing CRAN_GIT_BASE at a local mirror is what gives this teeth: if
  # default_io()$clone() ever grew its own way of building an authenticated
  # URL instead of passing `base` straight through, the clone of this fixture
  # would fail or the recorded URL would not be the mirror's - either way this
  # test fails rather than passing quietly.
  skip_if(!nzchar(Sys.which("git")), "git is required")
  m <- .local_cran_mirror(withr::local_tempdir(), "pkgB")
  dest <- file.path(withr::local_tempdir(), "clone")

  old <- CRAN_GIT_BASE
  CRAN_GIT_BASE <<- m$base
  on.exit(CRAN_GIT_BASE <<- old, add = TRUE)

  expect_true(default_io()$clone("pkgB", dest))

  url <- .recorded_origin(dest)
  expect_identical(url, paste0(m$base, "/pkgB.git"))
  expect_false(grepl("@", url, fixed = TRUE))
})

test_that("no pipeline script reads a GitHub credential", {
  # The environment allowlist keeps a token away from an evaluated citation
  # file, but the token also has to stop being fetched in the first place. This
  # is the grep that says so, kept as a test so a reintroduction has to be
  # deliberate.
  files <- list.files("../../scripts", pattern = "\\.R$",
                      full.names = TRUE, recursive = TRUE)
  expect_true(length(files) > 0L)
  reads <- Filter(function(f) {
    txt <- readLines(f, warn = FALSE)
    any(grepl("Sys.getenv\\s*\\(\\s*[\"'](GITHUB_TOKEN|GH_TOKEN)[\"']", txt))
  }, files)
  expect_identical(basename(reads), character(0L))
})
