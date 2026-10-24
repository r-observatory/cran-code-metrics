# tests/testthat/test-transit.R: CRAN Transit versions, committed onto their base tag and walked.

# One release of `pkg` in `dir`, exporting `exports`, committed and tagged on `date`.
.tv_release <- function(dir, pkg, ver, date, exports) {
  writeLines(c(paste("Package:", pkg), paste("Version:", ver), "Title: T",
               "Description: T.", "Author: T", "Maintainer: T <t@t.test>",
               "License: MIT"), file.path(dir, "DESCRIPTION"))
  writeLines(sprintf("export(%s)", exports), file.path(dir, "NAMESPACE"))
  dir.create(file.path(dir, "R"), showWarnings = FALSE)
  writeLines(sprintf("%s <- function() 1", exports), file.path(dir, "R", "code.R"))
  .vst_git(dir, "add", "-A", date = date)
  .vst_git(dir, "commit", "-q", "-m", shQuote(paste("version", ver)), date = date)
  .vst_git(dir, "tag", ver, date = date)
  invisible(dir)
}

# A mirror repository at <root>/<pkg>.git with 1.0 (exporting a) and 2.0 (a, b and z).
.tv_source <- function(root, pkg = "tvpkg") {
  dir <- file.path(root, paste0(pkg, ".git"))
  dir.create(dir, recursive = TRUE)
  .vst_git(dir, "init", "-q")
  .tv_release(dir, pkg, "1.0", "2026-01-01T00:00:00Z", "a")
  .tv_release(dir, pkg, "2.0", "2026-04-01T00:00:00Z", c("a", "b", "z"))
  dir
}

# A source tarball of `pkg` with everything under <pkg>/, as R CMD build writes it.
# `files` maps a path to its exact bytes, as a string; `exec` are made executable
# and `links` maps a path to the target of a symbolic link.
.tv_tarball <- function(dir, pkg, files, exec = character(0L), links = list()) {
  stage <- tempfile("tv_stage_", tmpdir = dir)
  for (p in names(files)) {
    f <- file.path(stage, pkg, p)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(files[[p]]), f)
  }
  for (p in exec) Sys.chmod(file.path(stage, pkg, p), mode = "0755")
  for (p in names(links)) file.symlink(links[[p]], file.path(stage, pkg, p))
  out <- file.path(dir, paste0(pkg, "_", sub("^Version: ", "", regmatches(
    files[["DESCRIPTION"]], regexpr("Version: [^\n]+", files[["DESCRIPTION"]]))), ".tar.gz"))
  system2("tar", c("-czf", shQuote(out), "-C", shQuote(stage), pkg),
          env = "COPYFILE_DISABLE=1", stdout = FALSE, stderr = FALSE)
  out
}

# The Transit release 1.0-1 of tvpkg: exports a and b.
.tv_files <- function(pkg = "tvpkg", ver = "1.0-1") {
  list(DESCRIPTION = paste0("Package: ", pkg, "\nVersion: ", ver,
                            "\nTitle: T\nDescription: T.\nAuthor: T\n",
                            "Maintainer: T <t@t.test>\nLicense: MIT\n"),
       NAMESPACE = "export(a)\nexport(b)\n",
       "R/code.R" = "a <- function() 1\nb <- function() 1\n")
}

# One row of the Transit listing for a tarball.
.tv_row <- function(tarball, pkg = "tvpkg", ver = "1.0-1", base = "1.0",
                    released = "2026-02-15T19:04:53Z",
                    sha256 = digest::digest(file = tarball, algo = "sha256")) {
  data.frame(package = pkg, version = ver, base_version = base, released = released,
             url = paste0("file://", normalizePath(tarball)), sha256 = sha256,
             stringsAsFactors = FALSE)
}

.tv_clone <- function(root, pkg = "tvpkg") {
  dest <- file.path(withr::local_tempdir(.local_envir = parent.frame()), pkg)
  stopifnot(clone_package(pkg, dest, base = root))
  dest
}

.tv_blob <- function(repo, spec) {
  f <- tempfile()
  on.exit(unlink(f))
  system2("git", c("-C", shQuote(repo), "cat-file", "blob", shQuote(spec)),
          stdout = f, stderr = FALSE)
  readBin(f, "raw", 1e6)
}

# ---- The listing ---------------------------------------------------------------

test_that("the listing in the repository names each version once, with a pinned copy", {
  tv <- read_transit_versions(test_path("..", "..", "scripts", "transit-versions.csv"))
  expect_identical(names(tv), c("package", "version", "base_version", "released", "url", "sha256"))
  expect_gt(nrow(tv), 0L)
  expect_false(anyDuplicated(paste(tv$package, tv$version)) > 0L)
  expect_true(all(grepl("^[0-9a-f]{64}$", tv$sha256)))
  expect_true(all(startsWith(tv$url, "https://packagemanager.posit.co/cran/")))
  expect_true(all(endsWith(tv$url, paste0("/", tv$package, "_", tv$version, ".tar.gz"))))
  expect_false(any(is.na(as.POSIXct(tv$released, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))))
  expect_true(all(tv$version != tv$base_version))
})

test_that("a listing with a column missing or a malformed checksum stops the run", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("package,version,base_version,released,url",
               "p,1.0-1,1.0,2026-04-15T19:04:53Z,https://x/p_1.0-1.tar.gz"), f)
  expect_error(read_transit_versions(f), "sha256")
  writeLines(c("package,version,base_version,released,url,sha256",
               "p,1.0-1,1.0,2026-04-15T19:04:53Z,https://x/p_1.0-1.tar.gz,abc"), f)
  expect_error(read_transit_versions(f), "checksum")
})

# ---- Committing a Transit version ------------------------------------------------

test_that("a Transit version lands between its base and the next release, dated as listed", {
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  head_before <- .vst_rev_parse(repo, "HEAD")
  tb <- .tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files())
  expect_identical(add_transit_versions(repo, .tv_row(tb)), "1.0-1")
  lv <- list_versions(repo)
  expect_identical(lv$version, c("1.0", "1.0-1", "2.0"))
  expect_identical(lv$date[[2L]], "2026-02-15")
  expect_identical(.vst_rev_parse(repo, "1.0-1^"), .vst_rev_parse(repo, "1.0"))
  expect_identical(.vst_rev_parse(repo, "HEAD"), head_before)
  expect_identical(.vst_git(repo, "log", "-1", "--format=%s", "1.0-1"), "version 1.0-1")
})

test_that("the same tarball gives the same commit in two clones", {
  root <- withr::local_tempdir()
  .tv_source(root)
  tb <- .tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files())
  one <- .tv_clone(root); two <- .tv_clone(root)
  add_transit_versions(one, .tv_row(tb))
  add_transit_versions(two, .tv_row(tb))
  expect_identical(.vst_rev_parse(one, "1.0-1"), .vst_rev_parse(two, "1.0-1"))
  expect_match(.vst_rev_parse(one, "1.0-1"), "^[0-9a-f]{40}$")
})

test_that("the tree holds the tarball's bytes, whatever its .gitignore and .gitattributes say", {
  # A runner converting line endings on the way in would rewrite crlf.txt.
  cfg <- withr::local_tempfile()
  writeLines(c("[core]", "\tautocrlf = true"), cfg)
  withr::local_envvar(GIT_CONFIG_GLOBAL = cfg, GIT_CONFIG_NOSYSTEM = "1")
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  files <- c(.tv_files(),
             ".gitignore" = "ignored.txt\n",
             ".gitattributes" = "*.txt text eol=lf\n",
             "ignored.txt" = "kept\n",
             "inst/crlf.txt" = "one\r\ntwo\r\n",
             "configure" = "#!/bin/sh\nexit 0\n")
  tb <- .tv_tarball(withr::local_tempdir(), "tvpkg", files, exec = "configure",
                    links = list("inst/link" = "crlf.txt"))
  add_transit_versions(repo, .tv_row(tb))
  ls <- .vst_git(repo, "ls-tree", "-r", "1.0-1")
  mode_of <- function(p) sub(" .*", "", ls[endsWith(ls, paste0("\t", p))])
  expect_setequal(sub(".*\t", "", ls), c(names(files), "inst/link"))
  for (p in names(files)) {
    expect_identical(.tv_blob(repo, paste0("1.0-1:", p)), charToRaw(files[[p]]), info = p)
  }
  expect_identical(mode_of("configure"), "100755")
  expect_identical(mode_of("DESCRIPTION"), "100644")
  expect_identical(mode_of("inst/link"), "120000")
  expect_identical(.tv_blob(repo, "1.0-1:inst/link"), charToRaw("crlf.txt"))
})

test_that("a tarball whose checksum differs fails as a transit_failure and adds nothing", {
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  tb <- .tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files())
  err <- tryCatch(add_transit_versions(repo, .tv_row(tb, sha256 = strrep("0", 64L))),
                  error = function(e) e)
  expect_s3_class(err, "transit_failure")
  expect_match(conditionMessage(err), "tvpkg 1.0-1", fixed = TRUE)
  expect_match(conditionMessage(err), "checksum", fixed = TRUE)
  expect_identical(list_versions(repo)$version, c("1.0", "2.0"))
})

test_that("a missing base tag fails before anything is fetched", {
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  tb <- .tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files())
  fetch <- function(url, destfile) stop("fetch was called")
  err <- tryCatch(add_transit_versions(repo, .tv_row(tb, base = "0.9"), fetch = fetch),
                  error = function(e) e)
  expect_s3_class(err, "transit_failure")
  expect_match(conditionMessage(err), "base 0.9", fixed = TRUE)
  expect_false(grepl("fetch was called", conditionMessage(err), fixed = TRUE))
})

test_that("a download that never lands fails as a transit_failure", {
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  row <- .tv_row(.tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files()))
  row$url <- paste0("file://", file.path(withr::local_tempdir(), "gone.tar.gz"))
  err <- tryCatch(add_transit_versions(repo, row, fetch = function(url, destfile)
                    .fetch_transit_tarball(url, destfile, tries = 1L)),
                  error = function(e) e)
  expect_s3_class(err, "transit_failure")
  expect_match(conditionMessage(err), "could not be downloaded", fixed = TRUE)
})

test_that("a cap that fires while a Transit version is added goes on as itself", {
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  row <- .tv_row(.tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files()))
  fetch <- function(url, destfile) stop(.time_limit_msg())
  err <- tryCatch(add_transit_versions(repo, row, fetch = fetch), error = function(e) e)
  expect_false(inherits(err, "transit_failure"))
  expect_true(.is_time_limit(err))
  expect_identical(.classify_failure(err, 1), "timeout")
})

test_that("a version the mirror already has a tag for is left alone and nothing is fetched", {
  root <- withr::local_tempdir()
  src <- .tv_source(root)
  .tv_release(src, "tvpkg", "2.1", "2026-05-01T00:00:00Z", "a")
  repo <- .tv_clone(root)
  tb <- .tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files(ver = "2.1"))
  fetch <- function(url, destfile) stop("fetched")
  expect_identical(add_transit_versions(repo, .tv_row(tb, ver = "2.1"), fetch = fetch), "2.1")
  expect_identical(.vst_rev_parse(repo, "2.1"), .vst_rev_parse(src, "2.1"))
})

test_that("no listed version adds nothing", {
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  tb <- .tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files())
  expect_identical(add_transit_versions(repo, NULL), character(0L))
  expect_identical(add_transit_versions(repo, .tv_row(tb)[0L, ]), character(0L))
  expect_identical(list_versions(repo)$version, c("1.0", "2.0"))
})

test_that("the Transit commit's churn is recorded against its base when its tag is named", {
  root <- withr::local_tempdir()
  .tv_source(root)
  repo <- .tv_clone(root)
  add_transit_versions(repo, .tv_row(.tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files())))
  expect_false("1.0-1" %in% package_churn(repo)$version)
  ch <- package_churn(repo, refs = "1.0-1")
  rows <- ch[ch$version %in% "1.0-1", , drop = FALSE]
  expect_setequal(rows$file, c("DESCRIPTION", "NAMESPACE", "R/code.R"))
  expect_identical(rows$added[rows$file == "NAMESPACE"], 1L)
  expect_identical(rows$commit[[1L]], .vst_rev_parse(repo, "1.0-1"))
  # The mirror's own commits are each recorded once, as without the tag.
  mirror <- ch[!ch$version %in% "1.0-1", , drop = FALSE]
  rownames(mirror) <- NULL
  expect_identical(mirror, package_churn(repo))
})

# ---- A run with a Transit listing ------------------------------------------------

# One run_update over `latest`, cloning from `root`, with `transit` as the listing.
.tv_run <- function(out_dir, root, latest, transit = NULL, skip = TRUE) {
  .local_global("VERSION_SKIP", skip)
  io <- list(
    package_list = function() data.frame(package = names(latest),
                                         latest_version = unname(latest),
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) clone_package(pkg, dest, base = root))
  if (!is.null(transit)) io$transit_versions <- function() transit
  m <- NULL
  log <- utils::capture.output(m <- suppressMessages(suppressWarnings(
    run_update(io, out_dir, shard_size = 10L))))
  structure(m, log = log)
}

.tv_status <- function(out_dir) jsonlite::fromJSON(file.path(out_dir, "run-status.json"))

.tv_api <- function(out_dir, pkg = "tvpkg") {
  .vst_query(out_dir, "SELECT version, exports_added, exports_removed, n_exports
                        FROM cran_api_history WHERE package = ? ORDER BY version",
             params = list(pkg))
}

test_that("a stored package with an unanalysed Transit version is queued once and read", {
  .vst_local_run_env(); .vs_stub(c("1.0", "1.0-1", "2.0"))
  root <- withr::local_tempdir()
  .tv_source(root)
  out <- withr::local_tempdir()
  .tv_run(out, root, c(tvpkg = "2.0"))
  expect_identical(.vst_summary_keys(out, "tvpkg")$version, c("1.0", "2.0"))
  row <- .tv_row(.tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files()))

  log <- attr(.tv_run(out, root, c(tvpkg = "2.0"), transit = row), "log")
  expect_identical(.tv_status(out)$n_shard, 1L)
  expect_true(any(grepl("read 1 of 3 listed versions (new 1); diffs redone: 1", log, fixed = TRUE)))
  expect_identical(.vst_summary_keys(out, "tvpkg")$version, c("1.0", "1.0-1", "2.0"))
  rel <- .vst_query(out, "SELECT version, released FROM cran_code_summary
                          WHERE package = 'tvpkg' ORDER BY version")
  expect_identical(rel$released[rel$version == "1.0-1"], "2026-02-15")
  api <- .tv_api(out)
  expect_identical(api$exports_added[api$version == "1.0-1"], '["b"]')
  expect_identical(api$exports_added[api$version == "2.0"], '["z"]')
  st <- .vst_state(out, "tvpkg")
  expect_identical(st$prev_version[st$version == "2.0"], "1.0-1")
  ch <- .vst_query(out, "SELECT file FROM cran_code_churn WHERE package = 'tvpkg' AND version = '1.0-1'")
  expect_setequal(ch$file, c("DESCRIPTION", "NAMESPACE", "R/code.R"))

  # Stored now, so the listing alone queues it no more.
  .tv_run(out, root, c(tvpkg = "2.0"), transit = row)
  expect_identical(.tv_status(out)$n_shard, 0L)
})

test_that("a skip run that adds a Transit version writes the rows a full walk writes", {
  .vst_local_run_env(); .vs_stub(c("1.0", "1.0-1", "2.0"))
  root <- withr::local_tempdir()
  .tv_source(root)
  row <- .tv_row(.tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files()))
  skip <- withr::local_tempdir(); full <- withr::local_tempdir()
  .tv_run(skip, root, c(tvpkg = "2.0"))
  .tv_run(skip, root, c(tvpkg = "2.0"), transit = row)
  .tv_run(full, root, c(tvpkg = "2.0"), transit = row, skip = FALSE)
  expect_identical(.vs_rows(skip, "tvpkg"), .vs_rows(full, "tvpkg"))
})

test_that("an archived package with an unanalysed Transit version is queued", {
  .vst_local_run_env(); .vs_stub(c("1.0", "1.0-1", "2.0"))
  root <- withr::local_tempdir()
  .tv_source(root)
  out <- withr::local_tempdir()
  .tv_run(out, root, c(tvpkg = "2.0"))
  row <- .tv_row(.tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files()))
  .tv_run(out, root, c(tvpkg = NA_character_), transit = row)
  expect_identical(.vst_summary_keys(out, "tvpkg")$version, c("1.0", "1.0-1", "2.0"))
})

test_that("a Transit version that cannot be fetched fails its package at the fetch stage and keeps its rows", {
  .vst_local_run_env(); .vs_stub(c("1.0", "1.0-1", "2.0", "2.1"))
  root <- withr::local_tempdir()
  src <- .tv_source(root)
  out <- withr::local_tempdir()
  row <- .tv_row(.tv_tarball(withr::local_tempdir(), "tvpkg", .tv_files()))
  .tv_run(out, root, c(tvpkg = "2.0"), transit = row)
  before <- .package_rows(out, "tvpkg")
  expect_true("1.0-1" %in% before[[paste(DB_FILENAME, SUMMARY_TABLE)]]$version)

  .tv_release(src, "tvpkg", "2.1", "2026-05-01T00:00:00Z", "a")
  bad <- row; bad$sha256 <- strrep("0", 64L)
  .tv_run(out, root, c(tvpkg = "2.1"), transit = bad)
  f <- .vst_query(out, "SELECT stage, reason FROM cran_metrics_failures WHERE package = 'tvpkg'")
  expect_identical(f$stage, "clone")
  expect_match(f$reason, "tvpkg 1.0-1", fixed = TRUE)
  expect_identical(.package_rows(out, "tvpkg"), before)
})
