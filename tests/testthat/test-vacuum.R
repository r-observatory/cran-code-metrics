# Tests for the space a delete does not give back.
#
# SQLite hands a deleted page to the database's own free list, not to the
# filesystem, so a database that rewrites rows every run stays at its
# high-water mark forever. Both published databases do exactly that (the
# dataset side deletes and re-inserts every re-scanned package), and the code
# database is close enough to the workflow's publish refusal that the pages
# nobody is using are what stands between a run and a release it can upload.

# Grow `path` by roughly `mb` MiB and then drop the table that holds it, which
# leaves the file at its new size with every one of those pages on the free
# list. That is the shape the real databases are in, in miniature.
.bloat_db <- function(path, mb = 4L) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE junk (id INTEGER, payload TEXT)")
  n <- as.integer(mb) * 256L
  DBI::dbAppendTable(con, "junk", data.frame(
    id = seq_len(n), payload = rep(strrep("x", 4096L), n),
    stringsAsFactors = FALSE))
  DBI::dbExecute(con, "DROP TABLE junk")
  invisible(NULL)
}

test_that("a delete does not shrink the file and a vacuum does", {
  path <- withr::local_tempfile(fileext = ".db")
  .bloat_db(path, mb = 4L)
  before <- as.numeric(file.info(path)$size)
  expect_gt(before, 4 * 1024^2)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- vacuum_db(con, path, min_reclaim = 0)

  expect_true(res$ran)
  expect_gt(res$reclaimed, 3 * 1024^2)
  expect_equal(res$before, before)
  expect_equal(res$after, as.numeric(file.info(path)$size))
  expect_lt(as.numeric(file.info(path)$size), before / 2)
})

test_that("a free list too small to matter is left alone", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (1)")
  before <- as.numeric(file.info(path)$size)

  res <- vacuum_db(con, path)

  expect_false(res$ran)
  expect_equal(res$reclaimed, 0)
  expect_match(res$reason, "free list")
  expect_equal(as.numeric(file.info(path)$size), before)
})

test_that("a disk that cannot hold the copy skips the vacuum instead of failing", {
  path <- withr::local_tempfile(fileext = ".db")
  .bloat_db(path, mb = 4L)
  before <- as.numeric(file.info(path)$size)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- expect_no_error(vacuum_db(con, path, min_reclaim = 0, free_bytes = 1024))

  expect_false(res$ran)
  expect_equal(res$reclaimed, 0)
  expect_match(res$reason, "disk")
  expect_equal(as.numeric(file.info(path)$size), before)
})

test_that("an unmeasurable disk does not stop the reclaim", {
  path <- withr::local_tempfile(fileext = ".db")
  .bloat_db(path, mb = 4L)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- vacuum_db(con, path, min_reclaim = 0, free_bytes = NA_real_)
  expect_true(res$ran)
  expect_gt(res$reclaimed, 0)
})

test_that("free_disk_bytes measures a real directory and refuses to guess at one that is not", {
  free <- free_disk_bytes(tempdir())
  expect_true(is.numeric(free) && length(free) == 1L)
  expect_gt(free, 0)
  expect_true(is.na(free_disk_bytes(file.path(tempdir(), "no-such-directory-here"))))
})

# ---------------------------------------------------------------------------
# The baseline the retention guard compares against has to be restated in the
# same units, or the reclaim reads as a loss.
# ---------------------------------------------------------------------------

test_that("crediting a reclaim lowers the baseline by exactly what was reclaimed", {
  path <- withr::local_tempfile(fileext = ".json")
  write_manifest(path, list(
    schema_version = 1L, series = "code", db_filename = DB_FILENAME,
    db_bytes = 1837748224, fingerprint = strrep("a", 64L),
    n_packages = 33282L, n_versions = 207463L,
    tables = list(cran_code_summary = 207463L)))

  expect_true(credit_reclaim_to_baseline(path, 630000000))

  after <- read_manifest_file(path)
  expect_equal(after$db_bytes, 1837748224 - 630000000)
  # Everything else the guard reads has to survive the rewrite untouched.
  expect_equal(after$n_packages, 33282L)
  expect_equal(after$tables$cran_code_summary, 207463L)
  expect_equal(after$series, "code")
})

test_that("crediting nothing rewrites nothing", {
  path <- withr::local_tempfile(fileext = ".json")
  write_manifest(path, list(schema_version = 1L, series = "code",
                            db_bytes = 100, n_packages = 1L))
  expect_false(credit_reclaim_to_baseline(path, 0))
  expect_equal(read_manifest_file(path)$db_bytes, 100)

  # A baseline that does not exist is the cold-start case, not an error.
  expect_false(credit_reclaim_to_baseline(
    file.path(dirname(path), "absent-manifest.json"), 100))
})

test_that("a credited baseline still refuses a run that lost more than it reclaimed", {
  prior <- list(schema_version = 1L, series = "code",
                db_bytes = 1000, n_packages = 10L, n_versions = 100L,
                tables = list(cran_code_summary = 100L))
  path <- withr::local_tempfile(fileext = ".json")
  write_manifest(path, prior)
  credit_reclaim_to_baseline(path, 300)
  credited <- read_manifest_file(path)
  expect_equal(credited$db_bytes, 700)

  # The reclaimed 300 bytes are excused; a file that came back at half of what
  # remains is not.
  ok  <- utils::modifyList(prior, list(db_bytes = 700))
  bad <- utils::modifyList(prior, list(db_bytes = 350))
  expect_identical(retention_violations("code", ok, credited), character(0L))
  expect_true(any(grepl("db_bytes", retention_violations("code", bad, credited))))
})
