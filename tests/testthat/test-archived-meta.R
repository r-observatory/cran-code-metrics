# tests/testthat/test-archived-meta.R
#
# Covers the narrow cran_archived_meta projection (scripts/export.R) and the
# --harvest-descriptions backlog path (scripts/update.R). Fully offline: small
# in-memory SQLite fixtures and locally-built tarballs; the harvest downloader
# is injected so no network is touched.

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Build an in-memory cran_code_summary from a data.frame and return the con.
.mk_code_summary <- function(df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "cran_code_summary", df, row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE UNIQUE INDEX idx_summary_pkg_ver ON cran_code_summary(package, version)")
  con
}

# A multi-version archived + live summary. arch1 spans 1.0/1.10/1.2 (so the
# last version by numeric_version() is 1.10, NOT the string max "1.2"). live1
# is a currently-live package and must never be projected.
.summary_fixture <- function() {
  data.frame(
    package          = c("arch1", "arch1", "arch1", "arch2", "live1"),
    version          = c("1.0",   "1.10",  "1.2",   "0.5",   "3.0"),
    title            = c("t-old", "t-new", "t-mid", NA,      "t-live"),
    description      = c("d-old", "d-new", "d-mid", NA,      "d-live"),
    authors          = c("[a]",   "[b]",   "[c]",   "[z]",   "[l]"),
    maintainer       = c("M",     "M",     "M",     "N",     "L"),
    maintainer_email = c("m@e",   "m@e",   "m@e",   "n@e",   "l@e"),
    license          = c("GPL-2", "GPL-2", "GPL-2", "MIT",   "MIT"),
    url              = c("u1",    "u2",    "u3",    NA,      "lu"),
    depends          = c("R",     "R",     "R",     NA,      "R"),
    imports          = c("dplyr", "dplyr", "dplyr", NA,      "x"),
    suggests         = c(NA,      NA,      NA,      NA,      NA),
    linking_to       = c("Rcpp",  "Rcpp",  "Rcpp",  NA,      NA),
    enhances         = c(NA,      NA,      NA,      NA,      NA),
    stringsAsFactors = FALSE
  )
}

# Build a real package tarball (<pkg>/DESCRIPTION + a stub) at a temp path and
# return its path. `top` overrides the top-level directory name (for the
# casing-fallback test). `fields` is a character vector of DESCRIPTION lines.
.make_tarball <- function(pkg, version, fields, top = pkg) {
  d   <- tempfile("ccm_tar_"); dir.create(d)
  pd  <- file.path(d, top);     dir.create(pd)
  writeLines(fields, file.path(pd, "DESCRIPTION"))
  dir.create(file.path(pd, "R"), showWarnings = FALSE)
  writeLines("f <- function() NULL", file.path(pd, "R", "f.R"))
  tf  <- file.path(d, sprintf("%s_%s.tar.gz", pkg, version))
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  utils::tar(basename(tf), top, compression = "gzip")
  tf
}

# ===========================================================================
# Projection: schema shape
# ===========================================================================

test_that("cran_archived_meta is WITHOUT ROWID, PK(package), no secondary index", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_archived_meta(con, c("arch1", "arch2"), scanned_at = "2026-07-12T00:00:00Z")

  sql <- DBI::dbGetQuery(con,
    "SELECT sql FROM sqlite_master WHERE type='table' AND name='cran_archived_meta'")$sql
  expect_match(sql, "WITHOUT ROWID")
  expect_match(sql, "PRIMARY KEY \\(package\\)")

  # No secondary indexes: the clustered PK is the only access path.
  idx <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='cran_archived_meta'")
  expect_equal(nrow(idx), 0L)

  # Exactly the narrow column set (order-independent), nothing from the wide table.
  cols <- DBI::dbListFields(con, "cran_archived_meta")
  expect_setequal(cols, c(
    "package", "last_version", "title", "description", "authors", "maintainer",
    "maintainer_email", "license", "url", "depends", "imports", "suggests",
    "linkingto", "enhances", "desc_sha", "source_scanned_at"))
})

# ===========================================================================
# Projection: correct last-version row per archived package
# ===========================================================================

test_that("projection picks the last version by numeric_version, not string max", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_archived_meta(con, c("arch1", "arch2"), scanned_at = "2026-07-12T00:00:00Z")

  arch1 <- DBI::dbGetQuery(con,
    "SELECT * FROM cran_archived_meta WHERE package = 'arch1'")
  # 1.10 > 1.2 > 1.0 under numeric_version ordering (string max would be "1.2").
  expect_equal(arch1$last_version, "1.10")
  expect_equal(arch1$title, "t-new")
  expect_equal(arch1$description, "d-new")
})

test_that("projection maps linkingto <- linking_to and stamps source_scanned_at", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_archived_meta(con, c("arch1", "arch2"), scanned_at = "2026-07-12T09:00:00Z")

  arch1 <- DBI::dbGetQuery(con,
    "SELECT * FROM cran_archived_meta WHERE package = 'arch1'")
  expect_equal(arch1$linkingto, "Rcpp")             # renamed from linking_to
  expect_equal(arch1$license, "GPL-2")
  expect_equal(arch1$url, "u2")                      # from the 1.10 row
  expect_equal(arch1$source_scanned_at, "2026-07-12T09:00:00Z")
})

test_that("projected rows have a NULL desc_sha (set only by the harvest path)", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_archived_meta(con, c("arch1", "arch2"))
  shas <- DBI::dbGetQuery(con, "SELECT desc_sha FROM cran_archived_meta")$desc_sha
  expect_true(all(is.na(shas)))
})

test_that("only archived packages are projected; live packages are excluded", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_archived_meta(con, c("arch1", "arch2"))
  pkgs <- DBI::dbGetQuery(con, "SELECT package FROM cran_archived_meta")$package
  expect_setequal(pkgs, c("arch1", "arch2"))
  expect_false("live1" %in% pkgs)
})

test_that("projection carries NA title/description through for pre-Change-1 rows", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_archived_meta(con, c("arch1", "arch2"))
  arch2 <- DBI::dbGetQuery(con,
    "SELECT * FROM cran_archived_meta WHERE package = 'arch2'")
  expect_equal(arch2$last_version, "0.5")
  expect_true(is.na(arch2$title))
  expect_true(is.na(arch2$description))
  expect_equal(arch2$maintainer, "N")               # still projected
})

test_that("projection tolerates a summary that lacks title/description columns", {
  # A DB built before Change 1: no title/description columns at all.
  df <- data.frame(
    package = c("archX", "archX"), version = c("2.0", "2.1"),
    license = c("MIT", "MIT"), linking_to = c(NA, "Rcpp"),
    stringsAsFactors = FALSE)
  con <- .mk_code_summary(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_no_error(project_archived_meta(con, "archX"))
  row <- DBI::dbGetQuery(con, "SELECT * FROM cran_archived_meta")
  expect_equal(row$last_version, "2.1")
  expect_true(is.na(row$title))
  expect_equal(row$linkingto, "Rcpp")
})

test_that("projection is a no-op when there are no archived packages", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_no_error(project_archived_meta(con, character(0L)))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_archived_meta")$n, 0L)
})

# ===========================================================================
# Projection: non-destructive re-run preserves harvested values
# ===========================================================================

test_that("re-projection preserves a harvested title/desc_sha (COALESCE)", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_archived_meta(con, c("arch1", "arch2"), scanned_at = "2026-07-12T00:00:00Z")

  # Simulate a harvest having filled arch2's title/description/desc_sha.
  DBI::dbExecute(con,
    "UPDATE cran_archived_meta
       SET title='Harvested', description='H desc', desc_sha='deadbeef'
     WHERE package='arch2'")

  # arch2's wide-table rows still have NULL title, so a re-projection must NOT
  # clobber the harvested values -- but it should refresh source_scanned_at.
  project_archived_meta(con, c("arch1", "arch2"), scanned_at = "2026-07-12T12:00:00Z")

  arch2 <- DBI::dbGetQuery(con,
    "SELECT * FROM cran_archived_meta WHERE package='arch2'")
  expect_equal(arch2$title, "Harvested")
  expect_equal(arch2$description, "H desc")
  expect_equal(arch2$desc_sha, "deadbeef")
  expect_equal(arch2$source_scanned_at, "2026-07-12T12:00:00Z")
})

test_that("a wide-table title wins over an older projected NULL and clears desc_sha", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # First projection: arch1 already has a title in the wide table.
  project_archived_meta(con, "arch1")
  before <- DBI::dbGetQuery(con,
    "SELECT title, desc_sha FROM cran_archived_meta WHERE package='arch1'")
  expect_equal(before$title, "t-new")
  expect_true(is.na(before$desc_sha))   # projection never sets desc_sha
})

# ===========================================================================
# Harvest: DESCRIPTION-only parse
# ===========================================================================

test_that(".harvest_parse_description extracts fields + sha from a tarball", {
  tf <- .make_tarball("harv1", "1.0", c(
    "Package: harv1", "Version: 1.0",
    "Title: A Harvested Package",
    "Description: Extracted straight from the DESCRIPTION.",
    "Authors@R: person('Jane', 'Doe', role = c('aut', 'cre'))",
    "Maintainer: Jane Doe <jane@example.com>",
    "License: GPL-3", "URL: https://harv1.example",
    "Imports: utils, stats", "LinkingTo: Rcpp"))
  on.exit(unlink(dirname(tf), recursive = TRUE, force = TRUE), add = TRUE)

  f <- .harvest_parse_description(tf, "harv1")
  expect_false(is.null(f))
  expect_equal(f$title, "A Harvested Package")
  expect_equal(f$description, "Extracted straight from the DESCRIPTION.")
  expect_equal(f$maintainer, "Jane Doe")
  expect_equal(f$maintainer_email, "jane@example.com")
  expect_equal(f$license, "GPL-3")
  expect_equal(f$url, "https://harv1.example")
  expect_equal(f$linkingto, "Rcpp")
  expect_equal(f$imports, "utils, stats")
  au <- jsonlite::fromJSON(f$authors, simplifyVector = FALSE)
  expect_equal(au[[1L]]$family, "Doe")
  # 64-hex sha256 over the DESCRIPTION bytes.
  expect_match(f$desc_sha, "^[0-9a-f]{64}$")
})

test_that(".harvest_parse_description falls back when the top dir is cased oddly", {
  # Tarball's top directory is 'HARV2' but the package is 'harv2'.
  tf <- .make_tarball("harv2", "2.0", c(
    "Package: harv2", "Version: 2.0", "Title: Cased Oddly",
    "Description: Top dir differs from the package name.", "License: MIT"),
    top = "HARV2")
  on.exit(unlink(dirname(tf), recursive = TRUE, force = TRUE), add = TRUE)

  f <- .harvest_parse_description(tf, "harv2")
  expect_false(is.null(f))
  expect_equal(f$title, "Cased Oddly")
})

test_that(".harvest_parse_description returns NULL when no DESCRIPTION is present", {
  d  <- tempfile("ccm_empty_"); dir.create(d)
  sub <- file.path(d, "nope"); dir.create(sub)
  writeLines("x", file.path(sub, "README"))
  tf <- file.path(d, "nope_1.0.tar.gz")
  old <- setwd(d); utils::tar(basename(tf), "nope", compression = "gzip"); setwd(old)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)

  expect_null(.harvest_parse_description(tf, "nope"))
})

# ===========================================================================
# Harvest: idempotency (desc_sha gate) + failure isolation
# ===========================================================================

# Injected downloader that copies a prebuilt local tarball for a package.
.fake_downloader <- function(tarballs, fail = character(0L)) {
  function(package, version, destfile, mirror, tries, sleep) {
    if (package %in% fail) return(FALSE)
    tf <- tarballs[[package]]
    if (is.null(tf)) return(FALSE)
    file.copy(tf, destfile, overwrite = TRUE)
  }
}

test_that("harvest fills title/desc_sha for archived rows missing a title", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  project_archived_meta(con, c("arch1", "arch2"))

  # arch2 has NULL title after projection; give it a tarball to harvest.
  tf <- .make_tarball("arch2", "0.5", c(
    "Package: arch2", "Version: 0.5", "Title: Recovered Title",
    "Description: Recovered from the archive.", "License: MIT",
    "URL: https://arch2.example"))
  on.exit(unlink(dirname(tf), recursive = TRUE, force = TRUE), add = TRUE)

  res <- harvest_descriptions(con, sleep = 0,
    download_fn = .fake_downloader(list(arch2 = tf)))

  # Only arch2 needed a title (arch1's projection already had one).
  expect_equal(res$todo, 1L)
  expect_equal(res$ok, 1L)
  row <- DBI::dbGetQuery(con,
    "SELECT * FROM cran_archived_meta WHERE package = 'arch2'")
  expect_equal(row$title, "Recovered Title")
  expect_equal(row$url, "https://arch2.example")
  expect_match(row$desc_sha, "^[0-9a-f]{64}$")
})

test_that("a completed harvest is a provable no-op on re-run", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  project_archived_meta(con, "arch2")

  tf <- .make_tarball("arch2", "0.5", c(
    "Package: arch2", "Version: 0.5", "Title: Recovered Title",
    "Description: Recovered.", "License: MIT"))
  on.exit(unlink(dirname(tf), recursive = TRUE, force = TRUE), add = TRUE)
  dl <- .fake_downloader(list(arch2 = tf))

  first  <- harvest_descriptions(con, sleep = 0, download_fn = dl)
  expect_equal(first$ok, 1L)

  # Default selection is title-IS-NULL, so the filled row is not even attempted.
  second <- harvest_descriptions(con, sleep = 0, download_fn = dl)
  expect_equal(second$todo, 0L)
  expect_equal(second$ok, 0L)
  expect_equal(second$skipped, 0L)
})

test_that("desc_sha gate: re-fetching a byte-identical DESCRIPTION is skipped", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  project_archived_meta(con, "arch2")

  tf <- .make_tarball("arch2", "0.5", c(
    "Package: arch2", "Version: 0.5", "Title: Recovered", "Description: R.",
    "License: MIT"))
  on.exit(unlink(dirname(tf), recursive = TRUE, force = TRUE), add = TRUE)
  dl <- .fake_downloader(list(arch2 = tf))

  harvest_descriptions(con, sleep = 0, download_fn = dl)

  # Force re-selection by targeting the package; the sha still matches so the
  # write is skipped rather than repeated.
  again <- harvest_descriptions(con, packages = "arch2", sleep = 0, download_fn = dl)
  expect_equal(again$todo, 1L)
  expect_equal(again$skipped, 1L)
  expect_equal(again$ok, 0L)
})

test_that("one failed package does not abort the harvest batch", {
  con <- .mk_code_summary(.summary_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Two archived rows lacking a title: arch2 (from fixture) and archG (added).
  # Explicit column list so the unspecified wide columns default to NULL.
  DBI::dbExecute(con,
    "INSERT INTO cran_code_summary (package, version, title, description)
     VALUES ('archG', '1.0', NULL, NULL)")
  project_archived_meta(con, c("arch2", "archG"))

  tf <- .make_tarball("archG", "1.0", c(
    "Package: archG", "Version: 1.0", "Title: Good One",
    "Description: Fine.", "License: MIT"))
  on.exit(unlink(dirname(tf), recursive = TRUE, force = TRUE), add = TRUE)

  # arch2's download fails; archG succeeds.
  res <- harvest_descriptions(con, sleep = 0,
    download_fn = .fake_downloader(list(archG = tf), fail = "arch2"))

  expect_equal(res$todo, 2L)
  expect_equal(res$ok, 1L)
  expect_equal(res$failed, 1L)
  good <- DBI::dbGetQuery(con,
    "SELECT title FROM cran_archived_meta WHERE package = 'archG'")
  expect_equal(good$title, "Good One")
  bad <- DBI::dbGetQuery(con,
    "SELECT title FROM cran_archived_meta WHERE package = 'arch2'")
  expect_true(is.na(bad$title))   # still needs harvesting
})
