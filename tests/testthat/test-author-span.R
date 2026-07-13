# tests/testthat/test-author-span.R
#
# Covers the cran_author_package_span projection (scripts/export.R) and the
# vectorised author parser it reuses from scripts/analyze.R. Fully offline:
# small in-memory SQLite fixtures, no network, no tarballs, no re-scan.

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Build an in-memory cran_code_summary from a data.frame and return the con.
.mk_summary_db <- function(df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "cran_code_summary", df, row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE UNIQUE INDEX idx_summary_pkg_ver ON cran_code_summary(package, version)")
  con
}

# Author-array JSON exactly as metrics_meta() emits it via jsonlite::toJSON().
.au <- function(...) {
  people <- list(...)
  as.character(jsonlite::toJSON(people, auto_unbox = TRUE))
}
.person <- function(given = NA_character_, family = NA_character_, roles = character(0L)) {
  list(given = given, family = family, roles = I(roles))
}

alice  <- .person("Alice", "Smith",  "aut")
bob    <- .person("Bob",   "Jones",  "ctb")
carol  <- .person("Carol", "Gone",   "ctb")

# spanpkg: Alice is there from 1.0; Bob is ADDED in 2.0 (the bug being fixed);
# Carol is REMOVED after 1.1. otherpkg: Alice again, from an earlier date, with
# a noisy display form ("  ALICE ") that must normalise onto the same key.
.span_fixture <- function() {
  data.frame(
    package = c("spanpkg", "spanpkg", "spanpkg", "otherpkg", "otherpkg"),
    version = c("1.0",     "1.1",     "2.0",     "0.1",      "0.2"),
    released = c("2010-01-01", "2012-05-01", "2024-03-01",
                 "2006-07-01", "2007-07-01"),
    authors = c(
      .au(alice, carol),
      .au(alice, carol),
      .au(alice, bob),
      .au(.person("  ALICE ", "smith", "aut")),
      .au(.person("Alice", "Smith", "cre"))
    ),
    # A wide-table column the projection must never need to read.
    loc_r = c(10L, 20L, 30L, 1L, 2L),
    stringsAsFactors = FALSE
  )
}

.spans <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT * FROM cran_author_package_span ORDER BY author_key, package")
}
.span_of <- function(con, key, package) {
  DBI::dbGetQuery(con,
    "SELECT * FROM cran_author_package_span WHERE author_key = ? AND package = ?",
    params = list(key, package))
}

# ===========================================================================
# Schema
# ===========================================================================

test_that("cran_author_package_span is WITHOUT ROWID with PK(author_key, package)", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  sql <- DBI::dbGetQuery(con,
    "SELECT sql FROM sqlite_master
      WHERE type='table' AND name='cran_author_package_span'")$sql
  expect_match(sql, "WITHOUT ROWID")
  expect_match(sql, "PRIMARY KEY \\(author_key, package\\)")

  expect_setequal(
    DBI::dbListFields(con, "cran_author_package_span"),
    c("author_key", "package", "given", "family", "first_version", "first_seen",
      "last_version", "last_seen", "n_versions"))
})

test_that("the package index exists for the package-page lookup", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  idx <- DBI::dbGetQuery(con,
    "SELECT name, sql FROM sqlite_master
      WHERE type='index' AND tbl_name='cran_author_package_span'")
  expect_true("idx_caps_package" %in% idx$name)
  expect_match(idx$sql[idx$name == "idx_caps_package"],
               "cran_author_package_span\\s*\\(package\\)")
})

# ===========================================================================
# Spans: joined, stayed, left
# ===========================================================================

test_that("an author present from v1 has first_seen = v1's release date", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  a <- .span_of(con, "alice smith", "spanpkg")
  expect_equal(nrow(a), 1L)
  expect_equal(a$first_version, "1.0")
  expect_equal(a$first_seen,    "2010-01-01")
  expect_equal(a$last_version,  "2.0")
  expect_equal(a$last_seen,     "2024-03-01")
  expect_equal(a$n_versions,    3L)
})

test_that("an author ADDED later starts at THAT version, not the package's first", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  b <- .span_of(con, "bob jones", "spanpkg")
  expect_equal(nrow(b), 1L)
  expect_equal(b$first_version, "2.0")
  expect_equal(b$first_seen,    "2024-03-01")   # NOT 2010-01-01, the bug fixed
  expect_equal(b$last_version,  "2.0")
  expect_equal(b$last_seen,     "2024-03-01")
  expect_equal(b$n_versions,    1L)
})

test_that("an author REMOVED stops at their last version, before the package's latest", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  c1 <- .span_of(con, "carol gone", "spanpkg")
  expect_equal(nrow(c1), 1L)
  expect_equal(c1$first_version, "1.0")
  expect_equal(c1$first_seen,    "2010-01-01")
  expect_equal(c1$last_version,  "1.1")
  expect_equal(c1$last_seen,     "2012-05-01")  # earlier than 2.0 / 2024-03-01
  expect_equal(c1$n_versions,    2L)

  latest <- DBI::dbGetQuery(con,
    "SELECT MAX(released) m FROM cran_code_summary WHERE package='spanpkg'")$m
  expect_true(c1$last_seen < latest)
})

test_that("n_versions counts only the versions that list the author", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  n <- DBI::dbGetQuery(con,
    "SELECT author_key, n_versions FROM cran_author_package_span
      WHERE package='spanpkg' ORDER BY author_key")
  expect_equal(n$author_key, c("alice smith", "bob jones", "carol gone"))
  expect_equal(n$n_versions, c(3L, 1L, 2L))
})

test_that("one row per (author, package) -- an author spans several packages", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  rows <- DBI::dbGetQuery(con,
    "SELECT package, first_seen FROM cran_author_package_span
      WHERE author_key='alice smith' ORDER BY package")
  expect_equal(rows$package,    c("otherpkg", "spanpkg"))
  expect_equal(rows$first_seen, c("2006-07-01", "2010-01-01"))

  # The author page's query: earliest appearance anywhere.
  on_cran_since <- DBI::dbGetQuery(con,
    "SELECT MIN(first_seen) AS since FROM cran_author_package_span
      WHERE author_key = ?", params = list("alice smith"))$since
  expect_equal(on_cran_since, "2006-07-01")
})

# ===========================================================================
# author_key normalisation and display form
# ===========================================================================

test_that("author_key is case- and whitespace-insensitive", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  # otherpkg lists "  ALICE " / "smith" then "Alice" / "Smith": one identity.
  a <- .span_of(con, "alice smith", "otherpkg")
  expect_equal(nrow(a), 1L)
  expect_equal(a$n_versions, 2L)
  expect_equal(a$first_version, "0.1")
  expect_equal(a$last_version,  "0.2")

  # The key matches the identity normalisation used by authors_added_later.
  expect_equal(.xv_author_identities(.au(.person("  ALICE ", "smith"))),
               "alice smith")
})

test_that("given/family are stored in the display form of the MOST RECENT version", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  a <- .span_of(con, "alice smith", "otherpkg")
  expect_equal(a$given,  "Alice")     # from 0.2, not "  ALICE " from 0.1
  expect_equal(a$family, "Smith")     # from 0.2, not "smith" from 0.1
})

test_that("a null given keeps the identity and stores NA as the display form", {
  df <- data.frame(
    package  = c("nullpkg", "nullpkg"),
    version  = c("1.0", "1.1"),
    released = c("2019-01-01", "2020-01-01"),
    authors  = c(.au(.person(NA_character_, "Lemaire")),
                 .au(.person(NA_character_, "Lemaire"))),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  row <- .spans(con)
  expect_equal(nrow(row), 1L)
  expect_equal(row$author_key, " lemaire")   # lower(trim("")) + " " + family
  expect_true(is.na(row$given))
  expect_equal(row$family, "Lemaire")
  expect_equal(row$n_versions, 2L)
})

test_that("an entry with neither given nor family is not an identity", {
  df <- data.frame(
    package  = "namelesspkg",
    version  = "1.0",
    released = "2020-01-01",
    authors  = .au(.person(NA_character_, NA_character_), .person("Real", "Person")),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  expect_equal(.spans(con)$author_key, "real person")
})

# ===========================================================================
# Ordering
# ===========================================================================

test_that("versions are ordered by released date, then numeric_version (1.10 > 1.9)", {
  df <- data.frame(
    package  = c("tiepkg", "tiepkg", "tiepkg"),
    version  = c("1.10", "1.9", "1.2"),           # inserted out of order
    released = c("2015-01-01", "2015-01-01", "2015-01-01"),  # same day: tie
    authors  = c(.au(alice, bob), .au(alice), .au(alice)),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  a <- .span_of(con, "alice smith", "tiepkg")
  expect_equal(a$first_version, "1.2")    # 1.2 < 1.9 < 1.10 numerically
  expect_equal(a$last_version,  "1.10")   # string max would be "1.9"
  expect_equal(a$n_versions,    3L)

  b <- .span_of(con, "bob jones", "tiepkg")
  expect_equal(b$first_version, "1.10")
  expect_equal(b$last_version,  "1.10")
})

test_that("release date wins over version number when they disagree", {
  # 2.0 was released BEFORE 1.5 (a back-port); `released` is authoritative.
  df <- data.frame(
    package  = c("backport", "backport"),
    version  = c("2.0", "1.5"),
    released = c("2020-01-01", "2021-06-01"),
    authors  = c(.au(alice), .au(alice, bob)),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  a <- .span_of(con, "alice smith", "backport")
  expect_equal(a$first_version, "2.0")
  expect_equal(a$first_seen,    "2020-01-01")
  expect_equal(a$last_version,  "1.5")
  expect_equal(a$last_seen,     "2021-06-01")

  b <- .span_of(con, "bob jones", "backport")
  expect_equal(b$first_version, "1.5")
  expect_equal(b$first_seen,    "2021-06-01")
})

# ===========================================================================
# Robustness: malformed / missing input must not abort the projection
# ===========================================================================

test_that("malformed, empty and NULL authors JSON do not abort the projection", {
  df <- data.frame(
    package  = c("badpkg", "badpkg", "badpkg", "badpkg", "goodpkg"),
    version  = c("1.0", "1.1", "1.2", "1.3", "1.0"),
    released = c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01",
                 "2018-05-01"),
    authors  = c(NA_character_,                 # NULL in SQLite
                 "",                            # empty string
                 '[{"given":"Trunc","family":', # truncated JSON
                 .au(alice),                    # one good version
                 "not json at all"),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_no_error(project_author_spans(con))

  rows <- .spans(con)
  expect_equal(nrow(rows), 1L)                  # only the good version's author
  expect_equal(rows$author_key,    "alice smith")
  expect_equal(rows$package,       "badpkg")
  expect_equal(rows$first_version, "1.3")       # the malformed rows contribute none
  expect_equal(rows$n_versions,    1L)
})

test_that("a package-version with no released date still projects", {
  df <- data.frame(
    package  = c("nodate", "nodate"),
    version  = c("1.0", "1.1"),
    released = c("2020-01-01", NA_character_),
    authors  = c(.au(alice), .au(alice)),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  a <- .span_of(con, "alice smith", "nodate")
  expect_equal(a$first_version, "1.0")          # dated rows sort first
  expect_equal(a$first_seen,    "2020-01-01")
  expect_equal(a$last_version,  "1.1")          # undated row sorts last
  expect_true(is.na(a$last_seen))
  expect_equal(a$n_versions, 2L)
})

test_that("the table is created even when cran_code_summary is absent or empty", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_no_error(project_author_spans(con))    # no cran_code_summary at all
  expect_true("cran_author_package_span" %in% DBI::dbListTables(con))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_author_package_span")$n, 0L)

  DBI::dbWriteTable(con, "cran_code_summary",
    data.frame(package = character(0L), version = character(0L),
               released = character(0L), authors = character(0L),
               stringsAsFactors = FALSE))
  expect_no_error(project_author_spans(con))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_author_package_span")$n, 0L)
})

test_that("a summary without an authors column is tolerated", {
  df <- data.frame(package = "old", version = "1.0", released = "2011-01-01",
                   stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_no_error(project_author_spans(con))
  expect_true("cran_author_package_span" %in% DBI::dbListTables(con))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cran_author_package_span")$n, 0L)
})

test_that("a legacy `date` column is used when `released` is absent", {
  df <- data.frame(
    package = c("legacy", "legacy"),
    version = c("1.0", "1.1"),
    date    = c("2013-01-01", "2014-01-01"),
    authors = c(.au(alice), .au(alice, bob)),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  expect_equal(.span_of(con, "alice smith", "legacy")$first_seen, "2013-01-01")
  expect_equal(.span_of(con, "bob jones",  "legacy")$first_seen, "2014-01-01")
})

# ===========================================================================
# Rebuild semantics
# ===========================================================================

test_that("re-running the projection is idempotent (no duplicates, same rows)", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)
  once <- .spans(con)
  project_author_spans(con)
  twice <- .spans(con)

  expect_equal(twice, once)
  expect_equal(nrow(twice), 4L)                 # 3 in spanpkg + 1 in otherpkg
  expect_equal(anyDuplicated(paste(twice$author_key, twice$package)), 0L)
})

test_that("the rebuild drops rows whose source versions are gone", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)
  expect_equal(nrow(.span_of(con, "bob jones", "spanpkg")), 1L)

  # Bob's only version disappears from the source table.
  DBI::dbExecute(con,
    "DELETE FROM cran_code_summary WHERE package='spanpkg' AND version='2.0'")
  project_author_spans(con)

  expect_equal(nrow(.span_of(con, "bob jones", "spanpkg")), 0L)
  a <- .span_of(con, "alice smith", "spanpkg")
  expect_equal(a$last_version, "1.1")
  expect_equal(a$n_versions,   2L)
})

test_that("chunking over packages does not change the result", {
  con <- .mk_summary_db(.span_fixture())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con, chunk_size = 500L)
  whole <- .spans(con)
  project_author_spans(con, chunk_size = 1L)    # one package per batch
  chunked <- .spans(con)

  expect_equal(chunked, whole)
})

# ===========================================================================
# The vectorised parser behind the projection
# ===========================================================================

test_that(".xv_author_pairs reads a whole authors column in one pass", {
  x <- c(.au(alice, bob), NA_character_, .au(carol), "", "garbage")
  p <- .xv_author_pairs(x)

  expect_equal(p$row,        c(1L, 1L, 3L))
  expect_equal(p$given,      c("Alice", "Bob", "Carol"))
  expect_equal(p$family,     c("Smith", "Jones", "Gone"))
  expect_equal(p$author_key, c("alice smith", "bob jones", "carol gone"))
  expect_equal(nrow(.xv_author_pairs(character(0L))), 0L)
})

test_that(".xv_author_pairs agrees with the per-row jsonlite parser", {
  x <- c(
    .au(.person("José", "Muñoz", "aut")),                 # non-ASCII, literal UTF-8
    .au(.person('A"B', "O'Neil", c("aut", "cre"))),       # escaped quote in a value
    .au(.person(NA_character_, "Lemaire")),               # null given
    .au(.person("Given", NA_character_)),                 # null family
    '[{"family":"Reversed","given":"Key","roles":[]}]'    # unexpected key order
  )
  fast <- .xv_author_pairs(x)
  slow <- unlist(lapply(x, .xv_author_identities), use.names = FALSE)

  expect_equal(fast$author_key, slow)
  expect_equal(fast$given[1L],  "José")
  expect_equal(fast$family[2L], "O'Neil")
  expect_equal(fast$given[2L],  'A"B')
  expect_true(is.na(fast$given[3L]))
  expect_true(is.na(fast$family[4L]))
  expect_equal(fast$author_key[5L], "key reversed")       # jsonlite fallback path
})

test_that(".version_sort_key orders exactly like numeric_version()", {
  v <- c("1.9", "1.10", "1.2", "0.99.9", "1.0", "2.0-1", "2.0.1", "1.0.0",
         "0.1-20230101", "10.0", "9.9")
  expect_equal(
    v[order(.version_sort_key(v), method = "radix")],
    v[order(numeric_version(v))])

  # "-" and "." are the same separator, as numeric_version() has it.
  expect_equal(.version_sort_key("2.0-1"), .version_sort_key("2.0.1"))
  # NA in, NA out (order() then sorts it last).
  expect_true(is.na(.version_sort_key(NA_character_)))
})

test_that("an author listed twice in one DESCRIPTION counts as one version", {
  df <- data.frame(
    package  = c("dup", "dup"),
    version  = c("1.0", "1.1"),
    released = c("2020-01-01", "2021-01-01"),
    authors  = c(.au(alice, .person("alice", "SMITH", "cre")), .au(alice)),
    stringsAsFactors = FALSE)
  con <- .mk_summary_db(df)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  project_author_spans(con)

  a <- .span_of(con, "alice smith", "dup")
  expect_equal(nrow(a), 1L)
  expect_equal(a$n_versions, 2L)                # 2 versions, not 3 listings
})
