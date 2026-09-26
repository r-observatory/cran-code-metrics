# tests/testthat/test-repository-only.R: CRAN reads a release, so a repository
# file the release does not carry is unknown (NULL), never "absent" (0).

.ro_row <- function(pkg, ci_present, has_pkgdown = 0L, ci_type = "[]",
                    ci_matrix_breadth = 0L, ci_pr_gated = 0L) {
  data.frame(package = pkg, version = "1.0", ci_present = ci_present,
             ci_type = ci_type, ci_matrix_breadth = ci_matrix_breadth,
             ci_pr_gated = ci_pr_gated, has_pkgdown = has_pkgdown,
             has_code_of_conduct = 0L, has_contributing_guide = 1L,
             stringsAsFactors = FALSE)
}

.ro_db <- function(frame = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".db", .local_envir = frame)
  con <- open_or_init_db(path)
  withr::defer(DBI::dbDisconnect(con), envir = frame)
  con
}

test_that("a zero becomes NULL, a one stays, and running it again changes nothing", {
  con <- .ro_db()
  DBI::dbWriteTable(con, SUMMARY_TABLE, rbind(
    .ro_row("none", 0L),
    .ro_row("ci", 1L, has_pkgdown = 1L, ci_type = '["github-actions"]',
            ci_matrix_breadth = 4L, ci_pr_gated = 1L)))
  .null_repository_only_columns(con)
  got <- DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s" ORDER BY package', SUMMARY_TABLE))
  ci <- got[got$package == "ci", ]
  none <- got[got$package == "none", ]
  expect_identical(ci$ci_present, 1L)
  expect_identical(ci$has_pkgdown, 1L)
  expect_identical(ci$ci_type, '["github-actions"]')
  expect_identical(ci$ci_matrix_breadth, 4L)
  expect_true(is.na(ci$has_code_of_conduct))
  expect_true(all(is.na(unlist(none[c("ci_present", "has_pkgdown", "has_code_of_conduct",
                                      "ci_type", "ci_matrix_breadth", "ci_pr_gated")]))))
  expect_identical(none$has_contributing_guide, 1L)
  expect_identical(.null_repository_only_columns(con), 0L)
})

test_that("every shard leaves the table in the release reading, old rows included", {
  con <- .ro_db()
  DBI::dbWriteTable(con, SUMMARY_TABLE, .ro_row("old", 0L))
  upsert_shard(con, .ro_row("new", 0L), churn_df = .empty_churn(), api_df = .empty_api())
  got <- DBI::dbGetQuery(con, sprintf('SELECT package, ci_present, has_pkgdown FROM "%s"',
                                      SUMMARY_TABLE))
  expect_true(all(is.na(got$ci_present)))
  expect_true(all(is.na(got$has_pkgdown)))
})

test_that("a table without these columns is left alone", {
  con <- .ro_db()
  DBI::dbWriteTable(con, SUMMARY_TABLE, data.frame(package = "p", version = "1.0",
                                                   stringsAsFactors = FALSE))
  expect_identical(.null_repository_only_columns(con), 0L)
})

test_that("an R-fallback row reads the repository-only metrics the release way", {
  m <- .null_repository_only_metrics(list(
    loc_r = 10L, ci_present = FALSE, ci_type = "[]", ci_matrix_breadth = 0L,
    ci_pr_gated = FALSE, has_pkgdown = FALSE, has_code_of_conduct = TRUE,
    has_contributing_guide = NA))
  expect_identical(m$loc_r, 10L)
  expect_true(is.na(m$ci_present))
  expect_true(is.na(m$ci_type))
  expect_true(is.na(m$ci_matrix_breadth))
  expect_true(is.na(m$ci_pr_gated))
  expect_true(is.na(m$has_pkgdown))
  expect_true(m$has_code_of_conduct)
  expect_true(is.na(m$has_contributing_guide))
})
