cit_db <- function() {
  p <- withr::local_tempfile(.local_envir = parent.frame(), fileext = ".db")
  con <- open_or_init_db(p)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  con
}

cit_sum <- function(pkg, ver) {
  data.frame(package = pkg, version = ver, n_files = 1L, stringsAsFactors = FALSE)
}

cit_one <- function(pkg, ver, pid, status = "ok") {
  data.frame(package = pkg, version = ver, is_current = 1L, payload_id = pid,
             source_sha256 = strrep("a", 64L), status = status,
             sandboxed = 1L, released_known = 1L, message = NA_character_,
             evaluated_at = "2026-08-08T00:00:00Z", stringsAsFactors = FALSE)
}

test_that("the citation tables exist before any citation is ever written", {
  # Created at connection open, not lazily. A first shard where no package ships
  # a citation would otherwise leave the second shard to infer a schema from a
  # data frame, and that inferred table has no primary key.
  con <- cit_db()
  tabs <- DBI::dbListTables(con)
  expect_true(all(c("cran_citations", "cran_citation_payloads",
                    "cran_citation_entries") %in% tabs))
})

test_that("re-analysing a package replaces its citation rows rather than doubling them", {
  con <- cit_db()
  pay <- data.frame(payload_id = "pid1", n_entries = 1L, mheader = NA_character_,
                    mfooter = NA_character_, header_scope = "none",
                    stringsAsFactors = FALSE)
  ent <- data.frame(payload_id = "pid1", entry_index = 1L, bibtype = "Misc",
                    title = "T", year = "2001", authors_json = "[]",
                    fields_json = "{}", text_version_json = "[]",
                    header = NA_character_, footer = NA_character_,
                    entry_key = NA_character_, fmt_bibtex = "@Misc{,}",
                    fmt_citation = "T", stringsAsFactors = FALSE)

  upsert_shard(con, cit_sum("p", "1.0"), .empty_churn(), .empty_api(),
               citations_df = cit_one("p", "1.0", "pid1"),
               payloads_df = pay, entries_df = ent)
  upsert_shard(con, cit_sum("p", "2.0"), .empty_churn(), .empty_api(),
               citations_df = cit_one("p", "2.0", "pid1"),
               payloads_df = pay, entries_df = ent)

  n <- DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_citations")$n
  expect_equal(n, 1L)
  v <- DBI::dbGetQuery(con, "SELECT version FROM cran_citations")$version
  expect_equal(v, "2.0")

  # The payload is shared, so it is inserted once and never deleted by package.
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_citation_payloads")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_citation_entries")$n, 1L)
})

test_that("a package that stops shipping a citation loses its rows", {
  con <- cit_db()
  upsert_shard(con, cit_sum("p", "1.0"), .empty_churn(), .empty_api(),
               citations_df = cit_one("p", "1.0", "pid1"),
               payloads_df = .empty_citation_payloads_df(),
               entries_df = .empty_citation_entries_df())
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_citations")$n, 1L)

  upsert_shard(con, cit_sum("p", "2.0"), .empty_churn(), .empty_api(),
               citations_df = .empty_citations_df(),
               payloads_df = .empty_citation_payloads_df(),
               entries_df = .empty_citation_entries_df())
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_citations")$n, 0L)
})
