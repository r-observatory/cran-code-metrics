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
             released_known = 1L, message = NA_character_,
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

cit_pay <- function(pid, mheader = NA_character_) {
  data.frame(payload_id = pid, n_entries = 0L, mheader = mheader,
             mfooter = NA_character_, header_scope = "none",
             stringsAsFactors = FALSE)
}

test_that(".upsert_ignore tolerates a duplicate primary key silently, without overwriting", {
  con <- cit_db()
  .upsert_ignore(con, "cran_citation_payloads", cit_pay("pidX", "first"), "payload_id")
  expect_no_error(
    .upsert_ignore(con, "cran_citation_payloads", cit_pay("pidX", "second"), "payload_id"))

  got <- DBI::dbGetQuery(con, "SELECT * FROM cran_citation_payloads")
  expect_equal(nrow(got), 1L)
  # The second writer of the same key is tolerated, not applied: the first
  # writer's content survives.
  expect_equal(got$mheader, "first")
})

test_that(".upsert_ignore raises on a NOT NULL violation rather than dropping the row", {
  # header_scope is NOT NULL on cran_citation_payloads. INSERT OR IGNORE would
  # have swallowed this the same way it swallows a duplicate key; ON CONFLICT
  # DO NOTHING must not, because the citation row that names this payload_id
  # would then point at a payload that was never actually written.
  con <- cit_db()
  bad_pay <- data.frame(payload_id = "pidBad", n_entries = 0L,
                        mheader = NA_character_, mfooter = NA_character_,
                        header_scope = NA_character_, stringsAsFactors = FALSE)
  expect_error(.upsert_ignore(con, "cran_citation_payloads", bad_pay, "payload_id"))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_citation_payloads")$n, 0L)
})

test_that(".upsert_ignore raises on a NOT NULL violation on the entries table too", {
  # bibtype is NOT NULL on cran_citation_entries; same hazard, other table.
  con <- cit_db()
  bad_ent <- data.frame(payload_id = "pidBad2", entry_index = 1L,
                        bibtype = NA_character_, title = NA_character_,
                        year = NA_character_, authors_json = "[]",
                        fields_json = "{}", text_version_json = "[]",
                        header = NA_character_, footer = NA_character_,
                        entry_key = NA_character_, fmt_bibtex = NA_character_,
                        fmt_citation = NA_character_, stringsAsFactors = FALSE)
  expect_error(.upsert_ignore(con, "cran_citation_entries", bad_ent,
                              c("payload_id", "entry_index")))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM cran_citation_entries")$n, 0L)
})

test_that(".gc_citation_payloads reclaims payloads a re-analysed package no longer points at", {
  con <- cit_db()
  for (v in c("1.0", "2.0", "3.0")) {
    pid <- paste0("pid_", v)
    upsert_shard(con, cit_sum("p", v), .empty_churn(), .empty_api(),
                 citations_df = cit_one("p", v, pid),
                 payloads_df = cit_pay(pid), entries_df = .empty_citation_entries_df())
  }

  # Only the payload the surviving (v3.0) citation row points at remains; the
  # two payloads from earlier re-analyses were orphaned and reclaimed inside
  # the same transaction that wrote v3.0, not left to accumulate.
  pays <- DBI::dbGetQuery(con, "SELECT payload_id FROM cran_citation_payloads")$payload_id
  expect_equal(pays, "pid_3.0")
})

test_that(".gc_citation_payloads does not collect a payload another package still points at", {
  con <- cit_db()
  shared <- cit_pay("pid_shared")

  upsert_shard(con, cit_sum("p", "1.0"), .empty_churn(), .empty_api(),
               citations_df = cit_one("p", "1.0", "pid_shared"),
               payloads_df = shared, entries_df = .empty_citation_entries_df())
  upsert_shard(con, cit_sum("q", "1.0"), .empty_churn(), .empty_api(),
               citations_df = cit_one("q", "1.0", "pid_shared"),
               payloads_df = shared, entries_df = .empty_citation_entries_df())

  # p re-analyses onto a different citation; q is untouched by this shard and
  # still points at the shared payload, so GC (triggered by p's write) must
  # not remove it.
  upsert_shard(con, cit_sum("p", "2.0"), .empty_churn(), .empty_api(),
               citations_df = cit_one("p", "2.0", "pid_new"),
               payloads_df = cit_pay("pid_new"), entries_df = .empty_citation_entries_df())

  pays <- sort(DBI::dbGetQuery(con, "SELECT payload_id FROM cran_citation_payloads")$payload_id)
  expect_equal(pays, sort(c("pid_shared", "pid_new")))
})

test_that(".gc_citation_payloads still reclaims the orphan when another row has a NA payload_id", {
  # cran_citations.payload_id is NULL on any crashed/timeout/error/malformed/
  # skipped row - a common shape, not a hypothetical one. SQL's
  # `x NOT IN (subquery)` evaluates to NULL, not TRUE, for every x once the
  # subquery's result set contains a NULL, which a WHERE clause then treats
  # as "don't delete" - so an unguarded query would turn the whole DELETE
  # into a no-op the moment one such row exists anywhere in the table, not
  # just fail to collect that one row's own (absent) payload.
  con <- cit_db()
  live   <- cit_pay("pid_live")
  orphan <- cit_pay("pid_orphan")
  upsert_shard(con, cit_sum("p", "1.0"), .empty_churn(), .empty_api(),
               citations_df = cit_one("p", "1.0", "pid_live"),
               payloads_df = live, entries_df = .empty_citation_entries_df())
  # Written directly rather than through upsert_shard(): a payload with no
  # citation row pointing at it at all, standing in for one orphaned by a
  # re-analysis in some earlier run.
  .upsert_ignore(con, "cran_citation_payloads", orphan, "payload_id")
  # A crashed citation row: it has no payload of its own, so payload_id is NA.
  DBI::dbAppendTable(con, "cran_citations",
                     cit_one("q", "1.0", NA_character_, status = "crashed"))

  .gc_citation_payloads(con)

  pays <- DBI::dbGetQuery(con, "SELECT payload_id FROM cran_citation_payloads")$payload_id
  expect_equal(pays, "pid_live")
})
