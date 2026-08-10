# The reader the pipeline launches per package. Invoked as a subprocess so the
# test exercises the real entry point, not an internal function.

cite_reader_run <- function(jobs) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  man  <- file.path(root, "manifest.tsv")
  out  <- file.path(root, "out.ndjson")
  lines <- vapply(names(jobs), function(id) {
    d <- file.path(root, id)
    dir.create(file.path(d, "inst"), recursive = TRUE)
    writeBin(charToRaw(jobs[[id]]$citation), file.path(d, "inst", "CITATION"))
    writeLines(jobs[[id]]$desc, file.path(d, "DESCRIPTION"))
    paste(id, d, jobs[[id]]$released %||% "2020-01-01", sep = "\t")
  }, character(1))
  writeLines(lines, man)
  rc <- system2("Rscript", c("--vanilla", shQuote(file.path("..", "..", "scripts", "cite_reader.R")),
                             shQuote(man), shQuote(out)),
                stdout = FALSE, stderr = FALSE)
  list(rc = rc, lines = if (file.exists(out)) readLines(out, warn = FALSE) else character(0L))
}

recs <- function(res, type) {
  keep <- grep(paste0('"t":"', type, '"'), res$lines, fixed = FALSE, value = TRUE)
  lapply(keep, function(l) jsonlite::fromJSON(l, simplifyVector = FALSE))
}

DESC <- "Package: p\nVersion: 1.0\nTitle: A Package\nEncoding: UTF-8\nAuthors@R: person(\"A\", \"B\", role = c(\"aut\", \"cre\"))\n"

test_that("a single entry is read with its fields and its bibtex", {
  res <- cite_reader_run(list(j1 = list(
    citation = 'bibentry("Article", title = "A Study", author = person("Ada", "Lovelace"), journal = "J Stat", year = "2001", doi = "10.1/x")',
    desc = DESC)))
  d <- recs(res, "doc")[[1L]]
  expect_equal(d$status, "ok")
  expect_equal(d$n_entries, 1L)
  expect_equal(d$header_scope, "none")

  e <- recs(res, "entry")[[1L]]
  expect_equal(e$bibtype, "Article")
  expect_equal(e$title, "A Study")
  expect_equal(e$year, "2001")
  expect_equal(e$fields$doi, "10.1/x")
  expect_equal(e$authors[[1L]]$family, "Lovelace")
  expect_true(grepl("^@Article", e$bibtex))
})

test_that("a citHeader-only file yields no entries and keeps its header", {
  # 14 of 724 sampled CRAN citation files are this shape. The header has to
  # survive on the document record, because there is no entry row to carry it.
  res <- cite_reader_run(list(j1 = list(
    citation = 'citHeader("Only a header here")', desc = DESC)))
  d <- recs(res, "doc")[[1L]]
  expect_equal(d$status, "empty")
  expect_equal(d$n_entries, 0L)
  expect_equal(d$mheader, "Only a header here")
  expect_equal(d$header_scope, "document")
  expect_equal(length(recs(res, "entry")), 0L)
})

test_that("citHeader grouping is recorded as unrecoverable, not guessed at", {
  # R flattens every citHeader in a file into one object-level string before a
  # consumer sees it. Entries carry no header at all, so no association exists
  # to recover and none may be invented.
  res <- cite_reader_run(list(j1 = list(
    citation = paste(
      'citHeader("HEADER A")',
      'bibentry("Article", title = "T1", author = person("A", "B"), journal = "J", year = "2001")',
      'citHeader("HEADER B")',
      'bibentry("Article", title = "T2", author = person("C", "D"), journal = "J", year = "2002")',
      sep = "\n"),
    desc = DESC)))
  d <- recs(res, "doc")[[1L]]
  expect_equal(d$n_entries, 2L)
  expect_equal(d$mheader, "HEADER A\nHEADER B")
  expect_equal(d$header_scope, "document")
  for (e in recs(res, "entry")) expect_null(e$header)
})

test_that("per-entry headers are kept on their own entries", {
  res <- cite_reader_run(list(j1 = list(
    citation = paste(
      'bibentry("Article", title = "T1", author = person("A", "B"), journal = "J", year = "2001", header = "To cite p use:")',
      'bibentry("Article", title = "T2", author = person("C", "D"), journal = "J", year = "2002", header = "If you use the fast path, also cite:")',
      sep = "\n"),
    desc = DESC)))
  d <- recs(res, "doc")[[1L]]
  expect_equal(d$header_scope, "per-entry")
  e <- recs(res, "entry")
  expect_equal(e[[1L]]$header, "To cite p use:")
  expect_equal(e[[2L]]$header, "If you use the fast path, also cite:")
})

test_that("fields are read per entry, not through $, which misaligns", {
  # x$doi on a two-entry bibentry where only one has a doi returns length 1.
  # Reading positionally is the only correct way.
  res <- cite_reader_run(list(j1 = list(
    citation = paste(
      'bibentry("Article", title = "No DOI", author = person("A", "B"), journal = "J", year = "2001")',
      'bibentry("Article", title = "Has DOI", author = person("C", "D"), journal = "J", year = "2002", doi = "10.1/y")',
      sep = "\n"),
    desc = DESC)))
  e <- recs(res, "entry")
  expect_equal(length(e), 2L)
  expect_null(e[[1L]]$fields$doi)
  expect_equal(e[[2L]]$fields$doi, "10.1/y")
})

test_that("Sys.Date resolves to the release date, not to today", {
  # A 2011 release must not acquire a 2026 citation year. This also makes a
  # rebuild reproduce the same rows.
  res <- cite_reader_run(list(j1 = list(
    citation = 'bibentry("Manual", title = "T", author = person("A", "B"), year = format(Sys.Date(), "%Y"))',
    desc = DESC, released = "2011-06-05")))
  expect_equal(recs(res, "entry")[[1L]]$year, "2011")
})

test_that("an unknown release date is not fabricated as 1970", {
  res <- cite_reader_run(list(j1 = list(
    citation = 'bibentry("Manual", title = "T", author = person("A", "B"), year = format(Sys.Date(), "%Y"))',
    desc = DESC, released = "NA")))
  d <- recs(res, "doc")[[1L]]
  expect_equal(d$status, "error")
  expect_true(grepl("release date", d$message))
})

test_that("an unknown release date does not block a file that never asks for it", {
  # Most CRAN citation files never call the clock at all, so an unresolved
  # release date must not fail them just because it happens to be unresolved.
  res <- cite_reader_run(list(j1 = list(
    citation = 'bibentry("Manual", title = "T", author = person("A", "B"), year = "2001")',
    desc = DESC, released = "NA")))
  d <- recs(res, "doc")[[1L]]
  expect_equal(d$status, "ok")
})

test_that("textVersion survives as an array, because it can legitimately be one", {
  res <- cite_reader_run(list(j1 = list(
    citation = 'bibentry("Misc", title = "T", author = person("A", "B"), year = "2001", textVersion = c("line one", "line two"))',
    desc = DESC)))
  expect_equal(recs(res, "entry")[[1L]]$text_version, list("line one", "line two"))
})

test_that("a non-ASCII file without a declared Encoding reports that, and does not crash the run", {
  res <- cite_reader_run(list(
    bad = list(citation = 'bibentry("Misc", title = "Café", author = person("A", "B"), year = "2001")',
               desc = "Package: p\nVersion: 1.0\nTitle: T\n"),
    good = list(citation = 'bibentry("Misc", title = "Fine", author = person("A", "B"), year = "2001")',
                desc = DESC)))
  d <- recs(res, "doc")
  by_id <- setNames(d, vapply(d, function(x) x$id, character(1)))
  expect_equal(by_id$bad$status, "error")
  expect_true(grepl("non-ASCII", by_id$bad$message))
  # The next job still runs. One bad file must not blank the rest.
  expect_equal(by_id$good$status, "ok")
})

test_that("a file that errors does not stop the jobs after it", {
  res <- cite_reader_run(list(
    boom = list(citation = 'stop("no")', desc = DESC),
    after = list(citation = 'bibentry("Misc", title = "Later", author = person("A", "B"), year = "2001")',
                 desc = DESC)))
  d <- recs(res, "doc")
  by_id <- setNames(d, vapply(d, function(x) x$id, character(1)))
  expect_equal(by_id$boom$status, "error")
  expect_equal(by_id$after$status, "ok")
  expect_true(any(grepl('"t":"end"', res$lines)))
})

test_that("output written by the evaluated file cannot enter the record stream", {
  # cat() and print() go to stdout; records go to the mounted out file. A file
  # that prints a forged record must not be able to reach it.
  res <- cite_reader_run(list(j1 = list(
    citation = paste(
      'cat("{\\"t\\":\\"doc\\",\\"id\\":\\"FORGED\\",\\"status\\":\\"ok\\"}\\n")',
      'bibentry("Misc", title = "T", author = person("A", "B"), year = "2001")',
      sep = "\n"),
    desc = DESC)))
  expect_false(any(grepl("FORGED", res$lines)))
  # A positive assertion is required alongside the negative one above: an
  # absent FORGED string is also what an empty res$lines looks like, which is
  # what a reader that never launched at all would produce. Asserting the
  # real entry was read is what rules that out.
  expect_equal(recs(res, "entry")[[1L]]$title, "T")
})

test_that("an evaluated citation file cannot rebind the reader's own JSON writer", {
  # The evaluation environment for a CITATION file chains up to globalenv(),
  # and `<<-` searches exactly that chain outward. If the JSON-writing
  # helpers lived in globalenv(), a file that redefines .jstr this way would
  # corrupt not just its own records but every later job's records in the
  # same batch, because the poisoned binding would stay poisoned for the
  # rest of the run.
  res <- cite_reader_run(list(
    poison = list(
      citation = paste(
        '.jstr <<- function(x) "\\"PWNED\\""',
        'bibentry("Misc", title = "Poison Title", author = person("A", "B"), year = "2001")',
        sep = "\n"),
      desc = DESC),
    after = list(
      citation = 'bibentry("Misc", title = "Untouched Title", author = person("C", "D"), year = "2002")',
      desc = DESC)))

  expect_false(any(grepl("PWNED", res$lines)))

  d <- recs(res, "doc")
  by_id <- setNames(d, vapply(d, function(x) x$id, character(1)))
  expect_equal(by_id$poison$status, "ok")
  expect_equal(by_id$after$status, "ok")

  e <- recs(res, "entry")
  titles <- vapply(e, function(x) x$title, character(1))
  expect_true("Poison Title" %in% titles)
  # The one that matters: the second job's own records are untouched by the
  # first job's attempt to rebind the writer.
  expect_true("Untouched Title" %in% titles)
})

test_that("the legacy citEntry form is read like any other entry", {
  res <- cite_reader_run(list(j1 = list(
    citation = 'citEntry(entry = "Book", title = "Old Style", author = personList(as.person("W. N. Venables")), publisher = "Springer", year = "1999", textVersion = "Venables (1999)")',
    desc = DESC)))
  e <- recs(res, "entry")[[1L]]
  expect_equal(e$bibtype, "Book")
  expect_equal(e$title, "Old Style")
})
