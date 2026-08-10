# Staging the inputs a citation reader needs, and nothing else.

test_that("a version that ships no citation stages nothing", {
  tmp <- withr::local_tempdir()
  stg <- withr::local_tempdir()
  writeLines("Package: p\nVersion: 1.0\n", file.path(tmp, "DESCRIPTION"))

  got <- stage_citation_inputs(tmp, stg, "p", "1.0", 1L, "2024-01-01")
  expect_equal(nrow(got), 0L)
  expect_equal(length(list.files(stg)), 0L)
})

test_that("staging writes the citation bytes verbatim, not a normalised read", {
  # CRLF and the trailing newline are part of the file R parses. ctx$read()
  # destroys both, so staging must not go through it.
  tmp <- withr::local_tempdir()
  stg <- withr::local_tempdir()
  dir.create(file.path(tmp, "inst"))
  raw_bytes <- charToRaw("citHeader(\"h\")\r\nbibentry(\"Misc\", title = \"t\")\n")
  writeBin(raw_bytes, file.path(tmp, "inst", "CITATION"))
  writeLines("Package: p\nVersion: 1.0\nEncoding: UTF-8\n", file.path(tmp, "DESCRIPTION"))

  got <- stage_citation_inputs(tmp, stg, "p", "1.0", 1L, "2024-01-01")
  expect_equal(nrow(got), 1L)
  expect_equal(got$package, "p")
  expect_equal(got$version, "1.0")
  expect_equal(got$is_current, 1L)
  expect_equal(got$released, "2024-01-01")

  staged <- file.path(got$dir, "inst", "CITATION")
  expect_true(file.exists(staged))
  expect_identical(readBin(staged, "raw", file.size(staged)), raw_bytes)

  # The DESCRIPTION travels too: readCitationFile hard-errors on a non-ASCII
  # file when meta$Encoding is absent, so meta is not optional.
  expect_true(file.exists(file.path(got$dir, "DESCRIPTION")))
})

test_that("the staged sha is of the bytes, and identical bytes reuse one directory", {
  tmp <- withr::local_tempdir()
  stg <- withr::local_tempdir()
  dir.create(file.path(tmp, "inst"))
  writeBin(charToRaw("bibentry(\"Misc\", title = \"t\")\n"),
           file.path(tmp, "inst", "CITATION"))
  writeLines("Package: p\nVersion: 1.0\n", file.path(tmp, "DESCRIPTION"))

  a <- stage_citation_inputs(tmp, stg, "p", "1.0", 0L, "2024-01-01")
  expect_equal(nchar(a$source_sha256), 64L)

  # A second version with the same citation bytes but a different DESCRIPTION
  # gets its own directory, because meta$Version reaches the file and changes
  # what it renders.
  writeLines("Package: p\nVersion: 2.0\n", file.path(tmp, "DESCRIPTION"))
  b <- stage_citation_inputs(tmp, stg, "p", "2.0", 1L, "2025-01-01")
  expect_equal(a$source_sha256, b$source_sha256)
  expect_false(identical(a$dir, b$dir))
})

test_that("a version with no DESCRIPTION still stages, with an empty meta", {
  # A malformed tarball is not a reason to lose the citation. The reader will
  # report the encoding failure honestly rather than the staging step guessing.
  tmp <- withr::local_tempdir()
  stg <- withr::local_tempdir()
  dir.create(file.path(tmp, "inst"))
  writeBin(charToRaw("bibentry(\"Misc\", title = \"t\")\n"),
           file.path(tmp, "inst", "CITATION"))

  got <- stage_citation_inputs(tmp, stg, "p", "1.0", 1L, NA_character_)
  expect_equal(nrow(got), 1L)
  expect_true(file.exists(file.path(got$dir, "DESCRIPTION")))
})

test_that("an empty input frame runs nothing at all", {
  got <- run_citation_reader(.empty_citation_inputs_df(),
                             file.path("..", "..", "scripts", "cite_reader.R"))
  expect_equal(got$outcome, "skipped")
  expect_equal(length(got$lines), 0L)
})

test_that("a missing terminator is reported as a crash, not as an empty result", {
  # A truncated stream means the process died partway. Treating it as "this
  # package ships no citation" would silently delete real data.
  got <- .citation_outcome(c('{"t":"doc","id":"a","status":"ok"}'), 0L)
  expect_equal(got, "crashed")
  ok <- .citation_outcome(c('{"t":"doc","id":"a","status":"ok"}', '{"t":"end"}'), 0L)
  expect_equal(ok, "ok")
  expect_equal(.citation_outcome(character(0L), 124L), "timeout")
})

test_that("bytes after the terminator are refused", {
  # A finalizer registered by an evaluated file runs after the terminator is
  # written. Anything past it is not ours.
  got <- .citation_outcome(c('{"t":"doc","id":"a","status":"ok"}',
                             '{"t":"end"}',
                             '{"t":"doc","id":"FORGED","status":"ok"}'), 0L)
  expect_equal(got, "crashed")
})

# --- The reader is a real subprocess, and it is started from an environment
# built by name rather than from this one.

stage_one <- function(root, citation_src) {
  d <- file.path(root, "k1")
  dir.create(file.path(d, "inst"), recursive = TRUE)
  writeBin(charToRaw(citation_src), file.path(d, "inst", "CITATION"))
  writeLines("Package: p\nVersion: 1.0\nTitle: T\n", file.path(d, "DESCRIPTION"))
  data.frame(package = "p", version = "1.0", is_current = 1L,
             released = "2024-01-01", source_sha256 = strrep("a", 64L),
             dir = d, stringsAsFactors = FALSE)
}

reader <- function() file.path("..", "..", "scripts", "cite_reader.R")

test_that("the reader evaluates a staged citation and terminates its stream", {
  root <- withr::local_tempdir()
  inp <- stage_one(root,
    'bibentry("Misc", title = "T", author = person("A", "B"), year = "2001")')

  got <- run_citation_reader(inp, reader())
  expect_equal(got$outcome, "ok")
  expect_true(any(grepl('"t":"end"', got$lines)))
})

test_that("the child environment carries what R needs and no credential", {
  # Allowlisted by name, so a variable nobody anticipated is absent by default
  # rather than present until someone remembers to unset it.
  withr::local_envvar(GITHUB_TOKEN = "ghp_notARealToken_0123456789",
                      GH_TOKEN = "ghp_notARealToken_0123456789")
  env <- .citation_child_env()
  names_only <- sub("=.*$", "", env)

  expect_true("R_HOME" %in% names_only)
  expect_true("PATH" %in% names_only)
  expect_false("GITHUB_TOKEN" %in% names_only)
  expect_false("GH_TOKEN" %in% names_only)
  expect_false(any(grepl("notARealToken", env, fixed = TRUE)))
})

test_that("a token in this process's environment never reaches the evaluated file", {
  # Not hypothetical, and not asserted only on the allowlist: a CITATION file
  # is R code and may call Sys.getenv(), and this pipeline runs with a
  # GITHUB_TOKEN that has push rights. Evaluated with the environment
  # inherited, this exact fixture puts the token in the title column, which is
  # published. What the file reads must be "".
  withr::local_envvar(GITHUB_TOKEN = "ghp_notARealToken_0123456789")
  root <- withr::local_tempdir()
  inp <- stage_one(root, paste0(
    'bibentry("Misc", title = Sys.getenv("GITHUB_TOKEN"), ',
    'author = person("A", "B"), year = "2001")'))

  run <- run_citation_reader(inp, reader())
  expect_equal(run$outcome, "ok")
  expect_false(any(grepl("notARealToken", run$lines, fixed = TRUE)))

  got <- parse_citation_records(run$lines, inp, run$outcome)
  expect_equal(got$citations$status, "ok")
  expect_equal(nrow(got$entries), 1L)
  # An empty title is dropped by bibentry() rather than stored as "", so the
  # column comes back NA. Either way it does not carry the token.
  expect_true(is.na(got$entries$title))
  no_token <- function(df) {
    chr <- df[vapply(df, is.character, logical(1))]
    !any(vapply(chr, function(col) any(grepl("notARealToken", col, fixed = TRUE)),
                logical(1)))
  }
  expect_true(no_token(got$citations))
  expect_true(no_token(got$payloads))
  expect_true(no_token(got$entries))
})

cit_inputs <- function(...) {
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r) {
    data.frame(package = r$package %||% "p", version = r$version,
               is_current = r$is_current %||% 0L, released = "2024-01-01",
               source_sha256 = r$sha %||% strrep("a", 64L),
               dir = file.path(tempdir(), r$id), stringsAsFactors = FALSE)
  }))
}

test_that("a record whose id was never requested is refused", {
  # The reader evaluates code an author wrote. A forged record naming another
  # package must not become that package's citation.
  inp <- cit_inputs(list(id = "k1", version = "1.0", is_current = 1L))
  lines <- c('{"t":"doc","id":"k1","status":"empty","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}',
             '{"t":"doc","id":"FORGED","status":"empty","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}',
             '{"t":"end"}')
  got <- parse_citation_records(lines, inp, "ok")
  expect_equal(nrow(got$citations), 1L)
  expect_equal(got$citations$package, "p")
  expect_false(anyNA(got$citations$status))
})

test_that("a version whose doc record never arrived is recorded as crashed", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"), list(id = "k2", version = "2.0"))
  lines <- c('{"t":"doc","id":"k1","status":"empty","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}',
             '{"t":"end"}')
  got <- parse_citation_records(lines, inp, "ok")
  expect_equal(nrow(got$citations), 2L)
  st <- setNames(got$citations$status, got$citations$version)
  expect_equal(unname(st[["1.0"]]), "empty")
  expect_equal(unname(st[["2.0"]]), "crashed")
  expect_false(anyNA(got$citations$status))
})

test_that("identical results across versions share one payload row", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"), list(id = "k2", version = "2.0"))
  doc <- function(id) sprintf('{"t":"doc","id":"%s","status":"ok","n_entries":1,"mheader":null,"mfooter":null,"header_scope":"none","message":null}', id)
  ent <- function(id) sprintf('{"t":"entry","id":"%s","i":1,"bibtype":"Misc","title":"T","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T"}', id)
  got <- parse_citation_records(c(doc("k1"), ent("k1"), doc("k2"), ent("k2"), '{"t":"end"}'), inp, "ok")
  expect_equal(nrow(got$citations), 2L)
  expect_equal(length(unique(got$citations$payload_id)), 1L)
  expect_equal(nrow(got$payloads), 1L)
  expect_equal(nrow(got$entries), 1L)
  expect_false(anyNA(got$citations$status))
})

test_that("a timeout marks every requested version, and writes no payload", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  got <- parse_citation_records(character(0L), inp, "timeout")
  expect_equal(got$citations$status, "timeout")
  expect_true(is.na(got$citations$payload_id))
  expect_equal(nrow(got$payloads), 0L)
  expect_false(anyNA(got$citations$status))
})

test_that("invalid UTF-8 in a field fails the record rather than storing it", {
  # json_encode in the viewer returns false on a malformed byte, which empties
  # the payload script tag and silently kills the format picker for that package.
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  bad <- rawToChar(as.raw(c(0x41, 0xff, 0x42)))
  lines <- c(sprintf('{"t":"doc","id":"k1","status":"ok","n_entries":1,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'),
             sprintf('{"t":"entry","id":"k1","i":1,"bibtype":"Misc","title":"%s","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T"}', bad),
             '{"t":"end"}')
  got <- parse_citation_records(lines, inp, "ok")
  expect_equal(got$citations$status, "malformed")
  expect_equal(nrow(got$entries), 0L)
  expect_false(anyNA(got$citations$status))
})

test_that("an escaped lone low surrogate in a field is rejected as malformed", {
  # Unlike the raw 0xff byte the fixture above uses (which fails to parse as
  # JSON at all and never reaches .cit_valid()), a lone low surrogate such as
  # \uDC00 is legal JSON grammar: it survives jsonlite::fromJSON() as an R
  # string that decodes to bytes ed b0 80, which validUTF8() rejects. This is
  # the case .cit_valid() exists to catch. Built via paste0() with a
  # standalone backslash rather than written as a literal "\\uDC00" in the
  # source, so R's own string parser never gets a chance to interpret it -
  # what reaches jsonlite is the six literal characters \, u, D, C, 0, 0.
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  bs <- "\\"
  bad_title <- paste0(bs, "u", "DC00")
  doc <- '{"t":"doc","id":"k1","status":"ok","n_entries":1,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  ent <- sprintf('{"t":"entry","id":"k1","i":1,"bibtype":"Misc","title":"%s","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T"}', bad_title)
  got <- parse_citation_records(c(doc, ent, '{"t":"end"}'), inp, "ok")
  expect_equal(got$citations$status, "malformed")
  expect_equal(nrow(got$entries), 0L)
  expect_false(anyNA(got$citations$status))
})

test_that("an unresolvable release date is recorded as such rather than dropped", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  inp$released <- NA_character_
  lines <- c('{"t":"doc","id":"k1","status":"error","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":"no known release date, so a citation year cannot be resolved"}',
             '{"t":"end"}')
  got <- parse_citation_records(lines, inp, "ok")
  expect_equal(got$citations$status, "error")
  expect_equal(got$citations$released_known, 0L)
  expect_false(anyNA(got$citations$status))
})

# --- Records that parse as JSON but arrive in a shape the honest reader
# never emits. None of these may abort the whole package's parse: one bad
# record must cost at most its own version, never the versions around it.

test_that("a null entry index does not abort the package's other versions", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"), list(id = "k2", version = "2.0"))
  doc <- function(id) sprintf('{"t":"doc","id":"%s","status":"ok","n_entries":1,"mheader":null,"mfooter":null,"header_scope":"none","message":null}', id)
  ent_ok <- '{"t":"entry","id":"k1","i":1,"bibtype":"Misc","title":"T","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T"}'
  ent_null_i <- '{"t":"entry","id":"k2","i":null,"bibtype":"Misc","title":"T","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T"}'
  got <- parse_citation_records(c(doc("k1"), ent_ok, doc("k2"), ent_null_i, '{"t":"end"}'), inp, "ok")
  expect_equal(nrow(got$citations), 2L)
  st <- setNames(got$citations$status, got$citations$version)
  expect_equal(unname(st[["1.0"]]), "ok")
  expect_equal(unname(st[["2.0"]]), "malformed")
  # only k1 contributed: nothing from k2's record survives into storage
  expect_equal(nrow(got$payloads), 1L)
  expect_equal(nrow(got$entries), 1L)
  expect_false(anyNA(got$citations$status))
})

test_that("an empty-array doc field does not abort the package's other versions", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"), list(id = "k2", version = "2.0"),
                    list(id = "k3", version = "3.0"))
  doc_ok <- '{"t":"doc","id":"k1","status":"empty","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  doc_bad_status <- '{"t":"doc","id":"k2","status":[],"n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  doc_bad_message <- '{"t":"doc","id":"k3","status":"error","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":[]}'
  got <- parse_citation_records(c(doc_ok, doc_bad_status, doc_bad_message, '{"t":"end"}'), inp, "ok")
  expect_equal(nrow(got$citations), 3L)
  st  <- setNames(got$citations$status, got$citations$version)
  msg <- setNames(got$citations$message, got$citations$version)
  expect_equal(unname(st[["1.0"]]), "empty")
  # status:[] is validated against the known set, not passed through: it
  # normalises to "malformed", never to NA (citations.status is NOT NULL).
  expect_equal(unname(st[["2.0"]]), "malformed")
  expect_equal(unname(st[["3.0"]]), "error")
  expect_true(is.na(unname(msg[["3.0"]])))
  # only k1's "empty" doc builds a (zero-entry) payload row; k2 is
  # malformed and k3 is "error", so neither builds one.
  expect_equal(nrow(got$payloads), 1L)
  expect_false(anyNA(got$citations$status))
})

test_that("an empty-array entry field does not abort the package's other versions", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  doc <- '{"t":"doc","id":"k1","status":"ok","n_entries":1,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  ent_bad_title <- '{"t":"entry","id":"k1","i":1,"bibtype":"Misc","title":[],"year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T"}'
  got <- parse_citation_records(c(doc, ent_bad_title, '{"t":"end"}'), inp, "ok")
  expect_equal(got$citations$status, "ok")
  expect_equal(nrow(got$entries), 1L)
  expect_true(is.na(got$entries$title))
  expect_false(anyNA(got$citations$status))
})

test_that("a non-scalar record id is refused rather than aborting the parse", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"), list(id = "k2", version = "2.0"))
  doc_ok  <- '{"t":"doc","id":"k1","status":"empty","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  doc_bad <- '{"t":"doc","id":["k1","k2"],"status":"ok","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  got <- parse_citation_records(c(doc_ok, doc_bad, '{"t":"end"}'), inp, "ok")
  expect_equal(nrow(got$citations), 2L)
  st <- setNames(got$citations$status, got$citations$version)
  expect_equal(unname(st[["1.0"]]), "empty")
  # the array-id record names no version in the manifest, so k2 gets no doc
  # record at all rather than the forged one being attributed to it.
  expect_equal(unname(st[["2.0"]]), "crashed")
  expect_false(anyNA(got$citations$status))
})

# --- n_entries must match which entries parsed, not just how many.

test_that("a duplicated entry index is malformed even though the count matches", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  doc <- '{"t":"doc","id":"k1","status":"ok","n_entries":2,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  ent1 <- '{"t":"entry","id":"k1","i":1,"bibtype":"Misc","title":"T1","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T1"}'
  # Entry 2 is corrupt and never arrives; a duplicate of entry 1 survives in
  # its place, so the count still comes to 2.
  dup <- '{"t":"entry","id":"k1","i":1,"bibtype":"Misc","title":"T1","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T1"}'
  got <- parse_citation_records(c(doc, ent1, dup, '{"t":"end"}'), inp, "ok")
  expect_equal(got$citations$status, "malformed")
  expect_equal(nrow(got$entries), 0L)
  expect_false(anyNA(got$citations$status))
})

test_that("a missing n_entries on an ok doc is malformed, not assumed absent", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"), list(id = "k2", version = "2.0"))
  doc_missing <- '{"t":"doc","id":"k1","status":"ok","mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  ent1 <- '{"t":"entry","id":"k1","i":1,"bibtype":"Misc","title":"T1","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T1"}'
  doc_null <- '{"t":"doc","id":"k2","status":"ok","n_entries":null,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  ent2 <- '{"t":"entry","id":"k2","i":1,"bibtype":"Misc","title":"T1","year":"2001","authors":[],"fields":{},"text_version":[],"header":null,"footer":null,"key":null,"bibtex":"@Misc{,}","citation":"T1"}'
  got <- parse_citation_records(c(doc_missing, ent1, doc_null, ent2, '{"t":"end"}'), inp, "ok")
  expect_equal(got$citations$status, c("malformed", "malformed"))
  expect_equal(nrow(got$entries), 0L)
  expect_false(anyNA(got$citations$status))
})

# --- citations.status is NOT NULL and this frame is written inside the same
# transaction as the rest of the shard: a doc's own status is validated
# against the fixed set the pipeline defines, never passed through raw.

test_that("a doc status arriving as an empty array is malformed, and the row is still written", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  doc <- '{"t":"doc","id":"k1","status":[],"n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  got <- parse_citation_records(c(doc, '{"t":"end"}'), inp, "ok")
  expect_equal(nrow(got$citations), 1L)
  expect_equal(got$citations$status, "malformed")
  expect_false(anyNA(got$citations$status))
})

test_that("an invented doc status is malformed, and the invented string is stored nowhere", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  doc <- '{"t":"doc","id":"k1","status":"pwned","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  got <- parse_citation_records(c(doc, '{"t":"end"}'), inp, "ok")
  expect_equal(got$citations$status, "malformed")
  no_pwned <- function(df) {
    chr_cols <- df[vapply(df, is.character, logical(1))]
    !any(vapply(chr_cols, function(col) any(grepl("pwned", col, fixed = TRUE)), logical(1)))
  }
  expect_true(no_pwned(got$citations))
  expect_true(no_pwned(got$payloads))
  expect_true(no_pwned(got$entries))
})

test_that("a doc record with no status key at all is malformed", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  doc <- '{"t":"doc","id":"k1","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  got <- parse_citation_records(c(doc, '{"t":"end"}'), inp, "ok")
  expect_equal(got$citations$status, "malformed")
  expect_false(anyNA(got$citations$status))
})

test_that("a whole-run outcome becomes the status of every version it covers", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  for (oc in c("timeout", "crashed", "skipped")) {
    got <- parse_citation_records(character(0L), inp, oc)
    expect_equal(got$citations$status, oc, info = oc)
    expect_false(anyNA(got$citations$status), info = oc)
  }
})

test_that(".empty_citations_df() has exactly the nine columns in order, matching a populated frame", {
  want_names <- c("package", "version", "is_current", "payload_id",
                  "source_sha256", "status", "released_known",
                  "message", "evaluated_at")
  empty <- .empty_citations_df()
  expect_equal(names(empty), want_names)
  expect_equal(nrow(empty), 0L)

  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  doc <- '{"t":"doc","id":"k1","status":"empty","n_entries":0,"mheader":null,"mfooter":null,"header_scope":"none","message":null}'
  populated <- parse_citation_records(c(doc, '{"t":"end"}'), inp, "ok")$citations
  expect_equal(names(populated), want_names)
  expect_equal(names(empty), names(populated))
})

test_that("a package with no citation-bearing version returns empty frames", {
  # The reader must not be launched at all. Starting a process per package
  # regardless of whether it has anything to read would multiply the cost of the
  # three quarters of CRAN that ships no citation file. Asserted directly by
  # counting calls, not just by inspecting the (necessarily empty) result: an
  # empty result is also what a reader that started and returned nothing would
  # produce, so checking only the frames would not catch a regression that
  # launches the reader anyway.
  calls <- 0L
  old <- run_citation_reader
  assign("run_citation_reader", function(...) { calls <<- calls + 1L; old(...) },
         envir = environment(citation_pass))
  on.exit(assign("run_citation_reader", old, envir = environment(citation_pass)), add = TRUE)

  got <- citation_pass(.empty_citation_inputs_df(), "scripts/cite_reader.R")
  expect_equal(nrow(got$citations), 0L)
  expect_equal(nrow(got$payloads), 0L)
  expect_equal(nrow(got$entries), 0L)
  expect_equal(calls, 0L)
})

test_that("a citation pass survives a reader that cannot start", {
  # A failure here must not propagate: analyze_package is wrapped in tryCatch by
  # the worker, and an error there marks the package failed, which after five
  # occurrences drops it from every metric the pipeline produces.
  root <- withr::local_tempdir()
  d <- file.path(root, "k1")
  dir.create(file.path(d, "inst"), recursive = TRUE)
  writeBin(charToRaw('bibentry("Misc", title = "T", author = person("A", "B"), year = "2001")'),
           file.path(d, "inst", "CITATION"))
  writeLines("Package: p\nVersion: 1.0\nTitle: T\n", file.path(d, "DESCRIPTION"))
  inp <- data.frame(package = "p", version = "1.0", is_current = 1L,
                    released = "2024-01-01", source_sha256 = strrep("a", 64L),
                    dir = d, stringsAsFactors = FALSE)

  # expect_warning() rather than letting it pass silently: keeps the suite's
  # warning count meaningful, and pins down that this specific path (a reader
  # that cannot be found at all) is what produced the crash.
  got <- expect_warning(citation_pass(inp, "no/such/reader.R"), "No such file")
  expect_equal(nrow(got$citations), 1L)
  # Indexed to a scalar rather than relying on %in% over the whole column:
  # correct today at one row, but %in% over a longer vector would still pass
  # if only some rows matched, silently the wrong shape should this fixture
  # ever grow to more than one version.
  expect_true(got$citations$status[[1L]] %in% c("crashed", "timeout"))
})

test_that("citation_reader_path() resolves to a file that actually exists", {
  # A guard against IMPORTANT 1 regressing silently: an unresolvable reader
  # path degrades to every version reading as "crashed" with no error
  # anywhere, which is indistinguishable from the reader itself failing.
  expect_true(file.exists(citation_reader_path()))
})

test_that("a whole-run failure's message reaches every row it produces", {
  # run_citation_reader()'s own message (e.g. "env(1) is required to run the
  # citation reader without this process's environment") explains every row
  # alike; discarding it makes a single misconfigured run indistinguishable
  # from many independent crashes.
  inp <- cit_inputs(list(id = "k1", version = "1.0"), list(id = "k2", version = "2.0"))
  got <- parse_citation_records(character(0L), inp, "crashed", "env(1) is required")
  expect_equal(got$citations$message, rep("env(1) is required", 2L))
})

test_that("a whole-run message is not attached to a per-version doc that simply never arrived", {
  # An "ok" run missing one version's doc record is a different fact from a
  # run that never started, and must not be reported with the same message.
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  got <- parse_citation_records(character(0L), inp, "ok", "should not appear")
  expect_equal(got$citations$status, "crashed")
  expect_true(is.na(got$citations$message))
})

test_that("an empty run message normalises to NA, not to an empty string", {
  inp <- cit_inputs(list(id = "k1", version = "1.0"))
  got <- parse_citation_records(character(0L), inp, "timeout", "")
  expect_true(is.na(got$citations$message))
})
