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

test_that("the sandbox mode is docker when docker is usable, else none", {
  mode <- citation_sandbox_mode()
  expect_true(mode %in% c("docker", "none"))
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

test_that("the reader runs unsandboxed locally and says so", {
  skip_if(citation_sandbox_mode() == "docker", "docker is available")
  withr::local_envvar(CITATION_SANDBOX = "allow")
  root <- withr::local_tempdir()
  d <- file.path(root, "k1")
  dir.create(file.path(d, "inst"), recursive = TRUE)
  writeBin(charToRaw('bibentry("Misc", title = "T", author = person("A", "B"), year = "2001")'),
           file.path(d, "inst", "CITATION"))
  writeLines("Package: p\nVersion: 1.0\nTitle: T\n", file.path(d, "DESCRIPTION"))
  inp <- data.frame(package = "p", version = "1.0", is_current = 1L,
                    released = "2024-01-01", source_sha256 = strrep("a", 64L),
                    dir = d, stringsAsFactors = FALSE)

  got <- run_citation_reader(inp, file.path("..", "..", "scripts", "cite_reader.R"))
  expect_equal(got$outcome, "unsandboxed")
  expect_true(any(grepl('"t":"end"', got$lines)))
})

test_that("an unsandboxed run is refused when the sandbox is required", {
  skip_if(citation_sandbox_mode() == "docker", "docker is available")
  withr::local_envvar(CITATION_SANDBOX = "require")
  inp <- data.frame(package = "p", version = "1.0", is_current = 1L,
                    released = "2024-01-01", source_sha256 = strrep("a", 64L),
                    dir = tempdir(), stringsAsFactors = FALSE)
  got <- run_citation_reader(inp, file.path("..", "..", "scripts", "cite_reader.R"))
  expect_equal(got$outcome, "crashed")
  expect_true(grepl("required", got$message))
})
