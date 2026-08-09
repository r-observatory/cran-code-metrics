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
