# tests/testthat/test-dataset-contract.R: the dataset column specs against the
# fields the analyzer actually emits.
#
# .write_datasets_normalized() routes a dataset record into the three tables by
# name: intersect(names(.DATASET_CONTENT_COLS), names(df)) and its two siblings.
# Nothing tied those lists to what the analyzer emits, so the two drifted in
# both directions at once, silently. A field the analyzer added and no spec
# declares is computed, carried across a fork, and then dropped on the way into
# SQLite. A column a spec declares and the analyzer never emits is shipped as
# public data that is NULL for every package in the archive.
#
# This test is the tie. It runs the real binary over the committed fixture
# package and compares the union of keys on its dataset records against the
# three specs, in both directions. The fixtures exist to make that union wide:
# every object under fixtures/dataset-contract/pkg/data is there to make one
# family of fields come back, and fixtures/dataset-contract/make.R regenerates
# them.

.contract_fixture_pkg <- function() {
  test_path("fixtures", "dataset-contract", "pkg")
}

# Keys the analyzer puts on a dataset record that are deliberately not in any
# column spec. Each is stored, just not by the spec-driven path: the record
# discriminator is not data, the identity and file fields are named one by one
# in .write_datasets_normalized's own INSERT lists, and the row sketch has a
# table of its own so it stays out of the merge allowlist.
.CONTRACT_STRUCTURAL_KEYS <- c(
  "rec",
  "name", "file", "internal",
  "format", "compression", "confidence",
  "content_fp", "schema_fp",
  "row_sketch"
)

# Declared columns the analyzer does not put on a dataset record, and why. A
# name here is a claim that the column is not dead: it is either filled from
# somewhere other than a top-level key, or it is the top-level home of a field
# the analyzer currently reports only per column, where the value survives
# inside the columns JSON and the column itself is the one that reads NULL.
#
# Adding a name here is the whole point of the friction: it says out loud that
# a public column is empty and someone decided that is acceptable for now.
.CONTRACT_NOT_EMITTED <- c(
  # Derived by .datasets_frame from the length of the columns array rather than
  # sent as a field, because the width of a profiled schema is a property of
  # that array.
  "n_cols",

  # Reported per column, inside the columns JSON, and never for the object as a
  # whole. The value is in the data; these columns are its empty top-level
  # home, which only a vector-shaped dataset could ever fill.
  "n_nan", "n_infinite_pos", "n_infinite_neg",
  "is_geometry", "n_empty",

  # Not emitted anywhere, by any fixture, at any level. These are the dead
  # columns: declared, shipped, and NULL for every package in the archive.
  # Removing a column is a separate exercise from noticing it, because the
  # pipelines only ever ALTER ADD and the published data has readers.
  "frequency",             # ts objects report ts_frequency instead
  "index_sorted", "index_has_duplicates",
  "n_fields", "year_min", "year_max"
)

# Run the analyzer over the fixture package once per session and return the
# union of keys seen on rec == "dataset" records, at the top level and nested
# inside the columns/elements arrays. Nested keys are reported separately
# because a field that is only ever per column is a different finding from one
# that is never computed at all.
.contract_keys <- local({
  cached <- NULL
  function(bin) {
    if (!is.null(cached)) return(cached)
    out <- suppressWarnings(system2(bin, shQuote(.contract_fixture_pkg()),
                                    stdout = TRUE, stderr = FALSE))
    top <- character(0L)
    nested <- character(0L)
    walk <- function(x) {
      if (!is.list(x)) return(invisible(NULL))
      if (!is.null(names(x))) nested <<- union(nested, names(x))
      for (el in x) walk(el)
      invisible(NULL)
    }
    n_records <- 0L
    for (line in out) {
      rec <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                      error = function(e) NULL)
      if (is.null(rec) || !identical(rec[["rec"]], "dataset")) next
      n_records <- n_records + 1L
      top <- union(top, names(rec))
      for (k in c("columns", "elements")) walk(rec[[k]])
    }
    cached <<- list(top = top, nested = nested, n_records = n_records)
    cached
  }
})

# The binary is optional in a local checkout and installed in CI, so an absent
# one skips rather than passes. Matches how test-binary.R handles the same
# situation, and reports the version it did find so a stream with no dataset
# records at all names the build that produced it.
.contract_setup <- function() {
  bin <- rpkg_analyzer_bin()
  skip_if(!nzchar(bin),
          "no rpkg-analyzer binary: set RPKG_ANALYZER_BIN or put one on PATH")
  keys <- .contract_keys(bin)
  skip_if(keys$n_records == 0L,
          sprintf(paste("rpkg-analyzer %s emitted no dataset records for the",
                        "fixture package, so there is no contract to check"),
                  rpkg_analyzer_version()))
  keys
}

.declared_dataset_cols <- function() {
  c(names(.DATASET_CONTENT_COLS), names(.DATASET_VERSION_COLS),
    names(.DATASET_IDENTITY_COLS))
}

test_that("the fixture package exercises every declared dataset family", {
  # A shrunken fixture set would make the direction below pass by never
  # reaching the fields it is meant to check, so the corpus itself is asserted.
  files <- list.files(file.path(.contract_fixture_pkg(), "data"))
  expect_gt(length(files), 30L)
  expect_true(file.exists(file.path(.contract_fixture_pkg(), "DESCRIPTION")))
  expect_true(file.exists(file.path(.contract_fixture_pkg(), "R", "sysdata.rda")))
})

test_that("every dataset field the analyzer emits is declared by a column spec", {
  keys <- .contract_setup()
  undeclared <- sort(setdiff(keys$top,
                             c(.declared_dataset_cols(), .CONTRACT_STRUCTURAL_KEYS)))
  expect_identical(
    undeclared, character(0L),
    info = paste0(
      "the analyzer emits these dataset fields and no column spec declares ",
      "them, so .write_datasets_normalized computes and then drops them: ",
      paste(undeclared, collapse = ", "),
      ". Add each to .DATASET_CONTENT_COLS, .DATASET_VERSION_COLS or ",
      ".DATASET_IDENTITY_COLS by what it describes, or to ",
      ".CONTRACT_STRUCTURAL_KEYS if it is stored some other way."))
})

test_that("every declared dataset column is one the analyzer emits", {
  keys <- .contract_setup()
  missing <- sort(setdiff(.declared_dataset_cols(),
                          c(keys$top, .CONTRACT_NOT_EMITTED)))
  expect_identical(
    missing, character(0L),
    info = paste0(
      "these columns are declared and shipped as public data, and the ",
      "analyzer never emits them, so they are NULL for every package: ",
      paste(missing, collapse = ", "),
      ". Either the fixture package no longer reaches the shape that fills ",
      "them, or the column is dead and belongs in .CONTRACT_NOT_EMITTED with ",
      "a reason."))
})

test_that("a column exempted as per-column-only really is emitted per column", {
  keys <- .contract_setup()
  # The exemptions split into three claims, and the two that assert a value
  # exists somewhere are checkable. n_cols is derived, so it is exempt from the
  # check as well as from the spec.
  per_column <- c("n_nan", "n_infinite_pos", "n_infinite_neg",
                  "is_geometry", "n_empty")
  expect_identical(sort(setdiff(per_column, keys$nested)), character(0L))

  # And the dead ones are genuinely dead: not at the top level, not per column,
  # not anywhere in the stream. If one of these starts arriving, the exemption
  # is stale and the column should come off the list.
  dead <- setdiff(.CONTRACT_NOT_EMITTED, c(per_column, "n_cols"))
  expect_identical(sort(intersect(dead, c(keys$top, keys$nested))), character(0L))
})
