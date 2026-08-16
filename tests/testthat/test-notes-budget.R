# tests/testthat/test-notes-budget.R
#
# The release body is what GitHub refuses, so the bound has to hold in bytes
# for a listing of any size, not in rows. A --recollect run or a catch-up after
# an outage marks tens of thousands of packages changed (the bootstrap marked
# 33,282), and the workflow publishes with `publish_metrics || exit 1`, so a
# body GitHub rejects stops the pipeline on the run that had the most to report.

.empty_churn_nb <- function() {
  data.frame(package = character(0L), version = character(0L),
             file = character(0L), added = integer(0L), deleted = integer(0L),
             stringsAsFactors = FALSE)
}

.empty_api_nb <- function() {
  data.frame(package = character(0L), version = character(0L),
             exports_added = character(0L), exports_removed = character(0L),
             n_exports = integer(0L), stringsAsFactors = FALSE)
}

# A code DB holding `n` packages whose names are `name_len` characters long,
# with metrics wide enough to exercise the widest cell each column can hold.
.big_code_db <- function(path, n, name_len = 12L, prefix = "pkg") {
  stem <- sprintf("%s%%0%dd", prefix, max(1L, name_len - nchar(prefix)))
  pkgs <- sprintf(stem, seq_len(n))
  summary_df <- data.frame(
    package             = pkgs,
    version             = rep("10.10.10-999", n),
    loc_r               = rep(999999L, n),
    n_exports           = rep(9999L, n),
    n_internal          = rep(9999L, n),
    n_deps_direct       = rep(999L, n),
    bump_type           = rep("minor", n),
    exports_added_n     = seq_len(n),
    exports_removed_n   = rep(0L, n),
    latest_release_date = rep("2026-08-15", n),
    stringsAsFactors = FALSE
  )
  export_metrics(path, summary_df, .empty_churn_nb(), .empty_api_nb())
  pkgs
}

.manifests_nb <- function(out) {
  write_manifest(file.path(out, "code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = DB_FILENAME,
    generated_at = "2026-08-15T00:00:00Z", db_bytes = 1256275968,
    fingerprint = strrep("a", 64), n_packages = 33282L, n_versions = 207465L,
    tables = list(cran_code_summary = 207465L, cran_functions = 2445143L),
    stats = list(loc_r_mean = 3975, loc_r_median = 1720, n_fns_r_median = 30),
    bootstrap = list(n_analyzed = 33282L, n_universe = 33307L,
      n_remaining = 0L, bootstrap_complete = TRUE)))
  write_manifest(file.path(out, "data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = DATA_DB_FILENAME,
    generated_at = "2026-08-15T00:00:00Z", db_bytes = 383332352,
    fingerprint = strrep("b", 64), n_packages = 11487L, n_versions = 464321L,
    tables = list(cran_datasets = 53149L, cran_dataset_contents = 65147L),
    stats = list(nrow_median = 180, ncol_median = 5),
    bootstrap = list(n_analyzed = 33282L, n_universe = 33307L,
      n_remaining = 0L, bootstrap_complete = TRUE)))
}

# Bytes as GitHub counts them: the file the workflow hands to `gh release
# create --notes-file`, newlines included.
.body_bytes <- function(lines) sum(nchar(lines, type = "bytes")) + length(lines)

test_that("a 10,000-package listing stays far inside the release body limit", {
  out <- withr::local_tempdir()
  pkgs <- .big_code_db(file.path(out, DB_FILENAME), 10000L)
  .manifests_nb(out)
  writeLines(pkgs, file.path(out, "changed-packages.txt"))
  writeLines(character(0L), file.path(out, "seed-packages.txt"))

  render_notes(out)
  md <- readLines(file.path(out, "release-notes-code.md"))

  expect_lt(.body_bytes(md), NOTES_BODY_GITHUB_LIMIT)
  expect_lte(.body_bytes(md), NOTES_BODY_MAX_BYTES)
})

test_that("the budget we allow ourselves stays well under the one GitHub enforces", {
  expect_lt(NOTES_BODY_MAX_BYTES, NOTES_BODY_GITHUB_LIMIT / 2)
})

test_that("the omitted count is stated and is the number of rows left out", {
  out <- withr::local_tempdir()
  pkgs <- .big_code_db(file.path(out, DB_FILENAME), 10000L)
  .manifests_nb(out)
  writeLines(pkgs, file.path(out, "changed-packages.txt"))
  writeLines(character(0L), file.path(out, "seed-packages.txt"))

  render_notes(out)
  md <- readLines(file.path(out, "release-notes-code.md"))

  shown <- sum(grepl("^\\| pkg[0-9]+ \\|", md))
  omitted <- grep("^\\| \\.\\.\\.and ", md, value = TRUE)
  expect_length(omitted, 1L)
  expect_true(grepl(sprintf("and %s more", format(10000L - shown, big.mark = ",")),
                    omitted, fixed = TRUE))
  expect_equal(shown, NOTES_TABLE_MAX_ROWS)
})

test_that("the byte budget, not the row count, is what bounds the body", {
  # Row cap lifted, names at 200 characters: no fixed number of rows bounds
  # this body, so the budget has to.
  out <- withr::local_tempdir()
  pkgs <- .big_code_db(file.path(out, DB_FILENAME), 5000L, name_len = 200L)
  .manifests_nb(out)
  code_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME),
                             flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(code_con), add = TRUE)

  notes <- build_release_notes(
    jsonlite::fromJSON(file.path(out, "code-manifest.json")),
    jsonlite::fromJSON(file.path(out, "data-manifest.json")),
    pkgs, character(0L), code_con, NULL, cap = 5000L)

  expect_lte(.body_bytes(notes), NOTES_BODY_MAX_BYTES)
  expect_gt(.body_bytes(notes), NOTES_BODY_MAX_BYTES - 2000L)  # the budget is used, not wasted
  expect_true(any(grepl("^\\| \\.\\.\\.and ", notes)))
})

test_that("a multi-byte package name is charged its bytes, not its characters", {
  out <- withr::local_tempdir()
  # Four bytes per character in UTF-8, so a character count would under-measure
  # this body by a factor of four.
  wide <- vapply(seq_len(2000L), function(i) paste0(strrep("\U0001F4E6", 60L), i),
                 character(1L))
  summary_df <- data.frame(
    package = wide, version = "1.0.0", loc_r = 1000L, n_exports = 10L,
    n_internal = 10L, n_deps_direct = 1L, bump_type = "minor",
    exports_added_n = seq_along(wide), exports_removed_n = 0L,
    latest_release_date = "2026-08-15", stringsAsFactors = FALSE)
  export_metrics(file.path(out, DB_FILENAME), summary_df, .empty_churn_nb(), .empty_api_nb())
  .manifests_nb(out)
  code_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME),
                             flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(code_con), add = TRUE)

  notes <- build_release_notes(
    jsonlite::fromJSON(file.path(out, "code-manifest.json")),
    jsonlite::fromJSON(file.path(out, "data-manifest.json")),
    wide, character(0L), code_con, NULL, cap = 2000L)

  expect_lte(.body_bytes(notes), NOTES_BODY_MAX_BYTES)
})

test_that("a body whose fixed sections overrun the budget is cut and says so", {
  out <- withr::local_tempdir()
  pkgs <- .big_code_db(file.path(out, DB_FILENAME), 50L)
  .manifests_nb(out)
  code_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME),
                             flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(code_con), add = TRUE)

  notes <- build_release_notes(
    jsonlite::fromJSON(file.path(out, "code-manifest.json")),
    jsonlite::fromJSON(file.path(out, "data-manifest.json")),
    pkgs, character(0L), code_con, NULL, budget = 400L)

  expect_lte(.body_bytes(notes), 400L)
  expect_true(any(grepl("truncated", notes)))
})

test_that("changed packages missing from the code database are declared", {
  out <- withr::local_tempdir()
  pkgs <- .big_code_db(file.path(out, DB_FILENAME), 5L)
  .manifests_nb(out)
  writeLines(c(pkgs, "ghostA", "ghostB"), file.path(out, "changed-packages.txt"))
  writeLines(character(0L), file.path(out, "seed-packages.txt"))

  render_notes(out)
  md <- readLines(file.path(out, "release-notes-code.md"))

  expect_true(any(grepl("^2 of the 7 changed packages have no row in the code database", md)))
})

test_that("a small release carries no omission line and no truncation marker", {
  out <- withr::local_tempdir()
  pkgs <- .big_code_db(file.path(out, DB_FILENAME), 3L)
  .manifests_nb(out)
  writeLines(pkgs, file.path(out, "changed-packages.txt"))
  writeLines(character(0L), file.path(out, "seed-packages.txt"))

  render_notes(out)
  md <- readLines(file.path(out, "release-notes-code.md"))

  expect_equal(sum(grepl("^\\| pkg[0-9]+ \\|", md)), 3L)
  expect_false(any(grepl("\\.\\.\\.and ", md)))
  expect_false(any(grepl("truncated", md)))
  expect_false(any(grepl("no row in the code database", md)))
})
