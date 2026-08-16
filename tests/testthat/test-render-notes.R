# tests/testthat/test-render-notes.R
#
# Fixture for the rich, per-package release notes format: a code DB with
# two packages (data.table already in the seed set = updated; ggplot2 not
# in the seed set = new) and a dataset DB where only data.table has rows.

.empty_churn_rn <- function() {
  data.frame(package = character(0L), version = character(0L),
             file = character(0L), added = integer(0L), deleted = integer(0L),
             stringsAsFactors = FALSE)
}

.empty_api_rn <- function() {
  data.frame(package = character(0L), version = character(0L),
             exports_added = character(0L), exports_removed = character(0L),
             n_exports = integer(0L), stringsAsFactors = FALSE)
}

# Writes code-manifest.json, data-manifest.json, changed-packages.txt,
# seed-packages.txt, and the two databases into `out` (a tempdir the caller
# owns/cleans up -- withr::local_tempdir() must be called in the test_that
# block itself, not in here, or the dir is removed as soon as this helper
# returns). `changed` lets the zero-changes case override the default
# two-package changed set. `prev` writes the previous release's manifests,
# which the workflow downloads before the shard loop; `run_status` writes the
# shard receipt update.R leaves in out/.
.setup_notes_fixture <- function(out, changed = c("data.table", "ggplot2"),
                                 prev = TRUE, run_status = NULL) {
  summary_df <- data.frame(
    package             = c("data.table", "ggplot2"),
    version              = c("1.15.0", "4.0.0"),
    loc_r                = c(18240L, 42110L),
    n_exports            = c(128L, 512L),
    n_internal           = c(214L, 378L),
    n_deps_direct        = c(2L, 11L),
    bump_type            = c("patch", "initial"),
    exports_added_n      = c(1L, 512L),
    exports_removed_n    = c(2L, 0L),
    latest_release_date  = c("2026-06-01", "2026-06-15"),
    stringsAsFactors = FALSE
  )
  export_metrics(file.path(out, DB_FILENAME), summary_df, .empty_churn_rn(), .empty_api_rn())

  data_con <- open_or_init_data_db(file.path(out, DATA_DB_FILENAME))
  DBI::dbExecute(data_con,
    "INSERT INTO cran_datasets (package, name) VALUES (?, ?)",
    params = list(rep("data.table", 3L), c("ds1", "ds2", "ds3")))
  DBI::dbDisconnect(data_con)

  write_manifest(file.path(out, "code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = DB_FILENAME,
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 100000L,
    fingerprint = paste0("abcdef12", strrep("0", 56)),
    n_packages = 1500L, n_versions = 5000L,
    tables = list(cran_code_summary = 5000L, cran_functions = 45000L),
    stats = list(loc_r_mean = 900, loc_r_median = 850, n_fns_r_median = 12),
    bootstrap = list(n_analyzed = 1000L, n_universe = 2000L,
      n_remaining = 1000L, bootstrap_complete = FALSE)))

  write_manifest(file.path(out, "data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = DATA_DB_FILENAME,
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 20000L,
    fingerprint = strrep("b", 64),
    n_packages = 3000L, n_versions = 12000L,
    tables = list(cran_datasets = 9000L, cran_dataset_contents = 7000L),
    stats = list(nrow_median = 180, ncol_median = 5),
    bootstrap = list(n_analyzed = 1000L, n_universe = 2000L,
      n_remaining = 1000L, bootstrap_complete = FALSE)))

  if (isTRUE(prev)) {
    write_manifest(file.path(out, "prev-code-manifest.json"), list(
      schema_version = 1L, series = "code", repo = "r", db_filename = DB_FILENAME,
      generated_at = "2026-07-09T00:00:00Z", db_bytes = 90000L,
      fingerprint = paste0("99887766", strrep("0", 56)),
      n_packages = 1480L, n_versions = 4950L,
      tables = list(cran_code_summary = 4950L, cran_functions = 44000L),
      stats = list(loc_r_median = 840),
      bootstrap = list(n_analyzed = 900L, n_universe = 2000L,
        n_remaining = 1100L, bootstrap_complete = FALSE)))
    write_manifest(file.path(out, "prev-data-manifest.json"), list(
      schema_version = 1L, series = "data", repo = "r", db_filename = DATA_DB_FILENAME,
      generated_at = "2026-07-09T00:00:00Z", db_bytes = 19000L,
      fingerprint = strrep("c", 64),
      n_packages = 2995L, n_versions = 11950L,
      tables = list(cran_datasets = 8990L, cran_dataset_contents = 6900L),
      stats = list(nrow_median = 175, ncol_median = 5),
      bootstrap = list(n_analyzed = 900L, n_universe = 2000L,
        n_remaining = 1100L, bootstrap_complete = FALSE)))
  }

  if (!is.null(run_status)) {
    write_manifest(file.path(out, "run-status.json"), run_status)
  }

  writeLines(changed, file.path(out, "changed-packages.txt"))
  writeLines("data.table", file.path(out, "seed-packages.txt"))

  invisible(NULL)
}

test_that("render_notes renders the headline, per-package table, catalog and footer", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out)
  render_notes(out, prev_code_tag = "metrics-2026-07-09", prev_data_tag = NULL,
               title_prefix = "CRAN")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  data_md <- readLines(file.path(out, "release-notes-data.md"))

  # Both notes files carry the same rich body.
  expect_identical(code_md, data_md)

  # No redundant top-level H1 (the GitHub release title already has it).
  expect_false(any(grepl("^# ", code_md)))

  # Headline: 1 new (ggplot2, not in seed-packages.txt), 1 updated (data.table).
  expect_true(any(grepl(
    "^1 package new to the catalog, 1 updated\\. Now tracking 1,500 packages across 5,000 versions\\.",
    code_md)))
  expect_true(any(grepl("Bootstrap 50% processed \\(1,000 remaining\\)\\.", code_md)))

  # Table: data.table is tagged with its bump type and its API change; ggplot2,
  # absent from the seed set, is tagged new. data.table removed an export, so
  # it sorts above ggplot2 whatever the alphabet says.
  i_dt <- grep("^\\| data\\.table \\| 1\\.15\\.0 \\(patch\\) \\| 18,240 \\| 342 \\| 128 \\| \\+1/-2 \\| 2 \\| 3 \\|$", code_md)
  i_gg <- grep("^\\| ggplot2 \\| 4\\.0\\.0 \\(new\\) \\| 42,110 \\| 890 \\| 512 \\| \\+512 \\| 11 \\| 0 \\|$", code_md)
  expect_length(i_dt, 1L)
  expect_length(i_gg, 1L)
  expect_lt(i_dt, i_gg)

  # Catalog section pulls straight from the manifests already read.
  expect_true(any(grepl("^## Catalog at a glance$", code_md)))
  expect_true(any(grepl("1,500 packages", code_md)))
  expect_true(any(grepl("median 850 LOC", code_md)))
  # Distinct datasets (cran_datasets = 9,000), NOT dataset versions (n_versions = 12,000).
  expect_true(any(grepl("9,000", code_md)))
  expect_false(any(grepl("12,000 datasets", code_md)))
  # Both DB sizes are named and flagged as separate releases (98 KB / 20 KB here).
  expect_true(any(grepl(
    "Databases: code metrics 98 KB and dataset metrics 20 KB \\(published as separate code and data releases\\)",
    code_md)))

  # Footer: short fingerprints only, no db bytes/full fingerprint/timestamp.
  expect_true(any(grepl("^<sub>fingerprint abcdef12 \\(was 99887766\\) - full manifest in the release assets</sub>$", code_md)))
  expect_false(any(grepl("100000", code_md)))
  expect_false(any(grepl(strrep("0", 56), code_md, fixed = TRUE)))
  expect_false(any(grepl("2026-07-10T00:00:00Z", code_md)))
})

test_that("the catalog says how far it moved, not only where it landed", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out)
  render_notes(out, prev_code_tag = "metrics-2026-07-09")

  md <- readLines(file.path(out, "release-notes-code.md"))

  # Code series: +20 packages, +50 versions, +1,000 functions.
  expect_true(any(grepl(
    "^- 1,500 packages \\(\\+20\\), 5,000 versions \\(\\+50\\), 45,000 functions \\(\\+1,000\\)$", md)))
  # Data series, which used to get a single line, now reports its own movement
  # and the shape of a typical dataset.
  expect_true(any(grepl(
    "^- Datasets: 9,000 \\(\\+10\\) across 3,000 packages \\(\\+5\\), 12,000 dataset versions \\(\\+50\\)$", md)))
  expect_true(any(grepl(
    "^- Typical dataset: 180 rows by 5 columns \\(median over 7,000 measured\\)$", md)))
  # R code line carries the second statistic the manifest already computes.
  expect_true(any(grepl(
    "^- R code: median 850 LOC and 12 functions per package, mean 900 LOC$", md)))
  # And the baseline is named once, so every parenthesis above is readable.
  expect_true(any(grepl(
    "change since metrics-2026-07-09, the release this run started from", md)))
})

test_that("without a previous manifest the notes carry no deltas at all", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out, prev = FALSE)
  render_notes(out)

  md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl("^- 1,500 packages, 5,000 versions, 45,000 functions$", md)))
  expect_false(any(grepl("release this run started from", md)))
  expect_true(any(grepl("^<sub>fingerprint abcdef12 - full manifest in the release assets</sub>$", md)))
})

test_that("packages that failed to analyze are counted in the summary", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out, run_status = list(
    changed = TRUE, bootstrap_complete = FALSE, n_analyzed = 1000L,
    n_universe = 2000L, n_remaining = 1000L, n_fresh = 2L, n_shard = 400L,
    n_versions = 2L, shard_failures = 3L))
  render_notes(out)

  md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl(
    "^3 of the 400 packages in the most recent shard failed to analyze", md)))
})

test_that("a shard with no failures says nothing about failures", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out, run_status = list(
    changed = TRUE, bootstrap_complete = FALSE, n_analyzed = 1000L,
    n_universe = 2000L, n_remaining = 1000L, n_fresh = 2L, n_shard = 400L,
    n_versions = 2L, shard_failures = 0L))
  render_notes(out)

  md <- readLines(file.path(out, "release-notes-code.md"))
  expect_false(any(grepl("failed to analyze", md)))
})

test_that("the table quotes the version the database marks as the latest one", {
  # Three stored versions, the marked one written first, so a renderer that
  # took the newest row instead of the marked one would quote 0.5.0.
  out <- withr::local_tempdir()
  summary_df <- data.frame(
    package             = rep("multi", 3L),
    version             = c("1.0.0", "0.9.0", "0.5.0"),
    loc_r               = c(300L, 200L, 100L),
    n_exports           = c(9L, 6L, 3L),
    n_internal          = c(1L, 1L, 1L),
    n_deps_direct       = c(2L, 2L, 2L),
    latest_release_date = c("2026-06-01", NA, NA),
    stringsAsFactors = FALSE)
  export_metrics(file.path(out, DB_FILENAME), summary_df, .empty_churn_rn(), .empty_api_rn())
  write_manifest(file.path(out, "code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = DB_FILENAME,
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 100000L,
    fingerprint = strrep("a", 64), n_packages = 1L, n_versions = 3L,
    tables = list(cran_code_summary = 3L), stats = list(),
    bootstrap = list(n_analyzed = 1L, n_universe = 1L, n_remaining = 0L,
      bootstrap_complete = TRUE)))
  write_manifest(file.path(out, "data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = DATA_DB_FILENAME,
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 20000L,
    fingerprint = strrep("b", 64), n_packages = 0L, n_versions = 0L,
    tables = list(cran_datasets = 0L), stats = list(),
    bootstrap = list(n_analyzed = 1L, n_universe = 1L, n_remaining = 0L,
      bootstrap_complete = TRUE)))
  writeLines("multi", file.path(out, "changed-packages.txt"))
  writeLines("multi", file.path(out, "seed-packages.txt"))

  render_notes(out)
  md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl("^\\| multi \\| 1\\.0\\.0 \\| 300 \\|", md)))
  expect_false(any(grepl("0\\.5\\.0", md)))
})

test_that("render_notes writes 'No package changes in this release.' when nothing changed", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out, changed = character(0L))
  render_notes(out, prev_code_tag = NULL, prev_data_tag = NULL, title_prefix = "CRAN")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl("^0 packages new to the catalog, 0 updated\\.", code_md)))
  expect_true(any(grepl("^No package changes in this release\\.$", code_md)))
  expect_false(any(grepl("^\\|", code_md)))  # no table rows at all
  expect_false(any(grepl("^# ", code_md)))
})

test_that("render_notes treats an absent seed-packages.txt as an empty seed set", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out)
  unlink(file.path(out, "seed-packages.txt"))
  render_notes(out, prev_code_tag = NULL, prev_data_tag = NULL, title_prefix = "CRAN")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  # Both packages now count as new (no seed set at all).
  expect_true(any(grepl("^2 packages new to the catalog, 0 updated\\.", code_md)))
  expect_true(any(grepl("data\\.table \\| 1\\.15\\.0 \\(new\\)", code_md)))
})
