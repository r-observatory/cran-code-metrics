# tests/testthat/test-release-text.R: the DESCRIPTION and release-notes text
# kept per analysed version. Identical in both code-metrics pipelines.

.rt_notes <- function(version, text = paste("- changes in", version),
                      truncated = FALSE) {
  list(rec = "release_notes", package_version = version, news_file = "NEWS.md",
       release_notes_source = "news_md", release_notes = text,
       release_notes_truncated = truncated)
}

.rt_dcf <- function(version, extra = character(0L)) {
  c(Package = "pkgA", Version = version, Title = "A Package",
    RoxygenNote = "7.3.2", `Config/testthat/edition` = "3", extra)
}

.rt_package <- function(pkg = "pkgA", versions = c("1.0", "1.1")) {
  rows <- lapply(versions, function(v) {
    .release_text_rows(pkg, v, .rt_dcf(v), .rt_notes(v), "0.5.0",
                       read_at = "2026-09-26T00:00:00Z")
  })
  .release_text_collect(rows, versions[[length(versions)]])
}

.rt_dbs <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  con <- open_or_init_db(file.path(dir, "code.db"))
  withr::defer(DBI::dbDisconnect(con), envir = frame)
  text_con <- open_or_init_release_text_db(file.path(dir, "text.db"))
  withr::defer(DBI::dbDisconnect(text_con), envir = frame)
  list(con = con, text_con = text_con)
}

.rt_count <- function(con, table, where = "1 = 1") {
  DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) n FROM "%s" WHERE %s', table, where))$n
}

test_that("a value cut to the byte budget stays valid UTF-8 and says it was cut", {
  # The leading "a" puts the second byte of a character just past the budget, so
  # a cut that does not back off to the character's first byte leaves a broken one.
  long <- paste0("a", strrep("é", 10000L))
  cut <- .cap_utf8_bytes(c(long, "short", NA), RELEASE_TEXT_FIELD_MAX_BYTES)
  expect_identical(nchar(cut[[1L]], type = "bytes"), RELEASE_TEXT_FIELD_MAX_BYTES - 1L)
  expect_true(validUTF8(cut[[1L]]))
  expect_identical(cut[[2L]], "short")
  expect_true(is.na(cut[[3L]]))

  rows <- list(.release_text_rows("pkgA", "1.0",
                                  c(SystemRequirements = long), NULL, "0.5.0"))
  latest <- .release_text_collect(rows, "1.0")$description_latest
  expect_identical(latest$value_truncated, 1L)
  expect_true(validUTF8(latest$value))
})

test_that("the latest rows are cut to the byte budget and the history keeps the whole text", {
  long <- paste0("a", strrep("é", 10000L))
  rows <- list(.release_text_rows("pkgA", "1.0", c(SystemRequirements = long),
                                  .rt_notes("1.0", long), "0.5.0"))
  text <- .release_text_collect(rows, "1.0")
  kept <- RELEASE_TEXT_FIELD_MAX_BYTES - 1L
  expect_identical(nchar(text$description_latest$value, type = "bytes"), kept)
  expect_identical(nchar(text$release_notes_latest$release_notes, type = "bytes"), kept)
  expect_true(validUTF8(text$release_notes_latest$release_notes))
  expect_identical(text$release_notes_latest$release_notes_truncated, 1L)
  expect_identical(nchar(text$description$value, type = "bytes"), 20001L)
  expect_identical(nchar(text$release_notes$release_notes, type = "bytes"), 20001L)

  # A section the analyzer already cut keeps saying so under the budget.
  own <- .release_text_collect(list(.release_text_rows(
    "pkgA", "1.1", NULL, .rt_notes("1.1", truncated = TRUE), "0.5.0")), "1.1")
  expect_identical(own$release_notes_latest$release_notes_truncated, 1L)
})

test_that("a new code database has both latest-only tables before any shard writes text", {
  db <- .rt_dbs()
  expect_true(all(c(DESCRIPTION_FIELDS_TABLE, RELEASE_NOTES_TABLE) %in%
                    DBI::dbListTables(db$con)))
})

test_that("an empty DESCRIPTION record and a missing one both leave a versions row", {
  empty <- .release_text_rows("pkgA", "1.0", stats::setNames(character(0L), character(0L)),
                              NULL, "0.5.0")
  expect_equal(nrow(empty$description), 0L)
  expect_identical(empty$versions$n_fields, 0L)
  none <- .release_text_rows("pkgA", "1.1", NULL, NULL, "0.5.0")
  expect_true(is.na(none$versions$n_fields))
  expect_identical(none$versions$has_release_notes, 0L)
})

test_that("the merged tables hold only the latest version and only the kept fields", {
  text <- .rt_package(versions = c("1.0", "1.1"))
  expect_setequal(unique(text$description$version), c("1.0", "1.1"))
  expect_identical(unique(text$description_latest$version), "1.1")
  expect_setequal(text$description_latest$field,
                  c("RoxygenNote", "Config/testthat/edition"))
  expect_identical(text$release_notes_latest$version, "1.1")
  expect_identical(text$release_notes_latest$package_version, "1.1")

  db <- .rt_dbs()
  upsert_shard(db$con, data.frame(package = "pkgA", version = c("1.0", "1.1"),
                                  stringsAsFactors = FALSE),
               churn_df = .empty_churn(), api_df = .empty_api(),
               description_df = text$description_latest,
               release_notes_df = text$release_notes_latest)
  got <- DBI::dbGetQuery(db$con, sprintf('SELECT * FROM "%s" ORDER BY field',
                                         DESCRIPTION_FIELDS_TABLE))
  expect_identical(got$field, c("Config/testthat/edition", "RoxygenNote"))
  expect_identical(unique(got$version), "1.1")
  expect_equal(.rt_count(db$con, RELEASE_NOTES_TABLE), 1L)
})

test_that("a package moving to a new latest version leaves no older merged rows", {
  db <- .rt_dbs()
  one <- .rt_package(versions = "1.0")
  upsert_shard(db$con, data.frame(package = "pkgA", version = "1.0", stringsAsFactors = FALSE),
               churn_df = .empty_churn(), api_df = .empty_api(),
               description_df = one$description_latest,
               release_notes_df = one$release_notes_latest)
  # The newest version fell back to R this time, so it has no text of its own.
  upsert_shard(db$con, data.frame(package = "pkgA", version = c("1.0", "1.1"),
                                  stringsAsFactors = FALSE),
               churn_df = .empty_churn(), api_df = .empty_api(),
               description_df = .empty_description_fields_rows(),
               release_notes_df = .empty_release_notes_rows())
  expect_equal(.rt_count(db$con, DESCRIPTION_FIELDS_TABLE), 0L)
  expect_equal(.rt_count(db$con, RELEASE_NOTES_TABLE), 0L)
})

test_that("the history replaces the versions it read and keeps the ones it did not", {
  db <- .rt_dbs()
  first <- .rt_package(versions = c("1.0", "1.1"))
  upsert_release_text(db$text_con, first$description, first$release_notes, first$versions)
  expect_equal(.rt_count(db$text_con, RELEASE_TEXT_VERSIONS_TABLE), 2L)

  # Only 1.1 was read this time, with a DESCRIPTION that dropped a field.
  again <- .release_text_collect(list(.release_text_rows(
    "pkgA", "1.1", c(Package = "pkgA", Version = "1.1"), NULL, "0.5.0")), "1.1")
  upsert_release_text(db$text_con, again$description, again$release_notes, again$versions)

  expect_equal(.rt_count(db$text_con, DESCRIPTION_HISTORY_TABLE, "version = '1.0'"), 5L)
  expect_equal(.rt_count(db$text_con, DESCRIPTION_HISTORY_TABLE, "version = '1.1'"), 2L)
  expect_equal(.rt_count(db$text_con, RELEASE_NOTES_HISTORY_TABLE, "version = '1.1'"), 0L)
  expect_equal(.rt_count(db$text_con, RELEASE_NOTES_HISTORY_TABLE, "version = '1.0'"), 1L)
  expect_equal(.rt_count(db$text_con, RELEASE_TEXT_VERSIONS_TABLE), 2L)
})

test_that("a version read twice keeps its last reading, as the summary row does", {
  # upsert_shard keeps the last row of a doubled version; the text keys must not
  # fail the shard on the same input.
  first  <- .release_text_rows("pkgA", "1.0", .rt_dcf("1.0"), .rt_notes("1.0", "- old"),
                               "0.5.0")
  second <- .release_text_rows("pkgA", "1.0", .rt_dcf("1.0", c(Date = "2026-01-01")),
                               .rt_notes("1.0", "- new"), "0.5.0")
  text <- .release_text_collect(list(first, second), "1.0")
  expect_equal(nrow(text$versions), 1L)
  expect_identical(text$release_notes$release_notes, "- new")
  expect_true("Date" %in% text$description_latest$field)

  db <- .rt_dbs()
  expect_no_error(upsert_release_text(db$text_con, text$description, text$release_notes,
                                      text$versions))
  expect_no_error(upsert_shard(db$con, data.frame(package = "pkgA", version = "1.0",
                                                  stringsAsFactors = FALSE),
                               churn_df = .empty_churn(), api_df = .empty_api(),
                               description_df = text$description_latest,
                               release_notes_df = text$release_notes_latest))
  expect_equal(.rt_count(db$text_con, DESCRIPTION_HISTORY_TABLE), 6L)
  expect_equal(.rt_count(db$con, DESCRIPTION_FIELDS_TABLE), 3L)
})

test_that("a latest version with no NEWS section of its own shows no older notes", {
  # A NEWS file not updated for the release has no section for it, and the merged
  # table must not present an older version's notes as the latest ones.
  rows <- list(
    .release_text_rows("pkgA", "1.0", .rt_dcf("1.0"), .rt_notes("1.0"), "0.5.0"),
    .release_text_rows("pkgA", "1.1", .rt_dcf("1.1"), NULL, "0.5.0"))
  text <- .release_text_collect(rows, "1.1")
  expect_equal(nrow(text$release_notes), 1L)
  expect_equal(nrow(text$release_notes_latest), 0L)

  db <- .rt_dbs()
  one <- .rt_package(versions = "1.0")
  upsert_shard(db$con, data.frame(package = "pkgA", version = "1.0", stringsAsFactors = FALSE),
               churn_df = .empty_churn(), api_df = .empty_api(),
               description_df = one$description_latest,
               release_notes_df = one$release_notes_latest)
  upsert_shard(db$con, data.frame(package = "pkgA", version = c("1.0", "1.1"),
                                  stringsAsFactors = FALSE),
               churn_df = .empty_churn(), api_df = .empty_api(),
               description_df = text$description_latest,
               release_notes_df = text$release_notes_latest)
  expect_equal(.rt_count(db$con, RELEASE_NOTES_TABLE), 0L)
  expect_identical(unique(DBI::dbGetQuery(db$con, sprintf(
    'SELECT version FROM "%s"', DESCRIPTION_FIELDS_TABLE))$version), "1.1")
})

test_that("history values are stored whole, however long", {
  db <- .rt_dbs()
  long <- strrep("x", 40000L)
  rows <- list(.release_text_rows("pkgA", "1.0", c(Description = long), NULL, "0.5.0"))
  text <- .release_text_collect(rows, "1.0")
  upsert_release_text(db$text_con, text$description, text$release_notes, text$versions)
  got <- DBI::dbGetQuery(db$text_con, sprintf('SELECT value FROM "%s"',
                                              DESCRIPTION_HISTORY_TABLE))$value
  expect_equal(nchar(got), 40000L)
})

test_that("the command line sources every script the tests source", {
  # A helper only the test runner loads passes every test and fails in production.
  runner <- readLines(file.path("..", "testthat.R"))
  cli    <- readLines(file.path("..", "..", "scripts", "update.R"))
  tested <- sub('^source\\("scripts/(.*)"\\)$', "\\1",
                grep('^source\\("scripts/[^"]+"\\)$', runner, value = TRUE))
  loaded <- sub('.*source\\(file\\.path\\(\\.script_dir, "([^"]+)"\\)\\).*', "\\1",
                grep('source\\(file\\.path\\(\\.script_dir, "[^"]+"\\)\\)', cli, value = TRUE))
  runner_only <- c("update.R", "preflight.R", "prune.R", "render_notes.R")
  expect_identical(setdiff(tested, c(loaded, runner_only)), character(0L))
  expect_true("release_text.R" %in% loaded)
})
