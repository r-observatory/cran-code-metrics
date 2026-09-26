# scripts/release_text.R: the DESCRIPTION and release notes the analyzer prints per
# version. Identical in both code-metrics pipelines; config.R names the tables.

# DESCRIPTION fields the merged latest-only table keeps, besides every Config/* field.
.LATEST_DESCRIPTION_FIELDS <- c("RdMacros", "RoxygenNote", "SystemRequirements",
                                "Language", "LazyData", "Date")

.RELEASE_NOTES_COLS <- c("package", "version", "package_version", "news_file",
                         "release_notes_source", "release_notes",
                         "release_notes_truncated")

.empty_description_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             field = character(0L), value = character(0L),
             stringsAsFactors = FALSE)
}

.empty_description_fields_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             field = character(0L), value = character(0L),
             value_truncated = integer(0L), stringsAsFactors = FALSE)
}

.empty_release_notes_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             package_version = character(0L), news_file = character(0L),
             release_notes_source = character(0L), release_notes = character(0L),
             release_notes_truncated = integer(0L), stringsAsFactors = FALSE)
}

.empty_text_versions_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             analyzer_version = character(0L), n_fields = integer(0L),
             has_release_notes = integer(0L), read_at = character(0L),
             stringsAsFactors = FALSE)
}

.empty_release_text <- function() {
  list(description = .empty_description_rows(),
       release_notes = .empty_release_notes_rows(),
       versions = .empty_text_versions_rows(),
       description_latest = .empty_description_fields_rows(),
       release_notes_latest = .empty_release_notes_rows())
}

#' Cut each string to at most `max_bytes` of UTF-8 without splitting a character.
.cap_utf8_bytes <- function(x, max_bytes) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    b <- charToRaw(enc2utf8(s))
    if (length(b) <= max_bytes) return(s)
    k <- max_bytes
    # A byte of the form 10xxxxxx continues a character, so back off to its start.
    while (k > 0L && bitwAnd(as.integer(b[k + 1L]), 0xC0L) == 0x80L) k <- k - 1L
    out <- rawToChar(b[seq_len(k)])
    Encoding(out) <- "UTF-8"
    out
  }, character(1L), USE.NAMES = FALSE)
}

.description_field_kept <- function(field) {
  field %in% .LATEST_DESCRIPTION_FIELDS | startsWith(field, "Config/")
}

#' The text rows one analysed version contributes. The versions row is written even
#' when the dcf record is empty or absent, so reconciliation never re-reads it forever.
.release_text_rows <- function(package, version, dcf, notes, analyzer_version,
                               read_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
                                                tz = "UTC")) {
  description <- if (length(dcf)) {
    data.frame(package = package, version = version, field = names(dcf),
               value = unname(dcf), stringsAsFactors = FALSE)
  } else {
    .empty_description_rows()
  }
  release_notes <- if (!is.null(notes)) {
    data.frame(package = package, version = version,
               package_version = .rec_chr(notes, "package_version"),
               news_file = .rec_chr(notes, "news_file"),
               release_notes_source = .rec_chr(notes, "release_notes_source"),
               release_notes = .rec_chr(notes, "release_notes"),
               release_notes_truncated = as.integer(.rec_lgl(notes, "release_notes_truncated")),
               stringsAsFactors = FALSE)
  } else {
    .empty_release_notes_rows()
  }
  versions <- data.frame(
    package = package, version = version,
    analyzer_version = as.character(analyzer_version %||% NA_character_),
    n_fields = if (is.null(dcf)) NA_integer_ else length(dcf),
    has_release_notes = as.integer(!is.null(notes)),
    read_at = read_at, stringsAsFactors = FALSE)
  list(description = description, release_notes = release_notes, versions = versions)
}

#' Bind one package's per-version text rows and cap its latest version's rows.
#' NULL entries in `rows` are versions the R fallback wrote.
.release_text_collect <- function(rows, latest_version,
                                  max_bytes = RELEASE_TEXT_FIELD_MAX_BYTES) {
  rows <- Filter(Negate(is.null), rows)
  # A version read twice keeps its last reading, the row upsert_shard keeps.
  read <- vapply(rows, function(r) r$versions$version[[1L]], character(1L))
  rows <- rows[!duplicated(read, fromLast = TRUE)]
  out <- .empty_release_text()
  if (!length(rows)) return(out)
  out$description   <- do.call(rbind, c(list(out$description),
                                        lapply(rows, `[[`, "description")))
  out$release_notes <- do.call(rbind, c(list(out$release_notes),
                                        lapply(rows, `[[`, "release_notes")))
  out$versions      <- do.call(rbind, c(list(out$versions),
                                        lapply(rows, `[[`, "versions")))

  d <- out$description[out$description$version == latest_version &
                         .description_field_kept(out$description$field), , drop = FALSE]
  if (nrow(d)) {
    capped <- .cap_utf8_bytes(d$value, max_bytes)
    out$description_latest <- data.frame(
      package = d$package, version = d$version, field = d$field, value = capped,
      value_truncated = as.integer(!is.na(d$value) &
                                     nchar(d$value, type = "bytes") > max_bytes),
      stringsAsFactors = FALSE)
  }
  n <- out$release_notes[out$release_notes$version == latest_version, , drop = FALSE]
  if (nrow(n)) {
    cut <- !is.na(n$release_notes) & nchar(n$release_notes, type = "bytes") > max_bytes
    n$release_notes <- .cap_utf8_bytes(n$release_notes, max_bytes)
    n$release_notes_truncated[cut] <- 1L
    out$release_notes_latest <- n
  }
  out
}

#' Bind the text of every package in a shard.
.bind_release_text <- function(texts) {
  texts <- Filter(Negate(is.null), texts)
  out <- .empty_release_text()
  for (k in names(out)) {
    out[[k]] <- do.call(rbind, c(list(out[[k]]), lapply(texts, `[[`, k)))
  }
  out
}

.ensure_release_text_tables <- function(con) {
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, field TEXT NOT NULL, value TEXT,
    PRIMARY KEY (package, version, field)) WITHOUT ROWID', DESCRIPTION_HISTORY_TABLE))
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, package_version TEXT,
    news_file TEXT, release_notes_source TEXT, release_notes TEXT,
    release_notes_truncated INTEGER,
    PRIMARY KEY (package, version)) WITHOUT ROWID', RELEASE_NOTES_HISTORY_TABLE))
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, analyzer_version TEXT,
    n_fields INTEGER, has_release_notes INTEGER, read_at TEXT,
    PRIMARY KEY (package, version))', RELEASE_TEXT_VERSIONS_TABLE))
  invisible(NULL)
}

#' Open the database holding the text history and create its tables. Pass `con`
#' when the history shares the code database, as it does on Bioconductor.
open_or_init_release_text_db <- function(path, con = NULL) {
  text_con <- if (is.null(con)) DBI::dbConnect(RSQLite::SQLite(), path) else con
  .ensure_release_text_tables(text_con)
  text_con
}

.ensure_latest_text_tables <- function(con) {
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, field TEXT NOT NULL,
    value TEXT, value_truncated INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (package, field))', DESCRIPTION_FIELDS_TABLE))
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL PRIMARY KEY, version TEXT NOT NULL,
    package_version TEXT, news_file TEXT, release_notes_source TEXT,
    release_notes TEXT, release_notes_truncated INTEGER)', RELEASE_NOTES_TABLE))
  invisible(NULL)
}

#' Replace the history rows of the versions just read, and only those: a version
#' the R fallback wrote this time keeps the history it already has.
upsert_release_text <- function(text_con, description_df, notes_df, versions_df) {
  if (is.null(versions_df) || nrow(versions_df) == 0L) return(invisible(NULL))
  keys <- unique(versions_df[, c("package", "version")])
  DBI::dbWithTransaction(text_con, {
    for (tbl in c(DESCRIPTION_HISTORY_TABLE, RELEASE_NOTES_HISTORY_TABLE,
                  RELEASE_TEXT_VERSIONS_TABLE)) {
      DBI::dbExecute(text_con,
        sprintf('DELETE FROM "%s" WHERE package = ? AND version = ?', tbl),
        params = list(keys$package, keys$version))
    }
    if (!is.null(description_df) && nrow(description_df) > 0L) {
      DBI::dbAppendTable(text_con, DESCRIPTION_HISTORY_TABLE,
                         description_df[, c("package", "version", "field", "value")])
    }
    if (!is.null(notes_df) && nrow(notes_df) > 0L) {
      DBI::dbAppendTable(text_con, RELEASE_NOTES_HISTORY_TABLE,
                         notes_df[, .RELEASE_NOTES_COLS])
    }
    DBI::dbAppendTable(text_con, RELEASE_TEXT_VERSIONS_TABLE, versions_df)
  })
  invisible(NULL)
}

#' Replace the merged latest-only rows of a shard's packages. Runs inside
#' upsert_shard's transaction, the way the vignette rows are replaced.
.write_latest_text <- function(con, pkgs, description_latest, notes_latest) {
  .ensure_latest_text_tables(con)
  .delete_by_package(con, DESCRIPTION_FIELDS_TABLE, pkgs)
  .delete_by_package(con, RELEASE_NOTES_TABLE, pkgs)
  if (!is.null(description_latest) && nrow(description_latest) > 0L) {
    DBI::dbAppendTable(con, DESCRIPTION_FIELDS_TABLE, description_latest)
  }
  if (!is.null(notes_latest) && nrow(notes_latest) > 0L) {
    DBI::dbAppendTable(con, RELEASE_NOTES_TABLE, notes_latest[, .RELEASE_NOTES_COLS])
  }
  invisible(NULL)
}
