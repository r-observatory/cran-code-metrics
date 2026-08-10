# scripts/citation.R: reading inst/CITATION out of a released version.
# Dependency: config.R must be sourced first (CITATION_TIMEOUT).

#' Whether a released version ships the file utils::citation() reads.
#'
#' Computed from the version's file list rather than from extraction, so a
#' package that ships a citation we failed to read is never reported as a
#' package that ships none. inst/CITATION only: a CITATION.cff at the root is
#' the CFF standard read by repository hosts, a different file with a different
#' reader, and man/CITATION or inst/extdata/CITATION are neither.
#'
#' @param files Character vector of paths in the released version.
#' @return 1L when inst/CITATION is present, 0L otherwise.
citation_shipped <- function(files) {
  as.integer(any(files == "inst/CITATION"))
}

#' Zero-row frame of staged citation inputs.
#'
#' One row per version that ships a citation file. `dir` is the directory the
#' two staged files were written to; `source_sha256` identifies the citation
#' bytes so a reader can tell whether the file changed between releases.
.empty_citation_inputs_df <- function() {
  data.frame(
    package       = character(0L),
    version       = character(0L),
    is_current    = integer(0L),
    released      = character(0L),
    source_sha256 = character(0L),
    dir           = character(0L),
    stringsAsFactors = FALSE
  )
}

#' Stage the two files a citation reader needs, and nothing else.
#'
#' The reader is pointed at what is staged here and nothing else: not the
#' extracted package tree, not the clone, not the workspace. Two files go in,
#' and no other path is named to it.
#'
#' What that is worth has changed, so it is worth saying exactly. The original
#' reason was that the clone URL carried a write-scoped token, which git wrote
#' into work/<pkg>/.git/config, and staging kept an evaluated file from being
#' pointed at it. That reason is gone: the clone is unauthenticated (update.R),
#' so there is no credential in the workspace to be kept away from.
#'
#' What staging still achieves is narrower. The two files are copies, so a file
#' that rewrites its own inst/CITATION or DESCRIPTION damages a scratch copy
#' rather than the tree the metrics were computed from, and the reader's inputs
#' do not shift when the analysis's do. What it does not achieve is confinement:
#' the reader is an ordinary process with this user's filesystem access, and an
#' evaluated file can name any path it likes whether or not we named it first.
#' Confinement is elsewhere - the reader is handed an environment built by name
#' rather than inherited, runs under a wall clock plus CPU, address-space and
#' file-size ceilings, and is killed as a process group so that nothing it
#' started outlives it (run_citation_reader()).
#'
#' Bytes are read with readBin rather than through ctx$read, because the context
#' reader collapses readLines output and so drops the trailing newline and
#' normalises CRLF on every file. What R parses is the bytes CRAN shipped.
#'
#' A version is staged per (sha, version) rather than per sha: the version's own
#' DESCRIPTION is bound as `meta`, files read meta$Version, and DESCRIPTION
#' changes on every release, so identical citation bytes still render differently.
#'
#' @param tmp        Extraction directory for this version.
#' @param stage_dir  Per-package staging root.
#' @param package,version  Identity of this version.
#' @param is_current 1L when this is the package's latest version.
#' @param released   Release date string, or NA when unknown.
#' @return One-row data.frame, or .empty_citation_inputs_df() when no citation.
stage_citation_inputs <- function(tmp, stage_dir, package, version,
                                  is_current, released) {
  cit <- file.path(tmp, "inst", "CITATION")
  if (!file.exists(cit)) return(.empty_citation_inputs_df())

  bytes <- tryCatch(readBin(cit, "raw", file.size(cit)),
                    error = function(e) raw(0L))
  if (length(bytes) == 0L) return(.empty_citation_inputs_df())

  sha <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  # Keyed by version as well as sha: see above, meta reaches the file.
  key <- substr(digest::digest(paste(sha, version, sep = "\x1f"),
                               algo = "sha256", serialize = FALSE), 1L, 32L)
  dest <- file.path(stage_dir, key)
  if (!dir.exists(dest)) dir.create(file.path(dest, "inst"), recursive = TRUE)

  writeBin(bytes, file.path(dest, "inst", "CITATION"))

  desc_src <- file.path(tmp, "DESCRIPTION")
  if (file.exists(desc_src)) {
    file.copy(desc_src, file.path(dest, "DESCRIPTION"), overwrite = TRUE)
  } else {
    # An absent DESCRIPTION is a malformed tarball, not a reason to skip. The
    # reader reports the resulting encoding or meta failure on its own terms.
    writeLines(character(0L), file.path(dest, "DESCRIPTION"))
  }

  data.frame(
    package       = package,
    version       = version,
    is_current    = as.integer(is_current),
    released      = if (is.null(released)) NA_character_ else as.character(released),
    source_sha256 = sha,
    dir           = dest,
    stringsAsFactors = FALSE
  )
}

#' Classify a reader run from its output and exit status.
#'
#' The terminator is the contract. A stream without one means the process died
#' partway, and treating that as an empty result would record a package that
#' ships a citation as one that ships none. Anything after the terminator is not
#' ours: a finalizer registered by an evaluated file runs at R shutdown, which is
#' after the terminator is written.
.citation_outcome <- function(lines, rc) {
  if (identical(as.integer(rc), 124L)) return("timeout")
  end <- which(lines == "{\"t\":\"end\"}")
  if (length(end) == 0L) return("crashed")
  if (end[[1L]] != length(lines)) return("crashed")
  "ok"
}

#' The environment the reader is handed, built by name instead of inherited.
#'
#' A CITATION file is R code, and evaluating it runs Sys.getenv() if the file
#' asks to. That is not hypothetical: a file that reads GITHUB_TOKEN and puts
#' the value in its title lands the token in the stored title, and the
#' environment this pipeline runs under carries that token with push rights.
#' So the reader starts from an empty environment with only the few names R
#' needs to start copied in, and a name that is not listed here does not exist
#' for the code being evaluated.
#'
#' An allowlist rather than unsetting the dangerous names one by one: a
#' blocklist has to be kept in step with every credential any caller might ever
#' export, and the first one nobody thought of is the one that leaks.
#'
#' @return Character vector of NAME=value assignments.
.citation_child_env <- function() {
  # PATH and HOME so R starts and path.expand() has something to expand;
  # TMPDIR because the reader writes temporaries; TZ and the locale names
  # because they decide how dates and non-ASCII citation text render, and a
  # reader that silently changed encoding relative to the process that staged
  # its input would store different bytes than the same file does today.
  keep <- c("PATH", "HOME", "TMPDIR", "TZ", "LANG", "LC_ALL", "LC_CTYPE")
  vals <- Sys.getenv(keep, unset = NA_character_)
  vals <- vals[!is.na(vals)]
  c(sprintf("R_HOME=%s", R.home()), sprintf("%s=%s", names(vals), vals))
}

#' This process's own process group, or NA when it cannot be determined.
#'
#' Read rather than assumed, because it is the only thing standing between
#' .kill_process_group() and this shard killing itself. NA when ps(1) is absent
#' or says something unexpected, and every caller treats NA as "do not kill":
#' losing the group kill costs one stray process, and getting it wrong costs the
#' worker and every package still queued behind it.
.own_process_group <- function() {
  out <- tryCatch(
    suppressWarnings(system2("ps", c("-o", "pgid=", "-p", Sys.getpid()),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0L))
  out <- trimws(out)
  out <- out[nzchar(out)]
  if (length(out) != 1L || !grepl("^[0-9]+$", out)) return(NA_integer_)
  suppressWarnings(as.integer(out))
}

#' The reader's process group, as its own launching shell recorded it.
#'
#' Refused, rather than returned, unless it is a positive integer that is
#' neither an init-like group nor this process's own. That last check is what
#' makes the kill safe: system2(timeout=) runs its command in a new process
#' group, so a value equal to ours means the isolation did not happen and
#' killing it would take this worker down with the reader.
#'
#' @param path Path the launching shell wrote its process group id to.
#' @param own  This process's group, from .own_process_group().
#' @return Integer process group, or NA when there is nothing safe to kill.
.reader_process_group <- function(path, own = .own_process_group()) {
  if (is.na(own)) return(NA_integer_)
  txt <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0L))
  txt <- trimws(txt)
  txt <- txt[nzchar(txt)]
  if (length(txt) != 1L || !grepl("^[0-9]+$", txt)) return(NA_integer_)
  pgid <- suppressWarnings(as.integer(txt))
  if (is.na(pgid) || pgid <= 1L || identical(pgid, own)) return(NA_integer_)
  pgid
}

#' SIGKILL a whole process group.
#'
#' Through the shell rather than tools::pskill(), which refuses a negative pid
#' and so cannot address a group at all: pskill(-pgid) returns FALSE without
#' signalling anything. A group that is already empty is not an error worth
#' reporting - the common case is that the reader exited cleanly and left
#' nothing behind - so the exit status is discarded.
#'
#' @param pgid Process group from .reader_process_group(); NA does nothing.
#' @return TRUE when a kill was attempted, FALSE when there was nothing to kill.
.kill_process_group <- function(pgid) {
  if (length(pgid) != 1L || is.na(pgid)) return(invisible(FALSE))
  tryCatch(
    suppressWarnings(system2("/bin/sh",
                             c("-c", shQuote(sprintf("kill -9 -%d", pgid))),
                             stdout = FALSE, stderr = FALSE)),
    error = function(e) NULL)
  invisible(TRUE)
}

#' The single shell command that starts one reader.
#'
#' Assembled in one place because the three things it has to do interact, and
#' each of them is wrong on its own.
#'
#' It records the process group before starting anything, from `$$` rather than
#' from the reader's pid: the reader usually exits in well under a second, and
#' asking ps(1) about a process that has already gone gives nothing at all. The
#' launching shell is in the same group and is alive by definition.
#'
#' It applies the ceilings one per line rather than as a single ulimit call, so
#' that a limit the platform rejects (macOS refuses -v) does not take the others
#' down with it, and tolerates each failure so that a shell without one of them
#' still runs the reader.
#'
#' It starts the reader in the background and waits, rather than exec-ing it.
#' The reader ends by killing itself (cite_reader.R), and whichever shell has it
#' as a direct child announces that death on its own stderr. exec-ing hands that
#' role to the shell R wraps every system2() in, whose stderr is this process's
#' and cannot be redirected from here - which is where a 2 KB `Killed: 9` line
#' per successful package came from. Waiting keeps the announcement inside a
#' shell whose stderr system2() has already pointed at /dev/null, and lets that
#' shell exit with the reader's status so R sees an ordinary exit.
#'
#' @param envbin,rscript Absolute paths to env(1) and Rscript.
#' @param argv    Arguments for Rscript.
#' @param pgid_path Where the shell should record its process group.
#' @return A single /bin/sh command string.
.citation_reader_command <- function(envbin, rscript, argv, pgid_path) {
  # system2 pastes args into a shell string and quotes only the command, so
  # every element has to be quoted here.
  reader <- paste(shQuote(envbin), "-i",
                  paste(shQuote(.citation_child_env()), collapse = " "),
                  shQuote(rscript),
                  paste(shQuote(argv), collapse = " "))
  paste(c(
    sprintf("ulimit -t %d 2>/dev/null || :", CITATION_CPU_SECONDS),
    sprintf("ulimit -f %d 2>/dev/null || :", CITATION_FILE_SIZE_KB),
    sprintf("ulimit -v %d 2>/dev/null || :", CITATION_ADDRESS_SPACE_KB),
    sprintf("ps -o pgid= -p $$ 2>/dev/null | tr -d ' ' > %s || :",
            shQuote(pgid_path)),
    paste(reader, "& reader=$!"),
    "wait $reader",
    "exit $?"
  ), collapse = "; ")
}

#' Evaluate one package's staged citation files.
#'
#' One reader process per package. Never one per version, because process start
#' dominates the cost; never one per shard, because a file in package A must not
#' run in a process that will later serialise package B's record.
#'
#' A subprocess rather than this session, for two reasons that have nothing to
#' do with what else is on the machine: an evaluated file that calls q() or
#' segfaults would otherwise take the worker down in the middle of its shard,
#' and one that calls cat() would otherwise write into the record stream this
#' function reads back.
#'
#' @param inputs_df Frame from stage_citation_inputs(), one row per version.
#' @param reader_path Path to scripts/cite_reader.R.
#' @return list(lines, outcome, message)
run_citation_reader <- function(inputs_df, reader_path) {
  empty <- function(outcome, message = "") {
    list(lines = character(0L), outcome = outcome, message = message)
  }
  if (is.null(inputs_df) || nrow(inputs_df) == 0L) return(empty("skipped"))

  # env(1) is what makes the allowlist an allowlist. system2()'s own `env`
  # argument only prepends NAME=value assignments to the shell command, so it
  # sets the names it is given and leaves every other one inherited - it can
  # add a variable but never remove one, and removing is the point here.
  # `env -i` starts the child from nothing instead. Refused rather than run
  # with this process's environment attached if env(1) is not there.
  envbin <- Sys.which("env")
  if (!nzchar(envbin)) {
    return(empty("crashed", paste("env(1) is required to run the citation reader",
                                  "without this process's environment")))
  }

  root <- dirname(inputs_df$dir[[1L]])
  man  <- file.path(root, "manifest.tsv")
  outf <- file.path(root, "out.ndjson")
  ids  <- basename(inputs_df$dir)

  writeLines(paste(ids, inputs_df$dir,
                   ifelse(is.na(inputs_df$released), "NA", inputs_df$released),
                   sep = "\t"), man)
  file.create(outf)

  # Resolved here rather than inside the suppressWarnings() below:
  # normalizePath() warns when the path does not exist, and that warning is how
  # a caller learns the reader could not be found at all, rather than seeing
  # every version of every package quietly read as crashed.
  argv <- c("--vanilla", normalizePath(reader_path), man, outf)
  # R.home()'s own Rscript rather than whatever PATH resolves first, so the
  # reader runs under the R this pipeline is running under and the R_HOME it is
  # handed names that same installation.
  rscript <- file.path(R.home("bin"), "Rscript")

  # Deliberately not under the staging root: that root is the one directory an
  # evaluated file is told about, and this file decides what gets a SIGKILL.
  # The pid is in the name explicitly rather than left to tempfile(): these run
  # in forked mclapply workers that share a temporary directory, and two workers
  # agreeing on this path would have each of them killing the other's reader.
  pgidf <- tempfile(sprintf("ccm_reader_pgid_%d_", Sys.getpid()))
  on.exit(unlink(pgidf), add = TRUE)

  rc <- tryCatch(
    suppressWarnings(
      system2("/bin/sh",
              c("-c", shQuote(.citation_reader_command(envbin, rscript, argv, pgidf))),
              stdout = FALSE, stderr = FALSE, timeout = CITATION_TIMEOUT)),
    error = function(e) 1L
  )

  # On completion and on timeout alike, because the reader outliving its own
  # exit is the point. system2(timeout=) kills the process it started and
  # nothing else, so a process an evaluated file backgrounded is reparented and
  # keeps running - long enough to write to the database this run is about to
  # publish. Killing the group reaches it whether the reader exited cleanly, was
  # killed by a ceiling, or ran out of wall clock.
  .kill_process_group(.reader_process_group(pgidf))

  lines <- tryCatch(readLines(outf, warn = FALSE), error = function(e) character(0L))
  list(lines = lines, outcome = .citation_outcome(lines, rc), message = "")
}

.empty_citations_df <- function() {
  data.frame(package = character(0L), version = character(0L),
             is_current = integer(0L), payload_id = character(0L),
             source_sha256 = character(0L), status = character(0L),
             released_known = integer(0L), message = character(0L),
             evaluated_at = character(0L), stringsAsFactors = FALSE)
}

.empty_citation_payloads_df <- function() {
  data.frame(payload_id = character(0L), n_entries = integer(0L),
             mheader = character(0L), mfooter = character(0L),
             header_scope = character(0L), stringsAsFactors = FALSE)
}

.empty_citation_entries_df <- function() {
  data.frame(payload_id = character(0L), entry_index = integer(0L),
             bibtype = character(0L), title = character(0L), year = character(0L),
             authors_json = character(0L), fields_json = character(0L),
             text_version_json = character(0L), header = character(0L),
             footer = character(0L), entry_key = character(0L),
             fmt_bibtex = character(0L), fmt_citation = character(0L),
             stringsAsFactors = FALSE)
}

#' Normalise a field that may arrive as NULL, an empty JSON array, or a JSON
#' array of several values (`simplifyVector = FALSE` turns `[]` into
#' `list()`, not into something `is.null()` catches). `unlist()` collapses
#' any of those to a plain vector before length is checked, so a length-0
#' result - not a bare `[[1L]]` on it - is what decides NA. Without this,
#' `as.character(list())[[1L]]` throws "subscript out of bounds": a shape
#' the honest reader never emits (.jstr/.jnum always write a scalar or
#' null) but a dishonest one can, and one bad field must not cost the
#' package its whole citation set.
.cit_chr <- function(x) {
  x <- as.character(unlist(x))
  if (!length(x)) NA_character_ else x[[1L]]
}

#' Integer counterpart of .cit_chr(), for fields read as a count or index
#' (`n_entries`, `i`). Same reasoning: a null or `[]` index must normalise
#' to NA rather than reach vapply() as a length-0 result, which would abort
#' the sort with "values must be length 1, but FUN(X[[1]]) result is length 0".
.cit_int <- function(x) {
  x <- suppressWarnings(as.integer(unlist(x)))
  if (!length(x)) NA_integer_ else x[[1L]]
}

# Reachable, and load-bearing. jsonlite::fromJSON() accepts an escaped lone
# UTF-16 surrogate as valid JSON grammar, but the two halves decode
# differently: a lone HIGH surrogate such as "\uD800" decodes to a
# replacement "?" and passes validUTF8(), while a lone LOW surrogate such as
# "\uDC00" decodes to the three bytes ed b0 80, which validUTF8() rejects
# (measured on jsonlite 2.0.0 with R 4.6.1). This function is what catches
# that second case, so it must not be treated as dead code.
.cit_valid <- function(x) {
  x <- x[!is.na(x)]
  length(x) == 0L || all(validUTF8(enc2utf8(x)))
}

#' Turn one package's reader output into the three frames the DB stores.
#'
#' Every record is matched against the manifest that was asked for. A record
#' naming an id that was never requested is discarded: the reader evaluates
#' code an author wrote, and a forged record must not become another package's
#' citation. A requested version with no record is recorded as crashed rather
#' than as empty, because "we could not read it" and "it ships none" are
#' different facts and the viewer says different things about them.
parse_citation_records <- function(lines, inputs_df, outcome, run_message = NA_character_) {
  if (is.null(inputs_df) || nrow(inputs_df) == 0L) {
    return(list(citations = .empty_citations_df(),
                payloads  = .empty_citation_payloads_df(),
                entries   = .empty_citation_entries_df()))
  }
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  want <- basename(inputs_df$dir)

  # A whole-run failure (a reader that could not be launched, a run that hit
  # the wall clock) explains every row alike; a doc that simply never arrived
  # for one version of an otherwise-successful run does not. Kept as its own
  # flag, computed once, so the two are never conflated below.
  is_run_failure <- !identical(outcome, "ok")
  # run_citation_reader() returns "" (not NA) for a clean run; that must not
  # be stored as though it were a real explanation.
  run_message <- if (length(run_message) != 1L || is.na(run_message) ||
                     !nzchar(run_message)) NA_character_ else run_message

  parsed <- list()
  for (l in lines) {
    if (!nzchar(l) || identical(l, "{\"t\":\"end\"}")) next
    r <- tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE),
                  error = function(e) NULL)
    if (is.null(r)) next
    # length() first, not just is.null(): an id that arrived as a JSON array
    # (forged or otherwise) is length 0 (absent), 1, or more, and computing
    # `%in%` on a length-2+ id before checking that throws "'length = 2' in
    # coercion to 'logical(1)'" rather than being refused like any other
    # unrequested id. The length check short-circuits before `%in%` ever
    # sees a multi-element id.
    if (length(r$id) != 1L || !(r$id %in% want)) next
    # Normalise to a plain length-1 character now, once, so every later
    # identical()/vapply(..., character(1)) on r$id (by_id, the per-id
    # Filter()) is comparing a guaranteed scalar rather than whatever shape
    # survived matching - a single-element JSON array such as "id":["k1"]
    # passes the check above (length 1, matches `want`) but is still a
    # one-element list, not a string, and vapply(character(1)) on that
    # throws rather than compares.
    r$id <- .cit_chr(r$id)
    parsed[[length(parsed) + 1L]] <- r
  }

  docs <- Filter(function(r) identical(r$t, "doc"), parsed)
  ents <- Filter(function(r) identical(r$t, "entry"), parsed)
  by_id <- setNames(docs, vapply(docs, function(r) r$id, character(1)))

  cit_rows <- list(); pay_rows <- list(); ent_rows <- list()

  for (k in seq_len(nrow(inputs_df))) {
    id  <- want[[k]]
    doc <- by_id[[id]]
    rel_known <- as.integer(!is.na(inputs_df$released[[k]]))

    if (is_run_failure) {
      status <- outcome; doc <- NULL
    } else if (is.null(doc)) {
      status <- "crashed"
    } else {
      # A status is a value this pipeline defines, not one a package
      # author's evaluated code gets to invent: doc$status is validated
      # here, once, right where it is first taken from the record, so
      # every later branch (the "ok"/"empty" check below, the citations
      # row itself) sees one of exactly three known values or "malformed" -
      # never NA and never an arbitrary string. NA matters specifically:
      # citations.status is NOT NULL, and this frame is written inside the
      # same transaction as the rest of the shard, so one package's `[]`
      # in this one field would otherwise roll back everyone else's work
      # in the batch, not just this package's citation.
      doc_status <- .cit_chr(doc$status)
      status <- if (doc_status %in% c("ok", "empty", "error")) doc_status else "malformed"
    }

    payload_id <- NA_character_
    if (!is.null(doc) && status %in% c("ok", "empty")) {
      mine <- Filter(function(r) identical(r$id, id), ents)
      # .cit_int() rather than a bare as.integer(r$i): a null or `[]` index
      # is length 0, and vapply(..., integer(1)) on a length-0 FUN() result
      # aborts the whole package's parse ("values must be length 1"), not
      # just this one entry.
      idxs <- vapply(mine, function(r) .cit_int(r$i), integer(1))
      mine <- mine[order(idxs)]

      cols <- function(r) c(.cit_chr(r$bibtype), .cit_chr(r$title), .cit_chr(r$year),
                            .cit_chr(r$header), .cit_chr(r$footer), .cit_chr(r$key),
                            .cit_chr(r$bibtex), .cit_chr(r$citation))
      texts <- c(.cit_chr(doc$mheader), .cit_chr(doc$mfooter),
                 unlist(lapply(mine, cols), use.names = FALSE))

      # Invalid UTF-8 *bytes* embedded raw in the JSON source fail the JSON
      # grammar itself, so a field corrupted that way never survives as a
      # string for .cit_valid() to inspect: the whole line fails
      # jsonlite::fromJSON() instead, and that entry silently disappears from
      # `ents` above rather than erroring here. An escaped lone UTF-16
      # surrogate is different: it is legal JSON grammar, so it does survive
      # to become an R string - that is exactly the case .cit_valid() below
      # exists to catch (see its own comment for which half of a pair does).
      # What is checked instead is identity, not just count: the entries
      # that did parse must be exactly 1..n_entries, each index present once.
      # A count match alone is not enough - an entry that vanished and a
      # surviving duplicate of another entry add back up to the count
      # declared while quietly replacing real content with a copy - and a
      # missing, null, or non-integer n_entries on a doc claiming ok/empty is
      # itself evidence of corruption, not a doc with no opinion on how many
      # entries it has.
      declared_n <- .cit_int(doc$n_entries)
      shape_ok <- !is.na(declared_n) && declared_n >= 0L && !anyNA(idxs) &&
                  length(idxs) == declared_n &&
                  identical(sort(idxs), seq_len(declared_n))
      if (!shape_ok) {
        status <- "malformed"
      } else if (!.cit_valid(texts)) {
        status <- "malformed"
      } else {
        # Content only: r$t and r$id are the job's own bookkeeping, not part
        # of what a reader sees rendered, and hashing them in would key the
        # payload to which version produced it rather than to what it says -
        # the exact dedup this hash exists to do would then never fire, since
        # id differs on every version by construction.
        entry_body <- function(r) jsonlite::toJSON(list(
          bibtype = .cit_chr(r$bibtype), title = .cit_chr(r$title),
          year = .cit_chr(r$year), authors = r$authors, fields = r$fields,
          text_version = r$text_version, header = .cit_chr(r$header),
          footer = .cit_chr(r$footer), key = .cit_chr(r$key),
          bibtex = .cit_chr(r$bibtex), citation = .cit_chr(r$citation)
        ), auto_unbox = TRUE)
        body <- paste(vapply(mine, function(r) as.character(entry_body(r)),
                             character(1)), collapse = "\n")
        payload_id <- digest::digest(
          paste(.cit_chr(doc$mheader), .cit_chr(doc$mfooter),
                .cit_chr(doc$header_scope), body, sep = "\x1f"),
          algo = "sha256", serialize = FALSE)

        pay_rows[[payload_id]] <- data.frame(
          payload_id = payload_id,
          n_entries  = length(mine),
          mheader    = .cit_chr(doc$mheader),
          mfooter    = .cit_chr(doc$mfooter),
          header_scope = .cit_chr(doc$header_scope),
          stringsAsFactors = FALSE)

        if (is.null(ent_rows[[payload_id]]) && length(mine) > 0L) {
          ent_rows[[payload_id]] <- do.call(rbind, lapply(seq_along(mine), function(i) {
            r <- mine[[i]]
            data.frame(
              payload_id  = payload_id,
              entry_index = i,
              bibtype     = .cit_chr(r$bibtype),
              title       = .cit_chr(r$title),
              year        = .cit_chr(r$year),
              # null = "null": an absent email or ORCID parses from the reader's
              # JSON as a missing list element, and without this the default
              # encoding renders that as {} - an empty object, which is truthy
              # in JavaScript, so a consumer guarding on the field would render
              # an object rather than treating it as absent.
              authors_json = as.character(jsonlite::toJSON(r$authors,
                                                            auto_unbox = TRUE,
                                                            null = "null")),
              fields_json  = as.character(jsonlite::toJSON(r$fields,  auto_unbox = TRUE)),
              # unlist() first: r$text_version is a list of scalars
              # (simplifyVector = FALSE), and toJSON() on that list directly
              # wraps each element in its own array - ["a","b"] would come out
              # as [["a"],["b"]]. as.character(unlist(...)) flattens it to a
              # plain character vector first, so auto_unbox = FALSE renders one
              # flat JSON array, matching what the reader actually emitted.
              text_version_json = as.character(jsonlite::toJSON(
                                     as.character(unlist(r$text_version)),
                                     auto_unbox = FALSE)),
              header    = .cit_chr(r$header),
              footer    = .cit_chr(r$footer),
              entry_key = .cit_chr(r$key),
              fmt_bibtex   = .cit_chr(r$bibtex),
              fmt_citation = .cit_chr(r$citation),
              stringsAsFactors = FALSE)
          }))
        }
      }
    }

    cit_rows[[k]] <- data.frame(
      package = inputs_df$package[[k]], version = inputs_df$version[[k]],
      is_current = as.integer(inputs_df$is_current[[k]]),
      payload_id = payload_id, source_sha256 = inputs_df$source_sha256[[k]],
      status = status, released_known = rel_known,
      message = if (is_run_failure) run_message
                 else if (is.null(doc)) NA_character_
                 else .cit_chr(doc$message),
      evaluated_at = now, stringsAsFactors = FALSE)
  }

  citations <- do.call(rbind, cit_rows) %||% .empty_citations_df()

  # citations.status is NOT NULL, and this frame is written inside the same
  # transaction as the rest of the shard, so a single NA or unrecognised
  # value here would abort every other package's write in the same batch,
  # not just this one's. Every branch above is meant to leave status as one
  # of exactly these seven strings; asserting it here catches a hole in
  # that reasoning at the point it was made, not as a constraint violation
  # three layers away.
  known_statuses <- c("ok", "empty", "error", "malformed",
                       "crashed", "timeout", "skipped")
  stopifnot(
    "citations$status must never be NA" = !anyNA(citations$status),
    "citations$status must be one of the known statuses" =
      all(citations$status %in% known_statuses)
  )

  list(
    citations = citations,
    payloads  = if (length(pay_rows)) do.call(rbind, unname(pay_rows))
                else .empty_citation_payloads_df(),
    entries   = if (length(ent_rows)) do.call(rbind, unname(ent_rows))
                else .empty_citation_entries_df()
  )
}

#' Evaluate one package's staged citations and shape the result.
#'
#' Wrapped so that nothing here can throw into analyze_package. The worker turns
#' any error from analyze_package into a recorded package failure, and five of
#' those drop the package from every metric this pipeline produces, so a citation
#' file must never be able to cost a package its code metrics.
citation_pass <- function(inputs_df, reader_path) {
  empty <- list(citations = .empty_citations_df(),
                payloads  = .empty_citation_payloads_df(),
                entries   = .empty_citation_entries_df())
  if (is.null(inputs_df) || nrow(inputs_df) == 0L) return(empty)

  tryCatch({
    run <- run_citation_reader(inputs_df, reader_path)
    if (identical(run$outcome, "skipped")) return(empty)
    # run$message carries a whole-run explanation (e.g. "env(1) is required to
    # run the citation reader without this process's environment") that applies
    # to every row this call produces, not to any one version's own record.
    # Threaded through rather than discarded, or a misconfigured run is
    # indistinguishable in the stored data from every version's reader having
    # independently crashed.
    parse_citation_records(run$lines, inputs_df, run$outcome, run$message)
  }, error = function(e) {
    warning(sprintf("citation pass failed for '%s': %s",
                    inputs_df$package[[1L]], conditionMessage(e)))
    # A second tryCatch, not just the outer one: parse_citation_records() has
    # its own stopifnot assertions, and this fallback call must not be able to
    # throw either. Only "crashed" ever reaches it here, which is why those
    # assertions cannot fire today, but that is a fact about the current call
    # site, not a guarantee parse_citation_records() itself makes - the
    # "nothing here can throw" contract this function exists for should not
    # depend on that staying true.
    tryCatch(
      list(citations = parse_citation_records(character(0L), inputs_df, "crashed")$citations,
           payloads  = .empty_citation_payloads_df(),
           entries   = .empty_citation_entries_df()),
      error = function(e2) empty
    )
  })
}

#' Directory this file was itself source()d from, found by walking the call
#' stack for a source() frame's own `ofile` local rather than trusting the
#' process's entry point. commandArgs()'s --file= names whatever script
#' Rscript was originally invoked on; that is scripts/update.R in production,
#' but under testthat it is tests/testthat.R while the working directory has
#' already been moved to tests/testthat/, and any future wrapper that
#' source()s update.R inherits the same mismatch. Computed once at source
#' time, since sys.frame() only sees frames live on the stack right now.
.CITATION_SOURCE_DIR <- local({
  d <- NA_character_
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (is.character(of) && length(of) == 1L && nzchar(of))
      d <- dirname(normalizePath(of, mustWork = FALSE))
  }
  d
})

#' Path to the reader script.
#'
#' Tried in order: the directory citation.R was itself source()d from (correct
#' regardless of what the process's entry point was), then the entry-point-based
#' guess update.R's own CLI block uses for its sources, then a bare "scripts/"
#' fallback. Each candidate is required to actually exist before being
#' returned, so a resolution failure here is loud (the caller sees a path that
#' fails file.exists()) rather than silently producing a wrong-but-plausible
#' string that only fails once run_citation_reader() tries to use it.
citation_reader_path <- function() {
  if (!is.na(.CITATION_SOURCE_DIR)) {
    p <- file.path(.CITATION_SOURCE_DIR, "cite_reader.R")
    if (file.exists(p)) return(p)
  }
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  base <- if (length(fa) >= 1L) dirname(sub("^--file=", "", fa[1L])) else "scripts"
  p <- file.path(base, "cite_reader.R")
  if (file.exists(p)) return(p)
  file.path("scripts", "cite_reader.R")
}
