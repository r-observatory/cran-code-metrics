# scripts/citation.R: reading inst/CITATION out of a released version.
# Dependency: config.R, context.R must be sourced first.

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
#' One row per version that ships a citation file. `dir` is the staging
#' directory the container will mount; `source_sha256` identifies the citation
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
#' The container sees only what is staged here. It is deliberately not given the
#' extracted package tree, the clone, or the workspace: the clone URL carries a
#' write-scoped token (git.R:16-27 writes it into work/<pkg>/.git/config), so an
#' evaluated citation file that can read the filesystem could otherwise exfiltrate
#' it. Two files go in, nothing else is reachable.
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

#' Whether a container is available to evaluate citation files in.
citation_sandbox_mode <- function() {
  bin <- Sys.which("docker")
  if (!nzchar(bin)) return("none")
  rc <- suppressWarnings(system2(bin, c("info", "--format", "{{.ServerVersion}}"),
                                 stdout = FALSE, stderr = FALSE, timeout = 20))
  if (identical(rc, 0L)) "docker" else "none"
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

#' Make a staged tree traversable and readable by any uid.
#'
#' dir.create() and file.copy() apply the process umask, so under a
#' restrictive umask (027, 077) the tree stage_citation_inputs() built can
#' come out 0750 or 0700. The container's non-root user then cannot even
#' traverse into /stage/<id>. That does not crash: the reader's per-job
#' tryCatch turns the resulting open() failure into a "status":"error" record
#' for every job, the terminator still gets written, and .citation_outcome()
#' reports "ok" - a failure that fails open into permanently wrong data is
#' worse than one that crashes. Fixed here, right before the reader runs, so
#' it covers whatever staging produced regardless of the umask that built it.
.citation_make_stage_readable <- function(root) {
  paths <- list.files(root, recursive = TRUE, all.files = TRUE,
                      full.names = TRUE, include.dirs = TRUE, no.. = TRUE)
  paths <- c(root, paths)
  is_dir <- file.info(paths)$isdir
  is_dir[is.na(is_dir)] <- FALSE
  # a+rX: directories get read+traverse, files get read only. use_umask =
  # FALSE so the umask that caused the problem cannot re-narrow the fix.
  if (any(is_dir))  Sys.chmod(paths[is_dir],  mode = "0755", use_umask = FALSE)
  if (any(!is_dir)) Sys.chmod(paths[!is_dir], mode = "0644", use_umask = FALSE)
}

#' Build the docker argument vector that runs the reader with nothing else
#' reachable.
#'
#' Extracted from run_citation_reader() so the flag set has one definition and
#' so it can be unit-tested without a daemon: these flags are the security
#' boundary, and a dropped or misspelled one would otherwise fail nothing.
#'
#' @param root Host staging root, mounted read-only at /stage.
#' @param outf Host output file, mounted read-write at /out.ndjson.
#' @param reader_path Path to scripts/cite_reader.R, mounted read-only at /reader.R.
#' @param image Image reference to run.
#' @return Character vector of docker arguments, command name not included.
.citation_docker_argv <- function(root, outf, reader_path, image) {
  c("run", "--rm",
    "--network", "none",
    "--read-only",
    "--cap-drop", "ALL",
    "--security-opt", "no-new-privileges",
    "--user", "65534:65534",
    "--pids-limit", "128",
    "--memory", "1g",
    "--cpus", "1",
    "--tmpfs", "/tmp:rw,noexec,nosuid,size=64m",
    "-v", paste0(root, ":/stage:ro"),
    "-v", paste0(outf, ":/out.ndjson:rw"),
    "-v", paste0(normalizePath(reader_path), ":/reader.R:ro"),
    "-e", "HOME=/tmp",
    image,
    "Rscript", "--vanilla", "/reader.R", "/stage/manifest.tsv", "/out.ndjson")
}

#' Evaluate one package's staged citation files.
#'
#' One container per package. Never one per version, because container start
#' dominates the cost; never one per shard, because a file in package A must not
#' run in a process that will later serialise package B's record.
#'
#' @param inputs_df Frame from stage_citation_inputs(), one row per version.
#' @param reader_path Path to scripts/cite_reader.R.
#' @return list(lines, outcome, message)
run_citation_reader <- function(inputs_df, reader_path) {
  empty <- function(outcome, message = "") {
    list(lines = character(0L), outcome = outcome, message = message)
  }
  if (is.null(inputs_df) || nrow(inputs_df) == 0L) return(empty("skipped"))

  mode <- citation_sandbox_mode()
  # Read live rather than through the CITATION_SANDBOX constant: config.R's
  # top-level `<-` captures Sys.getenv() once at source time, before a caller
  # (production or test) can change it. RPKG_ANALYZER_BIN in binary.R uses the
  # same live-read pattern for the same reason.
  sandbox_setting <- Sys.getenv("CITATION_SANDBOX", unset = "allow")
  if (mode == "none" && !identical(sandbox_setting, "allow")) {
    return(empty("crashed", "a container is required to evaluate citation files"))
  }

  root <- dirname(inputs_df$dir[[1L]])
  man  <- file.path(root, "manifest.tsv")
  outf <- file.path(root, "out.ndjson")
  ids  <- basename(inputs_df$dir)

  writeLines(paste(ids,
                   if (mode == "docker") file.path("/stage", ids) else inputs_df$dir,
                   ifelse(is.na(inputs_df$released), "NA", inputs_df$released),
                   sep = "\t"), man)
  file.create(outf)
  # Sweep the whole staging root readable first - this also covers man, which
  # the container reads from /stage - then widen outf last, after the sweep,
  # so the sweep's own 0644 on outf (it is a file under root too) does not
  # clobber the write grant the container needs to produce it.
  .citation_make_stage_readable(root)
  Sys.chmod(outf, mode = "0666", use_umask = FALSE)

  argv <- if (mode == "docker") {
    .citation_docker_argv(root, outf, reader_path, CITATION_IMAGE)
  } else {
    c("--vanilla", normalizePath(reader_path), man, outf)
  }
  cmd <- if (mode == "docker") Sys.which("docker") else Sys.which("Rscript")

  # system2 pastes args into a shell string and quotes only the command, so
  # every element has to be quoted here.
  rc <- tryCatch(
    suppressWarnings(system2(cmd, shQuote(argv), stdout = FALSE, stderr = FALSE,
                             timeout = CITATION_TIMEOUT)),
    error = function(e) 1L
  )

  lines <- tryCatch(readLines(outf, warn = FALSE), error = function(e) character(0L))
  outcome <- .citation_outcome(lines, rc)
  if (outcome == "ok" && mode != "docker") outcome <- "unsandboxed"
  list(lines = lines, outcome = outcome, message = "")
}

.empty_citations_df <- function() {
  data.frame(package = character(0L), version = character(0L),
             is_current = integer(0L), payload_id = character(0L),
             source_sha256 = character(0L), status = character(0L),
             sandboxed = integer(0L), released_known = integer(0L),
             message = character(0L),
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

# No known input makes this return FALSE. jsonlite::fromJSON() only ever
# hands back strings it has already validated as UTF-8 - even an escaped
# lone surrogate such as "\uD800" decodes to a replacement character that
# validUTF8() accepts - so every string reaching here has already passed.
# Kept as a direct check on the invariant the rest of this function relies
# on, in case that stops being true under a different JSON backend or a
# jsonlite version with looser decoding, not because a reachable case is
# known today.
.cit_valid <- function(x) {
  x <- x[!is.na(x)]
  length(x) == 0L || all(validUTF8(enc2utf8(x)))
}

#' Turn one package's reader output into the three frames the DB stores.
#'
#' Every record is matched against the manifest that was asked for. A record
#' naming an id that was never requested is discarded: the container evaluates
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
  # Derived from the outcome this call was handed, never from anything in a
  # doc record: a package's own CITATION file has no way to know or claim
  # which boundary read it, and this column exists so that fact can never be
  # confused with the doc's own status. The unsandboxed path exists so local
  # work is possible at all; the condition on allowing it is that its output
  # can never be mistaken for a run that went through the container.
  sandboxed <- if (identical(outcome, "unsandboxed")) 0L else 1L

  # A whole-run failure (a reader that could not be launched, a required
  # sandbox that never started) explains every row alike; a doc that simply
  # never arrived for one version of an otherwise-successful run does not.
  # Kept as its own flag, computed once, so the two are never conflated below.
  is_run_failure <- !identical(outcome, "ok") && !identical(outcome, "unsandboxed")
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

      # JSON forbids invalid UTF-8 bytes in string content, so a corrupted
      # field never survives as a string for .cit_valid() to inspect: the
      # whole line fails jsonlite::fromJSON() instead, and that entry
      # silently disappears from `ents` above rather than erroring here.
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
              authors_json = as.character(jsonlite::toJSON(r$authors, auto_unbox = TRUE)),
              fields_json  = as.character(jsonlite::toJSON(r$fields,  auto_unbox = TRUE)),
              text_version_json = as.character(jsonlite::toJSON(r$text_version,
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
      status = status, sandboxed = sandboxed, released_known = rel_known,
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
  # three layers away. "unsandboxed" is deliberately not among them: an
  # unsandboxed outcome only widens which doc records get trusted (treated
  # the same as "ok" above), it is never itself written as a status.
  known_statuses <- c("ok", "empty", "error", "malformed",
                       "crashed", "timeout", "skipped")
  stopifnot(
    "citations$status must never be NA" = !anyNA(citations$status),
    "citations$status must be one of the known statuses" =
      all(citations$status %in% known_statuses),
    # sandboxed is not read from a record, so it cannot inherit the NA/enum
    # holes doc$status could - but it holds the same NOT NULL contract, and
    # a debug run mistaken for a production one is exactly the failure this
    # column exists to rule out, so it gets the same loud check.
    "citations$sandboxed must never be NA" = !anyNA(citations$sandboxed),
    "citations$sandboxed must be 0 or 1" =
      all(citations$sandboxed %in% c(0L, 1L))
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
    # run$message carries a whole-run explanation (e.g. "a container is
    # required to evaluate citation files") that applies to every row this
    # call produces, not to any one version's own record. Threaded through
    # rather than discarded, or a misconfigured run (wrong reader path, a
    # required sandbox that never started) is indistinguishable in the stored
    # data from every version's reader having independently crashed.
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
