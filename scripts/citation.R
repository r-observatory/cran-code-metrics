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

.cit_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)[[1L]]

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
parse_citation_records <- function(lines, inputs_df, outcome) {
  if (is.null(inputs_df) || nrow(inputs_df) == 0L) {
    return(list(citations = .empty_citations_df(),
                payloads  = .empty_citation_payloads_df(),
                entries   = .empty_citation_entries_df()))
  }
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  want <- basename(inputs_df$dir)

  parsed <- list()
  for (l in lines) {
    if (!nzchar(l) || identical(l, "{\"t\":\"end\"}")) next
    r <- tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE),
                  error = function(e) NULL)
    if (is.null(r) || is.null(r$id) || !(r$id %in% want)) next
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

    if (!identical(outcome, "ok") && !identical(outcome, "unsandboxed")) {
      status <- outcome; doc <- NULL
    } else if (is.null(doc)) {
      status <- "crashed"
    } else {
      status <- .cit_chr(doc$status)
    }

    payload_id <- NA_character_
    if (!is.null(doc) && status %in% c("ok", "empty")) {
      mine <- Filter(function(r) identical(r$id, id), ents)
      mine <- mine[order(vapply(mine, function(r) as.integer(r$i), integer(1)))]

      cols <- function(r) c(.cit_chr(r$bibtype), .cit_chr(r$title), .cit_chr(r$year),
                            .cit_chr(r$header), .cit_chr(r$footer), .cit_chr(r$key),
                            .cit_chr(r$bibtex), .cit_chr(r$citation))
      texts <- c(.cit_chr(doc$mheader), .cit_chr(doc$mfooter),
                 unlist(lapply(mine, cols), use.names = FALSE))

      # JSON forbids invalid UTF-8 bytes in string content, so a corrupted
      # field never survives as a string for .cit_valid() below to inspect:
      # the whole line fails jsonlite::fromJSON() instead, and that entry
      # silently disappears from `ents` above rather than erroring here. A
      # doc that promised n_entries entries but yielded fewer is the only
      # remaining signal that something was dropped, and that mismatch is
      # treated as corruption rather than as a document with fewer entries
      # than it claims.
      declared_n <- suppressWarnings(as.integer(.cit_chr(doc$n_entries)))
      if (!is.na(declared_n) && length(mine) != declared_n) {
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
      status = status, released_known = rel_known,
      message = if (is.null(doc)) NA_character_ else .cit_chr(doc$message),
      evaluated_at = now, stringsAsFactors = FALSE)
  }

  list(
    citations = do.call(rbind, cit_rows) %||% .empty_citations_df(),
    payloads  = if (length(pay_rows)) do.call(rbind, unname(pay_rows))
                else .empty_citation_payloads_df(),
    entries   = if (length(ent_rows)) do.call(rbind, unname(ent_rows))
                else .empty_citation_entries_df()
  )
}
