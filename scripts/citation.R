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

  argv <- if (mode == "docker") {
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
      CITATION_IMAGE,
      "Rscript", "--vanilla", "/reader.R", "/stage/manifest.tsv", "/out.ndjson")
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
