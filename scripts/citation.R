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
