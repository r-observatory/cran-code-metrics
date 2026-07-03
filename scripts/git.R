# scripts/git.R: git plumbing wrappers for the cran-code-metrics pipeline.
#
# All functions communicate with git via system2("git", ...).
# clone_package() never throws on failure; it returns FALSE instead.
# Dependency: config.R must be sourced first (CRAN_GIT_BASE, %||%).

#' Clone a CRAN mirror repo from github.com/cran.
#'
#' @param pkg   Package name (exact case as it appears on CRAN).
#' @param dest  Local path for the clone (passed as <directory> to git clone).
#' @param base  Base URL; defaults to CRAN_GIT_BASE.
#' @param token Optional GitHub personal access token. When supplied the clone
#'   URL becomes https://x-access-token:<token>@github.com/cran/<pkg>.git so
#'   the request authenticates without a credential helper or .netrc.
#' @return TRUE on success, FALSE on any failure (404, network, etc.).
clone_package <- function(pkg, dest, base = CRAN_GIT_BASE, token = NULL) {
  if (!is.null(token) && nzchar(token)) {
    url <- paste0("https://x-access-token:", token,
                  "@github.com/cran/", pkg, ".git")
  } else {
    url <- paste0(base, "/", pkg, ".git")
  }
  rc <- suppressWarnings(
    system2("git", c("clone", "--quiet", url, dest),
            stdout = FALSE, stderr = FALSE)
  )
  identical(rc, 0L)
}

#' List version-like tags in a local clone, ordered by commit author date.
#'
#' Keeps tags whose name starts with a digit (e.g. "1.0", "2.0.1") or that
#' match the legacy pattern "R-<digit>..." (the "R-" prefix is stripped to
#' produce the version string).  De-duplicates by commit SHA so that two tags
#' pointing at the same commit produce only one row (the one whose tag comes
#' first in date order).
#'
#' @param repo  Path to a local git repository directory.
#' @return data.frame(version, ref, date, commit) ordered by date ascending.
#'   All columns are character.  Returns a zero-row frame when no tags match.
#' @note Assumes the repository uses lightweight tags (as CRAN mirror repos do).
#'   For lightweight tags, %(objectname) in for-each-ref output is the commit
#'   SHA directly.  Annotated tags would require %(*objectname) to dereference,
#'   which is not handled here.
list_versions <- function(repo) {
  empty <- data.frame(
    version = character(0L), ref    = character(0L),
    date    = character(0L), commit = character(0L),
    stringsAsFactors = FALSE
  )

  # %(objectname)       = SHA of the tag object (commit SHA for lightweight tags)
  # %(authordate:short) = author/tagger date YYYY-MM-DD
  #
  # Note: the %(*objectname) form (dereference annotated tags) contains a
  # literal '(' which the shell misinterprets when system2 pipes stdout through
  # /bin/sh.  CRAN mirror repos use lightweight tags exclusively, so we use the
  # plain objectname field and obtain the commit SHA via git rev-parse afterwards
  # only when needed.  For lightweight tags %(objectname) IS the commit SHA.
  # Use %09 (not %x09): Apple git 2.50+ does not expand the %xNN hex escape
  # form in for-each-ref --format, but does expand the decimal %09 form.
  fmt <- paste0(
    "%(refname:short)%09",
    "%(objectname)%09",
    "%(authordate:short)"
  )
  # system2 with stdout=TRUE pipes through /bin/sh; shQuote the format argument
  # so the shell does not interpret the '(' characters in %(field) as a subshell.
  raw <- suppressWarnings(
    system2("git", c("-C", repo, "for-each-ref",
                     shQuote(paste0("--format=", fmt)), "refs/tags"),
            stdout = TRUE, stderr = FALSE)
  )
  if (length(raw) == 0L || identical(raw, character(0L))) return(empty)

  rows <- lapply(raw, function(line) {
    p <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    if (length(p) < 3L) return(NULL)
    tag    <- p[1L]
    sha    <- p[2L]
    adate  <- p[3L]
    list(tag = tag, commit = sha, date = adate)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(empty)

  tags    <- vapply(rows, `[[`, character(1L), "tag")
  commits <- vapply(rows, `[[`, character(1L), "commit")
  dates   <- vapply(rows, `[[`, character(1L), "date")

  # Keep only version-like tags: starts with digit, or "R-" + digit
  is_ver <- grepl("^[0-9]", tags) | grepl("^R-[0-9]", tags)
  tags    <- tags[is_ver]
  commits <- commits[is_ver]
  dates   <- dates[is_ver]
  if (length(tags) == 0L) return(empty)

  versions <- sub("^R-", "", tags)

  # Sort by author date ascending (NAs last)
  ord     <- order(dates, na.last = TRUE)
  versions <- versions[ord]
  tags     <- tags[ord]
  commits  <- commits[ord]
  dates    <- dates[ord]

  # De-duplicate by commit SHA: keep first (oldest) occurrence.
  # Use a hashed environment for O(1) membership tests instead of O(n) %in%.
  seen <- new.env(hash = TRUE, parent = emptyenv())
  keep <- logical(length(commits))
  for (i in seq_along(commits)) {
    if (!exists(commits[i], envir = seen, inherits = FALSE)) {
      keep[i] <- TRUE
      assign(commits[i], TRUE, envir = seen)
    }
  }

  data.frame(
    version = versions[keep],
    ref     = tags[keep],
    date    = dates[keep],
    commit  = commits[keep],
    stringsAsFactors = FALSE
  )
}

#' Extract a version tree from a git archive into a directory.
#'
#' Runs `git archive <ref> | tar -x -C <dest>` so <dest> holds that version's
#' file tree exactly.  Creates <dest> if it does not exist.
#'
#' @param repo  Path to a local git repository.
#' @param ref   Tag name, branch, or commit SHA to archive.
#' @param dest  Destination directory for the extracted tree.
#' @return Character vector of extracted file paths, relative to dest.
#'   Returns character(0) when the archive or extraction fails.
extract_version <- function(repo, ref, dest) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  cmd <- paste0(
    "git -C ", shQuote(repo), " archive ", shQuote(ref),
    " | tar -x -C ", shQuote(dest)
  )
  rc <- system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (!identical(rc, 0L)) return(character(0L))
  list.files(dest, recursive = TRUE, all.files = TRUE,
             include.dirs = FALSE, no.. = TRUE)
}

# Resolve a git numstat path that contains a rename arrow ( => ).
# git emits renames as either:
#   brace form:  "pre/{old => new}/post"
#   plain form:  "old/path => new/path"
# Returns the new (right-hand) path in both cases.
.resolve_rename_path <- function(path) {
  if (!grepl(" => ", path, fixed = TRUE)) return(path)
  # Brace form: pre/{a => b}/post
  m <- regmatches(path, regexec("^(.*?)\\{([^}]*) => ([^}]*)\\}(.*)$", path))[[1L]]
  if (length(m) == 5L) {
    pre    <- m[2L]
    b      <- m[4L]   # new name (right-hand side)
    post   <- m[5L]
    result <- paste0(pre, b, post)
    # Collapse double slashes that arise when b is empty
    result <- gsub("//+", "/", result, perl = TRUE)
    # Strip a leading or trailing slash left by empty b
    result <- sub("^/", "", result)
    result <- sub("/$", "", result)
    return(result)
  }
  # Plain form: "old => new" -- take the right-hand side
  trimws(sub("^.* => ", "", path))
}

#' Compute per-file, per-commit churn from the full git history of a clone.
#'
#' Parses `git log --numstat --format=...` output in a single pass.
#' The commit subject is expected to be "version X.Y.Z" (as used by the CRAN
#' mirror); a version string is extracted from it via regex or left NA.
#' Binary files for which numstat reports "-" are recorded with NA added/deleted.
#'
#' @param repo  Path to a local git repository.
#' @return data.frame(commit, version, file, added, deleted).
#'   commit  - full SHA (character)
#'   version - version string parsed from subject, or NA_character_
#'   file    - repository-relative file path
#'   added   - integer lines added, NA for binary files
#'   deleted - integer lines deleted, NA for binary files
package_churn <- function(repo) {
  empty <- data.frame(
    commit  = character(0L), version = character(0L),
    file    = character(0L), added   = integer(0L),  deleted = integer(0L),
    stringsAsFactors = FALSE
  )

  # Separator: __C__<full_SHA>\t<authordate ISO>\t<subject>
  # git log --format uses %x09 (hex escape) for a literal tab.
  # This is the OPPOSITE of for-each-ref, which requires %09 (decimal):
  # Apple git 2.50+ does not expand %xNN in for-each-ref format strings
  # but DOES expand them in git log pretty-format strings.
  raw <- suppressWarnings(
    system2("git",
            c("-C", repo, "log", "--numstat",
              "--format=__C__%H%x09%ai%x09%s",
              "--diff-filter=ACMRD"),
            stdout = TRUE, stderr = FALSE)
  )
  if (length(raw) == 0L) return(empty)

  results         <- vector("list", length(raw))
  n_results       <- 0L
  cur_commit      <- NA_character_
  cur_version     <- NA_character_

  for (line in raw) {
    if (startsWith(line, "__C__")) {
      rest    <- substring(line, 6L)
      parts   <- strsplit(rest, "\t", fixed = TRUE)[[1L]]
      cur_commit  <- if (length(parts) >= 1L) parts[1L] else NA_character_
      subject     <- if (length(parts) >= 3L) parts[3L] else ""
      # Extract version string from "version X.Y.Z" in the subject.
      # Separator class includes dash so "0.20-45" and "3.5-7" parse fully.
      m <- regmatches(subject,
                      regexpr("[0-9]+[._-][0-9]+([._-][0-9]+)*", subject,
                              perl = TRUE))
      cur_version <- if (length(m) == 1L) m else NA_character_
    } else if (nzchar(trimws(line))) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
      if (length(parts) < 3L) next
      add_s <- parts[1L]
      del_s <- parts[2L]
      fpath <- .resolve_rename_path(parts[3L])
      # Binary files: numstat shows "-" for both counts
      added   <- if (add_s == "-") NA_integer_ else suppressWarnings(as.integer(add_s))
      deleted <- if (del_s == "-") NA_integer_ else suppressWarnings(as.integer(del_s))
      n_results <- n_results + 1L
      results[[n_results]] <- list(
        commit  = cur_commit,
        version = cur_version,
        file    = fpath,
        added   = added,
        deleted = deleted
      )
    }
  }

  if (n_results == 0L) return(empty)
  results <- results[seq_len(n_results)]

  data.frame(
    commit  = vapply(results, `[[`, character(1L), "commit"),
    version = vapply(results, function(x) {
      v <- x[["version"]]
      if (is.null(v) || (length(v) == 1L && is.na(v))) NA_character_ else v
    }, character(1L)),
    file    = vapply(results, `[[`, character(1L), "file"),
    added   = vapply(results, `[[`, integer(1L),   "added"),
    deleted = vapply(results, `[[`, integer(1L),   "deleted"),
    stringsAsFactors = FALSE
  )
}

#' Read the content of a file at a specific git ref.
#'
#' @param repo  Path to a local git repository.
#' @param ref   Tag, branch, or commit SHA.
#' @param path  Repository-relative file path (e.g. "DESCRIPTION").
#' @return Content as a single character string.  Returns "" when the path does
#'   not exist at the given ref (git exits non-zero; no error is thrown).
read_at <- function(repo, ref, path) {
  spec <- paste0(ref, ":", path)
  out  <- suppressWarnings(
    system2("git", c("-C", repo, "show", spec),
            stdout = TRUE, stderr = FALSE)
  )
  if (length(out) == 0L || identical(out, character(0L))) return("")
  paste(out, collapse = "\n")
}
