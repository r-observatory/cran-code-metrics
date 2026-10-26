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
#' @return TRUE on success, FALSE on any failure (404, network, etc.), with the
#'   exit status as attribute "status" (124 when GIT_TIMEOUT killed the clone).
#'   The checkout keeps LFS pointers (GIT_LFS_SKIP_SMUDGE=1), so a missing LFS
#'   object cannot fail it, and churn still reads the checked-out .gitattributes.
clone_package <- function(pkg, dest, base = CRAN_GIT_BASE, token = NULL) {
  if (!is.null(token) && nzchar(token)) {
    url <- paste0("https://x-access-token:", token,
                  "@github.com/cran/", pkg, ".git")
  } else {
    url <- paste0(base, "/", pkg, ".git")
  }
  rc <- suppressWarnings(
    system2("git", c("clone", "--quiet", url, dest),
            stdout = FALSE, stderr = FALSE, timeout = GIT_TIMEOUT,
            env = "GIT_LFS_SKIP_SMUDGE=1")
  )
  structure(identical(rc, 0L), status = as.integer(rc))
}

#' List version-like tags in a local clone, ordered by commit author date.
#'
#' Keeps tags whose name starts with a digit (e.g. "1.0", "2.0.1"), each its own
#' version. The mirror's "R-x.y.z" tags mark the package as it stood at R
#' release x.y.z, on commits of an older import of its history, so an R tag
#' takes the version its commit's subject names ("version 0.11.0") and is left
#' out when that names none. De-duplicates by commit SHA so that two tags
#' pointing at the same commit produce only one row, a plain tag before an R
#' tag, then by version, a plain tag over an R tag and otherwise the latest.
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
  # %(contents:subject) = the commit's subject, for lightweight tags
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
    "%(authordate:short)%09",
    "%(contents:subject)"
  )
  # system2 with stdout=TRUE pipes through /bin/sh; shQuote the format argument
  # so the shell does not interpret the '(' characters in %(field) as a subshell.
  raw <- suppressWarnings(
    system2("git", c("-C", repo, "for-each-ref",
                     shQuote(paste0("--format=", fmt)), "refs/tags"),
            stdout = TRUE, stderr = FALSE, timeout = GIT_TIMEOUT)
  )
  if (length(raw) == 0L || identical(raw, character(0L))) return(empty)
  if (!is.null(attr(raw, "status")) && attr(raw, "status") != 0L) return(empty)

  rows <- lapply(raw, function(line) {
    p <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    if (length(p) < 3L) return(NULL)
    tag    <- p[1L]
    sha    <- p[2L]
    adate  <- p[3L]
    # An empty subject leaves no fourth field.
    subj   <- if (length(p) >= 4L) p[4L] else ""
    list(tag = tag, commit = sha, date = adate, subject = subj)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(empty)

  tags     <- vapply(rows, `[[`, character(1L), "tag")
  commits  <- vapply(rows, `[[`, character(1L), "commit")
  dates    <- vapply(rows, `[[`, character(1L), "date")
  subjects <- vapply(rows, `[[`, character(1L), "subject")

  # A plain tag is its version. An R tag is the version its commit names, or nothing.
  named   <- "^\\s*version\\s+([0-9][^[:space:]]*)\\s*$"
  plain   <- grepl("^[0-9]", tags)
  r_named <- grepl("^R-[0-9]", tags) & grepl(named, subjects, ignore.case = TRUE, perl = TRUE)
  is_ver  <- plain | r_named
  if (!any(is_ver)) return(empty)
  versions <- ifelse(plain, tags, sub(named, "\\1", subjects, ignore.case = TRUE, perl = TRUE))[is_ver]
  tags     <- tags[is_ver]
  commits  <- commits[is_ver]
  dates    <- dates[is_ver]
  plain    <- plain[is_ver]

  # Sort by author date ascending (NAs last), a plain tag before an R tag of the same day.
  ord      <- order(dates, !plain, na.last = TRUE)
  versions <- versions[ord]
  tags     <- tags[ord]
  commits  <- commits[ord]
  dates    <- dates[ord]
  plain    <- plain[ord]

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

  v <- versions[keep]; t <- tags[keep]; d <- dates[keep]; c <- commits[keep]
  p <- plain[keep]

  # De-duplicate by version string too: an R tag can name the version of a plain
  # tag on another commit. Each version keeps one commit: its plain tag, or the
  # latest R tag when it has none (rows are ordered by date ascending).
  pick  <- vapply(split(seq_along(v), v), function(i) {
    if (any(p[i])) max(i[p[i]]) else max(i)
  }, integer(1L))
  vkeep <- seq_along(v) %in% pick

  data.frame(
    version = v[vkeep],
    ref     = t[vkeep],
    date    = d[vkeep],
    commit  = c[vkeep],
    stringsAsFactors = FALSE
  )
}

#' The condition extract_version raises when git archive or tar exits non-zero.
#'
#' @param step   "archive" or "tar".
#' @param ref    The ref being extracted.
#' @param status The exit status; 124 is system2's kill at GIT_TIMEOUT.
#' @param stderr The first line the step wrote to stderr; redacted here.
#' @return A condition of class c("extract_failure", "error", "condition").
.extract_failure <- function(step, ref, status, stderr = "") {
  status <- as.integer(status %||% NA_integer_)
  stderr <- .redact_reason(stderr)
  what   <- if (identical(step, "archive")) "git archive" else "tar"
  msg    <- sprintf("%s of %s exited %d", what, ref, status)
  if (nzchar(stderr)) msg <- paste0(msg, ": ", stderr)
  structure(
    class = c("extract_failure", "error", "condition"),
    list(message = msg, call = NULL, step = step, ref = ref,
         status = status, stderr = stderr))
}

#' Extract a version tree from a git archive into a directory.
#'
#' Runs `git archive <ref> | tar -x -C <dest>` so <dest> holds that version's
#' file tree exactly.  Creates <dest> if it does not exist.  LFS filters are
#' switched off, so an LFS-tracked file extracts as its pointer, as a checkout
#' without git-lfs holds it.
#'
#' @param repo  Path to a local git repository.
#' @param ref   Tag name, branch, or commit SHA to archive.
#' @param dest  Destination directory for the extracted tree.
#' @param paths Optional paths to extract alone; each must exist at <ref>.
#' @return Character vector of extracted file paths, relative to dest, and
#'   character(0) for a tree that is empty. A non-zero exit of git archive or
#'   tar raises an `extract_failure` (see .extract_failure).
extract_version <- function(repo, ref, dest, paths = character(0L)) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  # Write the archive to a temp file so each stage gets its own timeout.
  archive_file <- tempfile("git_archive_", fileext = ".tar")
  err_file     <- tempfile("git_archive_", fileext = ".err")
  on.exit(unlink(c(archive_file, err_file)), add = TRUE)
  first_err <- function() {
    l <- if (file.exists(err_file)) readLines(err_file, warn = FALSE) else character(0L)
    l <- l[grepl("[^[:space:]]", l, useBytes = TRUE)]
    if (length(l)) l[[1L]] else ""
  }
  rc1 <- suppressWarnings(
    system2("git", c("-c", "filter.lfs.smudge=", "-c", "filter.lfs.process=",
                     "-c", "filter.lfs.required=false",
                     "-C", repo, "archive", ref,
                     if (length(paths)) c("--", paths)),
            stdout = archive_file, stderr = err_file, timeout = GIT_TIMEOUT)
  )
  if (!identical(rc1, 0L)) stop(.extract_failure("archive", ref, rc1, first_err()))
  rc2 <- suppressWarnings(
    system2("tar", c("-x", "-C", dest),
            stdin = archive_file, stdout = FALSE, stderr = err_file,
            timeout = GIT_TIMEOUT)
  )
  if (!identical(rc2, 0L)) stop(.extract_failure("tar", ref, rc2, first_err()))
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
#' @param refs  Tags whose commits HEAD does not reach (add_transit_versions),
#'   logged with HEAD so their churn against their base is recorded too.
#' @return data.frame(commit, version, file, added, deleted).
#'   commit  - full SHA (character)
#'   version - version string parsed from subject, or NA_character_
#'   file    - repository-relative file path
#'   added   - integer lines added, NA for binary files
#'   deleted - integer lines deleted, NA for binary files
package_churn <- function(repo, refs = character(0L)) {
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
              "--diff-filter=ACMRD",
              if (length(refs)) c("HEAD", shQuote(paste0("refs/tags/", refs)))),
            stdout = TRUE, stderr = FALSE, timeout = GIT_TIMEOUT)
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
            stdout = TRUE, stderr = FALSE, timeout = GIT_TIMEOUT)
  )
  if (length(out) == 0L || identical(out, character(0L))) return("")
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) return("")
  paste(out, collapse = "\n")
}

# ---------------------------------------------------------------------------
# CRAN Transit versions
# ---------------------------------------------------------------------------

.TRANSIT_COLS <- c("package", "version", "base_version", "released", "url", "sha256")

#' The CRAN Transit versions listed in `path`, one row each.
#'
#' CRAN served these from src/contrib/Transit and deleted each when it was
#' replaced, so the mirror never tagged them and CRAN's archive does not hold
#' them. Each row names the mirror tag it follows, when CRAN listed it, and a
#' copy pinned by its SHA-256.
#'
#' @param path The listing, a CSV file with the columns of .TRANSIT_COLS.
#' @return data.frame with .TRANSIT_COLS, all character. A missing column, a
#'   malformed checksum or date stops the run rather than drop a version.
read_transit_versions <- function(path = TRANSIT_VERSIONS_FILE) {
  df <- utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
                        strip.white = TRUE)
  missing <- setdiff(.TRANSIT_COLS, names(df))
  if (length(missing)) {
    stop(sprintf("%s lacks the column(s) %s", path, paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  df <- df[.TRANSIT_COLS]
  bad <- !grepl("^[0-9a-f]{64}$", df$sha256) |
    is.na(as.POSIXct(df$released, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (any(bad)) {
    stop(sprintf("%s: malformed checksum or date for %s", path,
                 paste(df$package[bad], df$version[bad], collapse = ", ")), call. = FALSE)
  }
  rownames(df) <- NULL
  df
}

# The condition a Transit version that cannot be added raises, naming it.
.transit_failure <- function(row, what) {
  structure(
    class = c("transit_failure", "error", "condition"),
    list(message = sprintf("CRAN Transit version %s %s: %s", row$package, row$version, what),
         call = NULL))
}

#' Download a Transit version's copy, with linear backoff.
#' @return TRUE only when a non-empty file lands at destfile.
.fetch_transit_tarball <- function(url, destfile, tries = 3L, sleep = 2) {
  for (attempt in seq_len(tries)) {
    ok <- .retry_after_time_limit({
      suppressWarnings(utils::download.file(url, destfile, mode = "wb", quiet = TRUE))
      file.exists(destfile) && file.info(destfile)$size > 0
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
    if (attempt < tries) Sys.sleep(sleep * attempt)
  }
  FALSE
}

# git in `repo`, its stdout as lines; NULL when it exits non-zero.
.git_lines <- function(repo, args, ...) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(repo), args), stdout = TRUE,
                                  stderr = FALSE, timeout = GIT_TIMEOUT, ...))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) return(NULL)
  out
}

#' The tree of a source tarball's <package>/ directory, written into `repo`.
#'
#' Every file goes in as its bytes, with no filter, so a .gitignore or
#' .gitattributes inside the tarball drops or rewrites nothing.
.transit_tree <- function(repo, row, tarball, work) {
  ex <- file.path(work, "x")
  dir.create(ex)
  rc <- suppressWarnings(system2("tar", c("-xzf", shQuote(tarball), "-C", shQuote(ex)),
                                 stdout = FALSE, stderr = FALSE, timeout = GIT_TIMEOUT))
  if (!identical(rc, 0L)) stop(.transit_failure(row, sprintf("tar exited %d", rc)))
  if (!identical(list.files(ex, all.files = TRUE, no.. = TRUE), row$package)) {
    stop(.transit_failure(row, sprintf("its tarball does not hold %s/ alone", row$package)))
  }
  top   <- file.path(ex, row$package)
  paths <- list.files(top, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  full  <- file.path(top, paths)
  link  <- Sys.readlink(full)
  is_link <- nzchar(link)
  # A link's blob is its target, written to a file of its own to be hashed.
  src <- full
  for (i in which(is_link)) {
    src[[i]] <- file.path(work, sprintf("link-%d", i))
    writeBin(charToRaw(link[[i]]), src[[i]])
  }
  mode <- ifelse(is_link, "120000",
                 ifelse(bitwAnd(as.integer(file.info(full)$mode), 64L) > 0L, "100755", "100644"))
  list_file <- file.path(work, "paths")
  writeLines(src, list_file)
  blobs <- if (length(src)) {
    .git_lines(repo, c("hash-object", "-w", "--no-filters", "--stdin-paths"), stdin = list_file)
  } else character(0L)
  if (length(blobs) != length(src)) stop(.transit_failure(row, "git hash-object failed"))
  info <- file.path(work, "index-info")
  writeBin(unlist(lapply(sprintf("%s %s\t%s", mode, blobs, paths), function(s)
    c(charToRaw(enc2utf8(s)), as.raw(0L)))) %||% raw(0L), info)
  env <- paste0("GIT_INDEX_FILE=", shQuote(file.path(work, "index")))
  if (is.null(.git_lines(repo, c("update-index", "-z", "--index-info"), stdin = info, env = env))) {
    stop(.transit_failure(row, "git update-index failed"))
  }
  tree <- .git_lines(repo, "write-tree", env = env)
  if (length(tree) != 1L) stop(.transit_failure(row, "git write-tree failed"))
  tree
}

.add_transit_version <- function(repo, row, fetch) {
  ver <- row$version
  # The mirror tagged it after all: its own commit stands.
  if (!is.null(.git_lines(repo, c("rev-parse", "--verify", "-q", shQuote(paste0("refs/tags/", ver)))))) {
    return(ver)
  }
  base <- .git_lines(repo, c("rev-parse", "--verify", "-q",
                             shQuote(paste0("refs/tags/", row$base_version, "^{commit}"))))
  if (length(base) != 1L) {
    stop(.transit_failure(row, sprintf("its base %s has no tag in the clone", row$base_version)))
  }
  work <- tempfile("ccm_transit_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  tarball <- file.path(work, "source.tar.gz")
  if (!isTRUE(fetch(row$url, tarball)) || !file.exists(tarball)) {
    stop(.transit_failure(row, sprintf("its tarball could not be downloaded from %s", row$url)))
  }
  got <- digest::digest(file = tarball, algo = "sha256")
  if (!identical(got, row$sha256)) {
    stop(.transit_failure(row, sprintf("its tarball's checksum is %s, not the listed %s",
                                       got, row$sha256)))
  }
  tree <- .transit_tree(repo, row, tarball, work)
  # Fixed names and dates, so the same copy gives the same commit on every run.
  who <- c("cran-code-metrics", "cran-code-metrics@users.noreply.github.com")
  env <- c(paste0("GIT_AUTHOR_NAME=", who[[1L]]), paste0("GIT_AUTHOR_EMAIL=", who[[2L]]),
           paste0("GIT_COMMITTER_NAME=", who[[1L]]), paste0("GIT_COMMITTER_EMAIL=", who[[2L]]),
           paste0("GIT_AUTHOR_DATE=", row$released), paste0("GIT_COMMITTER_DATE=", row$released))
  commit <- .git_lines(repo, c("commit-tree", "--no-gpg-sign", tree, "-p", base,
                               "-m", shQuote(paste("version", ver))), env = env)
  if (length(commit) != 1L ||
      is.null(.git_lines(repo, c("update-ref", shQuote(paste0("refs/tags/", ver)), commit)))) {
    stop(.transit_failure(row, "its commit could not be written"))
  }
  ver
}

#' Add each Transit version in `rows` to a clone, as a commit of its copy whose
#' parent is its base tag, tagged with its version, so list_versions walks it
#' between its base and the next release.
#'
#' @param repo  A clone of the package's mirror.
#' @param rows  The package's rows of read_transit_versions(), or NULL.
#' @param fetch function(url, destfile) -> TRUE when the copy landed.
#' @return The tags, one per row. A version that cannot be added raises a
#'   transit_failure, so its package fails rather than walk without it: a whole
#'   write would delete its stored row. A cap that fires goes on as itself.
add_transit_versions <- function(repo, rows, fetch = .fetch_transit_tarball) {
  if (is.null(rows) || nrow(rows) == 0L) return(character(0L))
  vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    tryCatch(.add_transit_version(repo, row, fetch),
             transit_failure = function(e) stop(e),
             error = function(e) {
               if (.is_time_limit(e)) stop(e)
               stop(.transit_failure(row, conditionMessage(e)))
             })
  }, character(1L))
}
