# scripts/prune.R: choose which dated releases of one series to delete.

#' @param tags Character vector of all tags of ONE series (e.g. all "code-*").
#' @param keep Number of most-recent dailies to always keep (default 30), or Inf to keep all.
#' @return Character vector of tags to delete. Never deletes a first-of-month
#'   (tag ending "-01") release.
releases_to_prune <- function(tags, keep = 30L) {
  tags <- sort(unique(as.character(tags)), decreasing = TRUE)  # newest first
  if (length(tags) <= keep) return(character(0L))
  candidates <- tags[(keep + 1L):length(tags)]
  candidates[!grepl("-01$", candidates)]
}

# "all" keeps every release (Inf); anything else must be a whole number.
parse_keep <- function(x) {
  x <- tolower(trimws(x))
  if (identical(x, "all")) return(Inf)
  n <- suppressWarnings(as.integer(x))
  if (is.na(n)) stop("KEEP must be a number or \"all\", got: ", x, call. = FALSE)
  n
}

if (identical(sys.nframe(), 0L)) {
  # Reads tags on stdin (one per line), prints tags to delete.
  con <- file("stdin"); on.exit(close(con))
  tags <- readLines(con, warn = FALSE)
  tags <- tags[nzchar(tags)]
  keep <- parse_keep(Sys.getenv("KEEP", "30"))
  del <- releases_to_prune(tags, keep = keep)
  if (length(del)) cat(del, sep = "\n")
}
