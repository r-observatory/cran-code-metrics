# scripts/metrics/vignettes.R: one row per vignette, from the tarball.
#
# The summary says whether a package has vignettes and how many. This says which
# ones: their names, what they are written in, and what they are called in the
# vignette index. That last one is the string a user actually browses, and it
# lives nowhere else in the published data.
#
# Everything here is read from the released source, which is the only place a
# vignette is guaranteed to be: the repository may Rbuildignore it, ship it
# pre-built, or be ahead of the release.

#' The vignette's declared title.
#'
#' Two grammars, because R has two. Sweave and any engine driven by the classic
#' markers carry %\VignetteIndexEntry{...}; knitr and Quarto sources usually
#' carry YAML front matter instead, and most carry both. The index entry wins
#' when present, because it is what R itself lists.
.vignette_title <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NA_character_)
  ie <- regmatches(txt, regexec("VignetteIndexEntry\\{([^}]*)\\}", txt))[[1]]
  if (length(ie) == 2 && nzchar(trimws(ie[2]))) return(trimws(ie[2]))

  # YAML front matter: the first title: at the top of the file, quoted or bare.
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (length(lines) && grepl("^---\\s*$", lines[1])) {
    close_at <- which(grepl("^---\\s*$", lines[-1]))[1]
    if (!is.na(close_at)) {
      head_lines <- lines[2:close_at]
      hit <- grep("^title\\s*:", head_lines, value = TRUE)
      if (length(hit)) {
        v <- sub("^title\\s*:\\s*", "", hit[1])
        v <- gsub('^["\']|["\']$', "", trimws(v))
        if (nzchar(v)) return(v)
      }
    }
  }
  NA_character_
}

#' Which engine builds this source, from its extension.
#'
#' Named for the source, not the output: .Rnw and .Rtex go through LaTeX and are
#' PDF by construction, .Rhtml is HTML by construction, but an .Rmd or .qmd
#' renders to either and the extension does not say which.
.vignette_engine <- function(path) {
  ext <- tolower(sub("^.*\\.", "", path))
  switch(ext,
         rmd = "rmarkdown", qmd = "quarto", rnw = "sweave", rtex = "sweave",
         rhtml = "html", rrst = "rst", md = "markdown", NA_character_)
}

#' One row per vignette in this version.
#'
#' Returns the typed 0-row frame when the package ships none, which is a
#' measured absence: the tarball was read and held no vignette source.
metrics_vignettes <- function(ctx) {
  files <- ctx$find(VIGNETTE_SOURCE_RE)
  if (!length(files)) return(.empty_vignettes_df())

  builder <- ctx$desc[["VignetteBuilder"]]
  builder <- if (is.null(builder) || !nzchar(trimws(builder %||% ""))) NA_character_
             else trimws(builder)

  rows <- lapply(files, function(f) {
    txt <- tryCatch(ctx$read(f), error = function(e) "")
    lines <- if (nzchar(txt %||% "")) length(strsplit(txt, "\n", fixed = TRUE)[[1]]) else 0L
    data.frame(
      file  = f,
      # The name a reader sees in the index, and the stem of the file otherwise.
      name  = sub("\\.[^.]*$", "", basename(f)),
      engine = .vignette_engine(f),
      title  = .vignette_title(txt),
      builder = builder,
      lines = as.integer(lines),
      # Whether anything in it actually runs. A vignette of prose alone is a
      # different artifact from one that executes code against the package.
      has_code = as.integer(grepl("```\\{|<<[^>]*>>=", txt %||% "")),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[order(out$file), , drop = FALSE]
}

.empty_vignettes_df <- function() {
  data.frame(file = character(), name = character(), engine = character(),
             title = character(), builder = character(), lines = integer(),
             has_code = integer(), stringsAsFactors = FALSE)
}

#' The 0-row frame in the shape the exporter writes, prefix columns included.
.empty_vignettes_rows <- function() {
  cbind(package = character(), version = character(), is_current = integer(),
        .empty_vignettes_df(), stringsAsFactors = FALSE)
}
