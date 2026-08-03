# scripts/metrics/vignettes.R: one row per vignette, from the tarball.
#
# The summary says how many vignettes a package has. This says which ones: their
# names, what they are written in, what they render to, who wrote them, and what
# they are called in the vignette index.
#
# Read from the released source, which is the only place a vignette is
# guaranteed to be: the repository may Rbuildignore it, ship it pre-built, or be
# ahead of the release.

#' The vignette's declared title.
#'
#' Two grammars, because R has two. %\VignetteIndexEntry{...} is what R itself
#' lists, and YAML front matter carries title: as well; most sources have both,
#' and the index entry wins because that is the string a reader browses.
.vignette_title <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NA_character_)
  ie <- regmatches(txt, regexec("VignetteIndexEntry\\{([^}]*)\\}", txt))[[1]]
  if (length(ie) == 2 && nzchar(trimws(ie[2]))) return(trimws(ie[2]))
  .yaml_scalar(txt, "title")
}

#' A scalar field from YAML front matter, or NA.
#'
#' Deliberately only the simple `key: value` form. An author block written as a
#' list, or with name/affiliation sub-keys, returns NA rather than a fragment of
#' itself: half a name is worse than no name.
.yaml_scalar <- function(txt, key) {
  if (is.null(txt) || !nzchar(txt)) return(NA_character_)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (!length(lines) || !grepl("^---\\s*$", lines[1])) return(NA_character_)
  close_at <- which(grepl("^---\\s*$", lines[-1]))[1]
  if (is.na(close_at)) return(NA_character_)
  head_lines <- lines[2:close_at]
  hit <- grep(sprintf("^%s\\s*:", key), head_lines, value = TRUE)
  if (!length(hit)) return(NA_character_)
  v <- trimws(sub(sprintf("^%s\\s*:\\s*", key), "", hit[1]))
  v <- gsub('^["\']|["\']$', "", v)
  if (!nzchar(v)) return(NA_character_)   # a list or block follows, not a scalar
  v
}

#' The raw lines of a YAML front-matter key, including any block that follows.
#'
#' Returns character(0) when the key is absent, which is what lets a caller tell
#' "the vignette says nothing" from "the vignette says something in a shape this
#' does not parse". Those were the same answer until roughly a fifth of the
#' authored vignettes on GitHub turned out to use a block.
.yaml_block <- function(txt, key) {
  if (is.null(txt) || !nzchar(txt)) return(character(0))
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (!length(lines) || !grepl("^---\\s*$", lines[1])) return(character(0))
  close_at <- which(grepl("^---\\s*$", lines[-1]))[1]
  if (is.na(close_at)) return(character(0))
  head_lines <- lines[2:close_at]
  at <- grep(sprintf("^%s\\s*:", key), head_lines)[1]
  if (is.na(at)) return(character(0))
  out <- head_lines[at]
  # Continuation lines are the indented ones that follow, up to the next
  # top-level key.
  j <- at + 1L
  while (j <= length(head_lines) && grepl("^\\s+\\S", head_lines[j])) {
    out <- c(out, head_lines[j]); j <- j + 1L
  }
  out
}

#' Everyone the vignette says wrote it.
#'
#' Four shapes, because YAML offers three and Sweave adds one, and all four
#' occur: a scalar, a flow sequence, a block of plain items, and a block of
#' mappings with name: keys. A scalar-only reader returned NA for the last two,
#' which put roughly a fifth of the authored vignettes on GitHub into the same
#' bucket as the vignettes that name nobody.
#'
#' Returns character(0) when the vignette states no author at all.
.vignette_authors <- function(txt) {
  blk <- .yaml_block(txt, "author")
  if (length(blk)) {
    first <- trimws(sub("^author\\s*:\\s*", "", blk[1]))
    # Flow sequence on one line: author: [Ada, Bob]
    if (grepl("^\\[.*\\]$", first)) {
      items <- strsplit(gsub("^\\[|\\]$", "", first), ",", fixed = FALSE)[[1]]
      return(.clean_names(items))
    }
    if (nzchar(first)) return(.clean_names(first))   # plain scalar
    rest <- blk[-1]
    if (length(rest)) {
      # A block of mappings carries name: keys; a block of plain items does not.
      named <- grep("^\\s*-?\\s*name\\s*:", rest, value = TRUE)
      if (length(named)) return(.clean_names(sub("^\\s*-?\\s*name\\s*:\\s*", "", named)))
      items <- grep("^\\s*-\\s*\\S", rest, value = TRUE)
      if (length(items)) return(.clean_names(sub("^\\s*-\\s*", "", items)))
    }
    # Stated, but in a shape this does not read. NOT the same as unstated, and
    # the caller records the difference.
    return(NA_character_)
  }
  # Sweave: \author{Ada \and Bob} or \author{Ada, Bob}
  m <- regmatches(txt, regexec("\\\\author\\{([^}]*)\\}", txt %||% ""))[[1]]
  if (length(m) == 2 && nzchar(trimws(m[2]))) {
    parts <- strsplit(m[2], "\\\\and|,", perl = TRUE)[[1]]
    return(.clean_names(parts))
  }
  character(0)
}

#' Trim, unquote, drop LaTeX markup and empties.
.clean_names <- function(x) {
  x <- gsub("\\\\[a-zA-Z]+\\{|\\}", "", x)      # \texttt{...} and friends
  x <- gsub('^["\']|["\']$', "", trimws(x))
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)
}

#' The source format, from the extension.
.vignette_format <- function(path) {
  low <- tolower(path)
  if (grepl("\\.asis$", low)) return("asis")
  ext <- sub("^.*\\.", "", low)
  switch(ext,
         rmd = "rmarkdown", qmd = "quarto", rnw = "sweave", rtex = "sweave",
         rhtml = "html", rrst = "rst", md = "markdown", NA_character_)
}

#' What the vignette renders to: "pdf", "html", or NA when nothing says.
#'
#' The extension alone cannot answer this for .Rmd and .qmd, which was the
#' honest limit stated when only the extension was read. The sources usually do
#' say, in one of three places, so the answer is available far more often than
#' the extension suggests: the declared %\VignetteEngine, the .asis filename
#' (json-mapping.pdf.asis is a PDF), and the YAML output/format field.
.vignette_output <- function(path, txt, engine) {
  low <- tolower(path)
  if (grepl("\\.pdf\\.asis$", low))  return("pdf")
  if (grepl("\\.html\\.asis$", low)) return("html")
  if (!is.na(engine)) {
    e <- tolower(engine)
    if (grepl("pdf|latex|tex", e))   return("pdf")
    if (grepl("html", e))            return("html")
    # Sweave and Rnw go through LaTeX and are PDF by construction.
    if (grepl("sweave", e))          return("pdf")
  }
  if (identical(.vignette_format(path), "sweave")) return("pdf")
  out <- .yaml_scalar(txt, "output")
  if (!is.na(out)) {
    o <- tolower(out)
    if (grepl("pdf", o))  return("pdf")
    if (grepl("html", o)) return("html")
  }
  # Quarto states it under format:, which may be a scalar or a block.
  if (grepl("(?m)^format:\\s*$", txt %||% "", perl = TRUE)) {
    blk <- sub("(?s).*?^format:\\s*$", "", txt, perl = TRUE)
    blk <- substr(blk, 1, 200)
    if (grepl("pdf", blk))  return("pdf")
    if (grepl("html", blk)) return("html")
  }
  fmt <- .yaml_scalar(txt, "format")
  if (!is.na(fmt)) {
    f <- tolower(fmt)
    if (grepl("pdf", f))  return("pdf")
    if (grepl("html", f)) return("html")
  }
  NA_character_
}

#' The engine the source declares, verbatim, or NA.
.vignette_engine_declared <- function(txt) {
  m <- regmatches(txt, regexec("VignetteEngine\\{([^}]*)\\}", txt %||% ""))[[1]]
  if (length(m) == 2 && nzchar(trimws(m[2]))) return(trimws(m[2]))
  NA_character_
}

#' One row per vignette in this version.
#'
#' Returns the typed 0-row frame when the package ships none, which is a
#' measured absence: the tarball was read and held no vignette source.
metrics_vignettes <- function(ctx) {
  files <- ctx$find(VIGNETTE_SOURCE_RE)
  # .asis stubs are vignettes too: R.rsp ships a finished document beside them,
  # and VIGNETTE_SOURCE_RE does not match a stub because it has no R extension.
  files <- unique(c(files, ctx$find("^vignettes/.*\\.asis$")))
  if (!length(files)) return(.empty_vignettes_df())

  all_files <- ctx$find("^vignettes/")
  builder <- ctx$desc[["VignetteBuilder"]]
  builder <- if (is.null(builder) || !nzchar(trimws(builder %||% ""))) NA_character_
             else trimws(builder)

  rows <- lapply(files, function(f) {
    txt <- tryCatch(ctx$read(f), error = function(e) "")
    txt <- txt %||% ""
    auth <- .vignette_authors(txt)
    engine <- .vignette_engine_declared(txt)
    lines  <- if (nzchar(txt)) length(strsplit(txt, "\n", fixed = TRUE)[[1]]) else 0L
    data.frame(
      file   = f,
      name   = sub("\\.[^.]*$", "", basename(f)),
      format = .vignette_format(f),
      engine = engine,
      output = .vignette_output(f, txt, engine),
      title  = .vignette_title(txt),
      # Comma-joined, the way markers and evidence tiers already are here, plus
      # a count so "written by four people" is answerable without splitting.
      author = if (length(auth) == 0L || (length(auth) == 1L && is.na(auth[1]))) NA_character_
               else paste(auth, collapse = ", "),
      n_authors = if (length(auth) == 0L || (length(auth) == 1L && is.na(auth[1]))) NA_integer_
                  else length(auth),
      # Whether the vignette said anything at all about authorship. Without
      # this, a vignette naming nobody and a vignette naming people in a shape
      # this cannot read are the same row, which is the state that hid how
      # common the block form is.
      author_stated = as.integer(length(auth) > 0L),
      builder = builder,
      # Shipped finished rather than built at check time. R.rsp::asis takes a
      # document that is already a PDF or HTML file and installs it as the
      # vignette, so nothing in it runs and its "source" is a few marker lines.
      prebuilt = as.integer(!is.na(engine) && grepl("asis", tolower(engine))),
      # The precompute pattern: the .Rmd shipped is generated from an .Rmd.orig
      # kept beside it, so the expensive code ran before submission rather than
      # on CRAN's machines.
      precomputed = as.integer(paste0(f, ".orig") %in% all_files),
      lines = as.integer(lines),
      has_code = as.integer(grepl("```\\{|<<[^>]*>>=", txt)),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[order(out$file), , drop = FALSE]
}

.empty_vignettes_df <- function() {
  data.frame(file = character(), name = character(), format = character(),
             engine = character(), output = character(), title = character(),
             author = character(), n_authors = integer(), author_stated = integer(),
             builder = character(), prebuilt = integer(),
             precomputed = integer(), lines = integer(), has_code = integer(),
             stringsAsFactors = FALSE)
}

#' The 0-row frame in the shape the exporter writes, prefix columns included.
.empty_vignettes_rows <- function() {
  cbind(package = character(), version = character(), is_current = integer(),
        .empty_vignettes_df(), stringsAsFactors = FALSE)
}
