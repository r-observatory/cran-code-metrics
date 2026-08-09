# scripts/cite_reader.R: evaluates inst/CITATION and writes NDJSON.
#
# Runs inside the sandbox container, never in the pipeline process. Base R only,
# so a stock R image needs no package installed alongside untrusted code.
#
# Invoked as:  Rscript --vanilla scripts/cite_reader.R <manifest.tsv> <out.ndjson>
# Manifest:    one job per line, tab separated: id, dir, released (YYYY-MM-DD|NA)

# ---- JSON, hand-rolled so the container needs no packages ------------------

.jesc <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\b", "\\b", x, fixed = TRUE)
  x <- gsub("\f", "\\f", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  # Remaining control characters have no short escape.
  gsub("([\x01-\x1f])", "", x, perl = TRUE)
}

.jstr <- function(x) {
  if (is.null(x) || length(x) == 0L) return("null")
  x <- as.character(x)[[1L]]
  if (is.na(x)) return("null")
  x <- enc2utf8(x)
  if (!validUTF8(x)) x <- iconv(x, "UTF-8", "UTF-8", sub = "�")
  paste0("\"", .jesc(x), "\"")
}

.jarr <- function(x) {
  if (is.null(x) || length(x) == 0L) return("[]")
  paste0("[", paste(vapply(as.character(x), .jstr, character(1)), collapse = ","), "]")
}

.jobj <- function(pairs) {
  if (!length(pairs)) return("{}")
  paste0("{", paste(sprintf("%s:%s", vapply(names(pairs), .jstr, character(1)),
                            unlist(pairs, use.names = FALSE)),
                    collapse = ","), "}")
}

.jnum <- function(x) if (is.null(x) || is.na(x)) "null" else format(as.integer(x))

# ---- Faithful reimplementation of tools:::.parse_CITATION_file -------------
# Reproduced rather than called because it is namespace-internal, and because the
# ASCII hard error is a real behaviour we must report rather than work around.

.parse_citation <- function(cfile, encoding = NULL) {
  if (is.null(encoding) || !nzchar(encoding)) encoding <- "ASCII"
  if (encoding %in% c("C", "ASCII")) {
    x   <- readLines(cfile, warn = FALSE)
    asc <- iconv(x, "latin1", "ASCII")
    if (any(is.na(asc) | asc != x)) {
      stop("non-ASCII input in a CITATION file without a declared encoding")
    }
    parse(file = cfile)
  } else {
    con <- file(cfile, encoding = encoding)
    on.exit(close(con), add = TRUE)
    parse(con)
  }
}

# ---- Evaluation ------------------------------------------------------------

#' Evaluate one CITATION file the way readCitationFile does, with a controlled
#' parent environment.
#'
#' utils::readCitationFile builds its evaluation environment with
#' new.env(hash = TRUE), whose parent is the calling frame. That is not
#' something a caller can choose, and choosing it is the whole point here:
#' Sys.Date() must resolve to the version's release date so that a 2011 release
#' does not acquire this year's citation year, and so that a rebuild reproduces
#' the same rows.
#'
#' Masking is only for determinism. It is not a security boundary and is not
#' relied on as one: eval(quote(f()), baseenv()) walks straight past it. The
#' boundary is the container and what was staged into it.
.eval_citation <- function(cfile, meta, released) {
  clock <- new.env(parent = globalenv())
  d <- as.Date(released)
  # The release date is only required of files that actually consult the
  # clock. Checking it unconditionally for every job would fail files that
  # never call Sys.Date()/Sys.time()/date() even when the release date is
  # unknown, so the check is deferred to the point of use.
  need_date <- function() {
    if (is.na(d)) stop("no known release date, so a citation year cannot be resolved")
  }
  assign("Sys.Date", function() { need_date(); d }, envir = clock)
  assign("Sys.time", function() { need_date(); as.POSIXct(paste(released, "12:00:00"), tz = "UTC") },
         envir = clock)
  assign("date", function() { need_date(); format(as.POSIXct(paste(released, "12:00:00"), tz = "UTC"),
                                   "%a %b %d %H:%M:%S %Y") }, envir = clock)

  exprs <- .parse_citation(cfile, meta$Encoding)
  envir <- new.env(hash = TRUE, parent = clock)
  assign("meta", meta, envir = envir)

  rval <- list()
  for (expr in exprs) {
    x <- eval(expr, envir)
    if (inherits(x, "bibentry")) rval <- c(rval, list(x))
  }
  if (length(rval) == 1L) rval[[1L]]
  else if (length(rval) > 1L) do.call(c, rval)
  else structure(list(), class = "bibentry")
}

# ---- Serialisation ---------------------------------------------------------

.ser_person <- function(p) {
  if (is.null(p) || length(p) == 0L) return("[]")
  out <- vapply(seq_along(p), function(i) {
    e <- p[[i]]
    cm <- tryCatch(e$comment, error = function(z) NULL)
    orcid <- if (!is.null(cm) && !is.null(names(cm)) && "ORCID" %in% names(cm)) {
      cm[["ORCID"]]
    } else NULL
    .jobj(list(
      given  = .jstr(paste(e$given,  collapse = " ")),
      family = .jstr(paste(e$family, collapse = " ")),
      role   = .jarr(e$role),
      email  = .jstr(e$email),
      orcid  = .jstr(orcid)
    ))
  }, character(1))
  paste0("[", paste(out, collapse = ","), "]")
}

# Fields are read positionally. `$.bibentry` returns one element per entry but
# unwraps at length 1 and drops NULLs, so unlist() on it silently misaligns a
# multi-entry object: two entries where only the second has a doi yield a
# length-1 vector.
.ser_entry <- function(bib, i, id) {
  raw  <- unclass(bib)[[i]]
  attrs <- attributes(raw)
  nm   <- names(raw)
  skip <- c("author", "editor")

  people <- if ("author" %in% nm) .ser_person(raw[["author"]]) else "[]"

  fields <- list()
  for (k in setdiff(nm, skip)) {
    v <- raw[[k]]
    if (is.null(v) || length(v) == 0L) next
    fields[[k]] <- if (inherits(v, "person")) .jstr(format(v)) else .jstr(as.character(v)[[1L]])
  }

  tv <- attrs[["textVersion"]]
  one <- function(x) if (is.null(x)) NULL else as.character(x)[[1L]]

  .jobj(list(
    t            = .jstr("entry"),
    id           = .jstr(id),
    i            = .jnum(i),
    bibtype      = .jstr(attrs[["bibtype"]]),
    title        = .jstr(one(raw[["title"]])),
    year         = .jstr(one(raw[["year"]])),
    authors      = people,
    fields       = .jobj(fields),
    text_version = .jarr(tv),
    header       = .jstr(attrs[["header"]]),
    footer       = .jstr(attrs[["footer"]]),
    key          = .jstr(attrs[["key"]]),
    bibtex       = .jstr(paste(format(bib[i], style = "Bibtex"),  collapse = "\n")),
    citation     = .jstr(paste(format(bib[i], style = "citation"), collapse = "\n"))
  ))
}

.header_scope <- function(bib, mheader) {
  n <- length(bib)
  if (n > 0L) {
    per <- vapply(seq_len(n),
                  function(i) !is.null(attr(unclass(bib)[[i]], "header")),
                  logical(1))
    if (any(per)) return("per-entry")
  }
  if (!is.null(mheader) && nzchar(mheader)) return("document")
  "none"
}

# ---- Driver ----------------------------------------------------------------
# Everything lives in a function so that nothing of the reader's own state sits
# in globalenv(), where an evaluated citation file could reach it.

.run <- function(manifest_path, out_path) {
  jobs <- read.delim(manifest_path, header = FALSE, sep = "\t",
                     colClasses = "character", quote = "", comment.char = "")
  names(jobs) <- c("id", "dir", "released")

  con <- file(out_path, open = "wt", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  for (j in seq_len(nrow(jobs))) {
    id   <- jobs$id[j]
    dir  <- jobs$dir[j]
    rel  <- jobs$released[j]
    cfile <- file.path(dir, "inst", "CITATION")

    out <- tryCatch({
      if (!file.exists(cfile)) stop("staged CITATION file is missing")
      meta <- as.list(read.dcf(file.path(dir, "DESCRIPTION"))[1L, ])
      bib  <- .eval_citation(cfile, meta, rel)

      mh <- attr(bib, "mheader")
      mf <- attr(bib, "mfooter")
      n  <- length(bib)

      doc <- .jobj(list(
        t            = .jstr("doc"),
        id           = .jstr(id),
        status       = .jstr(if (n > 0L) "ok" else "empty"),
        n_entries    = .jnum(n),
        mheader      = .jstr(mh),
        mfooter      = .jstr(mf),
        header_scope = .jstr(.header_scope(bib, mh)),
        message      = "null"
      ))
      c(doc, if (n > 0L) vapply(seq_len(n), function(i) .ser_entry(bib, i, id),
                                character(1)) else character(0L))
    }, error = function(e) {
      .jobj(list(
        t            = .jstr("doc"),
        id           = .jstr(id),
        status       = .jstr("error"),
        n_entries    = .jnum(0L),
        mheader      = "null",
        mfooter      = "null",
        header_scope = .jstr("none"),
        message      = .jstr(conditionMessage(e))
      ))
    })

    writeLines(out, con)
    flush(con)
  }

  writeLines("{\"t\":\"end\"}", con)
  flush(con)
  close(con)
  on.exit()
  # SIGKILL rather than a clean exit: a finalizer registered by an evaluated
  # citation file runs at R shutdown, after the terminator is written, and could
  # otherwise append to the stream. The parent treats signal death as normal
  # provided the terminator is present.
  tools::pskill(Sys.getpid(), tools::SIGKILL)
}

if (identical(sys.nframe(), 0L)) {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) < 2L) {
    stop("usage: Rscript --vanilla cite_reader.R <manifest.tsv> <out.ndjson>")
  }
  .run(a[[1L]], a[[2L]])
}
