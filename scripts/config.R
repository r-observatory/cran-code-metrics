# scripts/config.R: pipeline-wide constants and base helpers.
# Source this first; all other scripts assume these are defined.

CRAN_GIT_BASE <- "https://github.com/cran"
PUBLISH_REPO  <- "r-observatory/cran-code-metrics"
DB_FILENAME   <- "cran-code-metrics.db"
DATA_DB_FILENAME <- "cran-data-metrics.db"
SHARD_SIZE         <- 400L
MAX_CLONE_FAILURES <- 5L
WORK_DIR           <- "work"

# Per-git-subprocess timeout in seconds. A hard cap so a pathological repo
# cannot stall a parallel shard. Overridable via GIT_TIMEOUT env var.
GIT_TIMEOUT <- as.integer(Sys.getenv("GIT_TIMEOUT", unset = "300"))

# Number of parallel workers for the per-package clone+analyze step.
# Default: all logical cores (overridable via ANALYSIS_CORES env var).
ANALYSIS_CORES <- {
  dc <- suppressWarnings(parallel::detectCores(logical = TRUE))
  max(1L, as.integer(Sys.getenv("ANALYSIS_CORES",
    unset = as.character(if (is.na(dc)) 1L else dc))))
}

# Per-package analysis timeout in seconds. A hard cap so a pathological
# file in a metric group (e.g. a catastrophic regex) cannot stall a shard.
# Overridable via WORKER_TIMEOUT env var.
WORKER_TIMEOUT <- as.integer(Sys.getenv("WORKER_TIMEOUT", unset = "600"))

#' Null/empty coalescing operator.
#' Returns b when a is NULL, length-0, or a scalar NA.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
}

# Vignette source files, by the extensions R's registered vignette engines
# build: Sweave (.Rnw, .Rtex), knitr (.Rmd, .Rhtml, .Rrst, .Rtex), Quarto
# (.qmd) and litedown (.md). Case-insensitive on the leading R because both
# spellings occur in the wild.
#
# Enumerated because R itself enumerates, but kept in one place and paired with
# a count of the files in vignettes/ that no pattern claims, so the next engine
# to ship shows up as an unrecognised file rather than as an absence of
# vignettes. That is how .qmd went unnoticed here for years.
VIGNETTE_SOURCE_RE <- "^vignettes/.*\\.([Rr](md|nw|html|rst|tex)|qmd|md)$"

# README sources, in the order a reader encounters them: the rendered .md if the
# package ships one, else the source it was knitted from. README.qmd was missing
# here, so a Quarto README reported as no README and every README metric was
# computed against a file that was never found.
README_SOURCES <- c("README.md", "README.Rmd", "README.qmd", "README.markdown")

# R source files under R/. One definition, because there were four: security and
# health matched .R only, functions matched .R and .r, and tests matched every
# file under R/ whatever its extension. A package using the lowercase .r that R
# has always accepted had its functions counted and was then skipped by every
# security and health metric, silently, with the row still looking populated.
R_SOURCE_RE <- "^R/.*\\.[Rr]$"
