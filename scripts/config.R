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

# Image the citation reader runs in. Docker is chosen over bubblewrap because it
# is preinstalled on the runner and, since its daemon runs as root, it does not
# depend on unprivileged user namespaces, which Ubuntu restricts by default and
# which differ between runner images. Pin by digest so an image rebuild cannot
# change the boundary under a scheduled run. Resolve a new digest with:
#   docker buildx imagetools inspect rocker/r-ver:4.4.1 --format '{{.Manifest.Digest}}'
CITATION_IMAGE <- Sys.getenv(
  "CITATION_IMAGE",
  unset = "rocker/r-ver:4.4.1")

# Wall-clock cap for one package's whole citation pass, in seconds. A package
# with hundreds of versions still gets one bounded run: the cost is dominated by
# container start, not by evaluation.
CITATION_TIMEOUT <- as.integer(Sys.getenv("CITATION_TIMEOUT", unset = "180"))

# CITATION_SANDBOX: "require" refuses to evaluate without a container, "allow"
# permits an unsandboxed local run that is recorded as such. CI sets require.
# Not cached as a constant here: run_citation_reader() (citation.R) reads it
# live with Sys.getenv() at call time, the same pattern rpkg_analyzer_bin()
# (binary.R) uses for RPKG_ANALYZER_BIN, so a caller can set it per run
# without re-sourcing this file.
