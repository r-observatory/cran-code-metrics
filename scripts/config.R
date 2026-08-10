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

# Wall-clock cap for one package's whole citation pass, in seconds. A package
# with hundreds of versions still gets one bounded run: the cost is dominated by
# starting the reader, not by evaluating any one file.
CITATION_TIMEOUT <- as.integer(Sys.getenv("CITATION_TIMEOUT", unset = "180"))

# Resource ceilings for the citation reader, applied by the shell that launches
# it and so inherited by anything it starts (scripts/citation.R).
#
# The wall clock above bounds elapsed time and nothing else. It does not notice
# a file that allocates until the machine swaps, or one that writes until the
# disk fills, and both of those hurt the sibling workers rather than only the
# package being read.
#
# There is deliberately no cap on processes. RLIMIT_NPROC is counted per uid,
# not per process tree, so a value low enough to stop a fork bomb would be
# reached by the pipeline's own parallel workers first and would fail the run it
# was meant to protect.

# CPU seconds. Kept below CITATION_TIMEOUT so a file that spins is stopped by
# the CPU limit and reported as a crash, rather than holding the wall clock open
# for the whole package. Evaluating a citation file is parsing a few dozen lines
# and formatting the result; a whole package's pass is dominated by R's own
# startup, so this is orders of magnitude of headroom.
CITATION_CPU_SECONDS <- as.integer(Sys.getenv("CITATION_CPU_SECONDS", unset = "120"))

# Address space in kilobytes (ulimit -v). R starts and reads citations inside a
# few hundred megabytes, so 4 GiB leaves room for an honest but large bibentry
# while still refusing an allocation that would push the runner into swap.
# macOS rejects this limit outright, so it is applied on its own line and its
# failure does not prevent the other two from taking effect.
CITATION_ADDRESS_SPACE_KB <- as.integer(
  Sys.getenv("CITATION_ADDRESS_SPACE_KB", unset = "4194304"))

# Largest single file the reader may write (ulimit -f), in blocks. Its own
# output is one NDJSON record per version plus one per entry, a few megabytes
# for the largest package on CRAN. bash counts 1024-byte blocks and dash counts
# 512-byte blocks, so the effective cap is 256 MiB or 128 MiB depending on which
# /bin/sh is present; both sit in the same wide gap between what the reader
# writes and a size that would matter. It caps one file, not the total written,
# so it bounds a runaway write rather than preventing a deliberate one.
CITATION_FILE_SIZE_KB <- as.integer(
  Sys.getenv("CITATION_FILE_SIZE_KB", unset = "262144"))
