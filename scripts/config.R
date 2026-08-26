# scripts/config.R: pipeline-wide constants and base helpers.
# Source this first; all other scripts assume these are defined.

CRAN_GIT_BASE <- "https://github.com/cran"
PUBLISH_REPO  <- "r-observatory/cran-code-metrics"
DB_FILENAME   <- "cran-code-metrics.db"
DATA_DB_FILENAME <- "cran-data-metrics.db"
SHARD_SIZE         <- 400L
MAX_CLONE_FAILURES <- 5L
WORK_DIR           <- "work"

# How many times a package may be handed to the analyzer without being read
# before the backfill queues stop asking for it. The same shape as
# MAX_CLONE_FAILURES, for the same reason: a package with no way of leaving a
# queue keeps the pipeline reporting a change forever and publishing a dated
# release for a database that has not moved.
#
# The queues it governs are the ones only the analyzer can satisfy. n_fns_r and
# the dataset rows come from the binary and from nowhere else, so a package the
# pure-R fallback analysed carries neither, and both queues hand it straight
# back. Nothing about the package changes between one such run and the next.
#
# Lower than the clone cap because the two failures are not alike. A clone
# fails on the network, so the next attempt is a genuinely different one and
# five of them are worth making. A read fails on what the package contains, and
# one build's answer is the same every time it is asked: the second attempt is
# there for a run that failed for a reason other than the package, a killed
# worker or a timeout, and a third would only collect the same answer again.
#
# Not a permanent verdict. The record carries the build that could not read the
# package, and a later build clears it, so the retirement lasts exactly as long
# as the reader it was measured against.
MAX_ANALYZER_READ_ATTEMPTS <- 2L

# Release notes: what GitHub refuses, and how much of it we allow ourselves.
#
# GitHub rejects a release body over 125,000 characters. The workflow publishes
# with `publish_metrics || exit 1`, so an oversized body does not cost us a
# shortened list, it stops the run, and the run it stops is the one with the
# most to report: a --recollect pass, or a catch-up after an outage, marks
# thousands of packages changed and the bootstrap marked 33,282.
#
# Bytes, not characters, because the limit applies to what GitHub receives and a
# package name, a version string or a maintainer's name can be multi-byte UTF-8,
# where one character costs up to four. Counting characters would under-measure
# such a body by up to a factor of four, which is exactly the case a bound is
# for.
#
# 60,000 is under half the limit. Published bodies in this series measured 267
# to 2,729 bytes, so the budget is already more than twenty times the largest
# one ever published, and the remaining 65,000 bytes are headroom for a section
# someone adds later without re-reading this comment. A budget set just under
# the limit would spend that headroom on rows nobody reads and leave nothing for
# the mistake.
NOTES_BODY_GITHUB_LIMIT <- 125000L
NOTES_BODY_MAX_BYTES    <- 60000L

# Rows of the changed-package table. Editorial, not the safety bound: the byte
# budget above is what makes the body safe whatever the names look like. Forty
# rows is about a screen, and the rows that survive are the ones with the
# largest API change (see .build_package_rows), not the front of the alphabet.
NOTES_TABLE_MAX_ROWS <- 40L

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

# The most one worker's progress line may be, in bytes.
#
# The workers are mclapply forks, all writing to the same inherited fd 1. A
# write that fits in one pipe buffer arrives whole, so two forks' lines are
# reordered but never spliced into each other; a longer write can be split and
# leave half of one package's line inside another's. POSIX guarantees PIPE_BUF
# is at least 512 bytes, which is what macOS uses where Linux uses 4096, so 512
# is the bound that holds wherever this runs.
#
# It matters because the line now carries the reason a package failed, and a
# condition message is as long as whatever it quoted.
WORKER_LINE_MAX_BYTES <- 512L

# The smallest reclaim worth rewriting a database for.
#
# SQLite gives a deleted page to the database's own free list, never back to
# the filesystem, so both published databases sit at their high-water mark
# whatever they currently hold. The dataset side deletes and re-inserts every
# re-scanned package's rows: cran-data-metrics.db was published byte-identical
# at 1,208,176,640 four days running while its contents changed every one of
# them. The code side is the one with less room, at 1,837,748,224 bytes against
# the workflow's 2,040,109,465-byte publish refusal, so pages nobody is using
# are what stands between a run and a release it cannot upload.
#
# VACUUM rewrites the whole file, which on a 1.8 GB database is minutes of a
# run's wall clock, so it is not something to do for a handful of pages. Below
# this the free list is doing its job and the next inserts will take those
# pages back.
VACUUM_MIN_RECLAIM_BYTES <- 64 * 1024^2

# The largest column profile a single dataset row may carry, in bytes.
#
# Nothing bounded this. The profile is a JSON array with one entry per column,
# so its size follows the width of what was read, and a file read as something
# it is not can be read as having millions of columns: three of them in the
# published data measure 321 MB, 117 MB and 63 MB, from an analyzer that
# mistook a file with only carriage returns for one very long line.
#
# A value that size is not merely large, it is unservable. The viewer's MySQL
# refuses any single value over max_allowed_packet, whose 32 MiB ceiling is a
# hard one that cannot be raised, and a write over it fails the load of the
# whole table rather than of the one row. That has already cost this org three
# days of cold loads.
#
# 4 MiB is an eighth of that ceiling, so a refused row still leaves the rest of
# the profile room inside a packet, and it is far above what a real schema
# costs: one column's entry runs to a few hundred bytes, so this is thousands
# of columns before anything is refused. The bound is aimed at the misparse,
# not at wide data.
MAX_DATASET_COLUMNS_BYTES <- 4 * 1024^2

# What the catalog says beside a dataset the reader took no measurement of.
#
# Such a record keeps its place in the catalog with no profile behind it, and
# the confidence and the note on its version link are the whole of what can be
# said about it. Neither said it. The analyzer's confidence is about the file
# it opened rather than about the values inside it, so these records arrive
# calling themselves `exact` with no note (an rda holding no object) or
# `degraded` with one (an S4 class the reader has no representation for, an R
# script only R can run), and `degraded` claims a part was read where no part
# was. One value for all of them, so a reader can ask the question once, with
# whatever the analyzer did say kept after the note as the reason.
#
# `unmeasured` is a value the deployed viewer has never seen, which is the
# point: it renders the confidence as text and compares it against `exact`
# alone, so an unfamiliar value reads as not-exact, which is true.
DATASET_UNMEASURED_CONFIDENCE <- "unmeasured"
DATASET_UNMEASURED_NOTE <- "no fingerprint was taken, so this record has no profile"

# VACUUM builds the compacted database beside the original and then copies it
# back over it under a rollback journal, so at its peak the file exists about
# twice over on top of itself. Ask for that much free space and skip the
# reclaim when it is not there: a run that cannot reclaim still has a database
# worth publishing, and failing over a full disk would throw away the day's
# collection to save space nobody needed yet.
VACUUM_DISK_FACTOR <- 2

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
