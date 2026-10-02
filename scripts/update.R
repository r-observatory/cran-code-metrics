# scripts/update.R: sharded, resumable orchestration layer.
#
# Load order: config.R -> git.R -> context.R -> metrics/*.R -> analyze.R -> export.R -> update.R
# This file does NOT auto-source its dependencies; the caller controls source order.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Row-bind a list of data.frames, filling missing columns with NA.
# Any NULL or zero-row element is silently dropped.
# Returns NULL when no non-empty frames are present.
.rbind_union_all <- function(dfs) {
  dfs <- Filter(function(df) !is.null(df) && nrow(df) > 0L, dfs)
  if (length(dfs) == 0L) return(NULL)
  all_cols <- unique(unlist(lapply(dfs, names)))
  padded <- lapply(dfs, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    for (col in missing_cols) df[[col]] <- NA
    df[, all_cols, drop = FALSE]
  })
  do.call(rbind, padded)
}

.empty_summary <- function() {
  data.frame(package = character(0L), version = character(0L),
             stringsAsFactors = FALSE)
}

.empty_churn <- function() {
  data.frame(
    package = character(0L), version = character(0L),
    file    = character(0L), added   = integer(0L),
    deleted = integer(0L),
    stringsAsFactors = FALSE
  )
}

.empty_api <- function() {
  data.frame(
    package         = character(0L), version         = character(0L),
    exports_added   = character(0L), exports_removed = character(0L),
    n_exports       = integer(0L),
    stringsAsFactors = FALSE
  )
}

# Collapse a message to one line and cut it to a byte budget.
#
# One line because the caller's write has to stay a single write. Bytes rather
# than characters because the budget is a pipe's, and a path or a maintainer's
# name costs up to four bytes a character. Whole characters because cutting a
# multi-byte one in half leaves a string R cannot print.
.clip_bytes <- function(s, max_bytes) {
  s <- gsub("[[:space:]]+", " ", trimws(as.character(s)))
  if (max_bytes <= 0L) return("")
  if (nchar(s, type = "bytes") <= max_bytes) return(s)
  chars <- strsplit(s, "")[[1L]]
  keep  <- cumsum(nchar(chars, type = "bytes")) <= (max_bytes - 3L)
  paste0(paste(chars[keep], collapse = ""), "...")
}

#' The one line a worker prints when it finishes a package.
#'
#' Built here rather than inside the fork so what a failure says can be checked
#' without a subprocess, and so the byte bound that keeps the write atomic is
#' applied in one place.
#'
#' @param reason Why it failed, when there is one. NULL leaves the line as it
#'   was; anything else is appended after a colon, clipped so the whole line
#'   still fits in one pipe write.
#' @param elapsed Seconds the worker took; NA for a fork that returned nothing.
#' @param analyzer_exit The analyzer's non-zero exits, from .analyzer_exit_text;
#'   "" leaves them off.
#' @return A single string ending in one newline.
.worker_line <- function(idx, n, ok, pkg, stage, nver, elapsed, reason = NULL,
                         worker_timeout = WORKER_TIMEOUT, analyzer_exit = "") {
  stem <- if (isTRUE(ok)) {
    sprintf("[%d/%d] ok %s: %d versions in %.1fs%s", idx, n, pkg, nver, elapsed,
            if (isTRUE(elapsed >= worker_timeout))
              sprintf(" (past the %ds cap)", as.integer(worker_timeout)) else "")
  } else if (is.na(elapsed)) {
    sprintf("[%d/%d] FAIL %s: %s", idx, n, pkg, stage)
  } else {
    sprintf("[%d/%d] FAIL %s: %s after %.1fs", idx, n, pkg, stage, elapsed)
  }
  if (nzchar(analyzer_exit)) stem <- sprintf("%s [analyzer exit %s]", stem, analyzer_exit)
  if (is.null(reason) || !nzchar(trimws(as.character(reason)))) {
    return(paste0(stem, "\n"))
  }
  # Two for the ": " that joins them, one for the newline. A package name long
  # enough to leave no room takes the line it already had rather than a colon
  # with nothing after it.
  room <- WORKER_LINE_MAX_BYTES - nchar(stem, type = "bytes") - 3L
  if (room <= 3L) return(paste0(stem, "\n"))
  paste0(stem, ": ", .clip_bytes(reason, room), "\n")
}

# The analyzer's non-zero exits in a worker's tally, as "101 x2, 134 x1": each
# status and how many versions ended with it. "" when every exit was zero.
.analyzer_exit_text <- function(tally) {
  nm <- grep("^analyzer_exit_[0-9]+$", names(tally), value = TRUE)
  if (!length(nm)) return("")
  status <- as.integer(sub("^analyzer_exit_", "", nm))
  o <- order(status)
  paste(sprintf("%d x%d", status[o], as.integer(unlist(tally[nm[o]]))), collapse = ", ")
}

# The stage of an error analyze_package raised. An elapsed time at the cap also
# catches a cap swallowed earlier and a later failure.
.classify_failure <- function(e, elapsed, worker_timeout = WORKER_TIMEOUT) {
  if (inherits(e, "extract_failure")) {
    return(if (isTRUE(e$status == 124L)) "git_timeout" else "extract")
  }
  if (inherits(e, "analyzer_parse_incomplete")) return("analyze")
  if (inherits(e, c("analyzer_killed", "analyzer_failed"))) return("crash")
  if ((inherits(e, "condition") && .is_time_limit(e)) ||
      isTRUE(elapsed >= worker_timeout)) {
    return("timeout")
  }
  "analyze"
}

# The stage of a clone that did not succeed: 124 is system2's kill at GIT_TIMEOUT.
.clone_stage <- function(ok) {
  if (isTRUE(as.integer(attr(ok, "status")) == 124L)) "git_timeout" else "clone"
}

# What a clone that did not succeed says: the error it raised, or its exit status.
.clone_reason <- function(ok) {
  if (!is.null(attr(ok, "reason"))) return(attr(ok, "reason"))
  st <- attr(ok, "status")
  if (is.null(st)) "clone failed" else sprintf("git clone exited %d", as.integer(st))
}

# One worker result as the parent records it. A fork that returned nothing, or
# raised outside the worker's handlers, printed no line, so from_parent says
# the parent prints it; its time-limit message makes it a timeout.
.classify_result <- function(r) {
  if (is.null(r)) {
    return(list(ok = FALSE, stage = "crash", elapsed = NA_real_,
                reason = "worker returned no result", from_parent = TRUE))
  }
  if (inherits(r, "try-error")) {
    cond <- attr(r, "condition")
    msg  <- if (inherits(cond, "condition")) conditionMessage(cond) else as.character(r)
    return(list(ok = FALSE,
                stage = if (grepl(.time_limit_msg(), msg, fixed = TRUE)) "timeout" else "crash",
                elapsed = NA_real_, reason = .redact_reason(msg), from_parent = TRUE))
  }
  if (isTRUE(r$ok)) {
    return(list(ok = TRUE, stage = NA_character_, elapsed = r$elapsed %||% NA_real_,
                reason = "", from_parent = FALSE))
  }
  list(ok = FALSE, stage = r$stage %||% "analyze", elapsed = r$elapsed %||% NA_real_,
       reason = r$reason %||% "", from_parent = FALSE)
}

# The run a verdict belongs to: PIPELINE_RUN_ID, which the workflow sets in the
# shard step alone. GITHUB_RUN_ID is never read, since Actions sets it in the
# test steps too. NA outside a run, which keeps one attempt per shard.
.current_run_id <- function() {
  v <- Sys.getenv("PIPELINE_RUN_ID", "")
  if (nzchar(v)) v else NA_character_
}

# Packages that already failed in this run. Nothing outside a run, so tests and
# local runs keep today's one attempt per shard.
.tried_this_run <- function(con, run_id) {
  if (is.na(run_id) || !"cran_metrics_failures" %in% DBI::dbListTables(con)) {
    return(character(0L))
  }
  as.character(DBI::dbGetQuery(con,
    "SELECT package FROM cran_metrics_failures WHERE last_run_id = ?",
    params = list(run_id))$package)
}

# The build a verdict names: "" when no binary ran.
.build_key <- function(build) {
  if (is.null(build) || length(build) != 1L || is.na(build)) "" else as.character(build)
}

# The counter a stage counts in. A fetch never reached the analyzer, so its
# count carries across builds; the other two are verdicts on one build.
.failure_class <- function(stage) {
  if (stage %in% c("clone", "extract")) "fetch"
  else if (stage %in% c("timeout", "crash", "git_timeout")) "timeout"
  else "analyze"
}

# Record one failed attempt. fetch_failures restarts on a new latest_version and
# any other verdict zeroes it; analyze_failures and timeout_failures restart on a
# new build or a row from before stages were kept, and timeouts on a new cap.
.record_failure <- function(con, pkg, stage, build, worker_timeout, run_id,
                            elapsed, reason, latest_version) {
  cls <- .failure_class(stage)
  DBI::dbExecute(con, "
    INSERT INTO cran_metrics_failures
      (package, consecutive_failures, last_attempt, stage, analyzer_version,
       worker_timeout, fetch_failures, fetch_version, analyze_failures,
       timeout_failures, last_run_id, elapsed_s, reason)
    VALUES (:pkg, 1, :now, :stage, :build, :wt, :fetch,
            CASE WHEN :fetch = 1 THEN :lv END, :analyze, :timeout,
            :run_id, :elapsed, :reason)
    ON CONFLICT(package) DO UPDATE SET
      consecutive_failures = consecutive_failures + 1,
      fetch_failures   = CASE WHEN :fetch = 1
                              THEN (CASE WHEN IFNULL(fetch_version, '') = IFNULL(:lv, '')
                                         THEN fetch_failures ELSE 0 END) + 1
                              ELSE 0 END,
      fetch_version    = CASE WHEN :fetch = 1 THEN :lv ELSE fetch_version END,
      analyze_failures = (CASE WHEN stage IS NOT NULL
                                AND IFNULL(analyzer_version, '') = :build
                               THEN analyze_failures ELSE 0 END) + :analyze,
      timeout_failures = (CASE WHEN stage IS NOT NULL
                                AND IFNULL(analyzer_version, '') = :build
                                AND worker_timeout = :wt
                               THEN timeout_failures ELSE 0 END) + :timeout,
      last_attempt     = excluded.last_attempt,
      stage            = excluded.stage,
      analyzer_version = excluded.analyzer_version,
      worker_timeout   = excluded.worker_timeout,
      last_run_id      = excluded.last_run_id,
      elapsed_s        = excluded.elapsed_s,
      reason           = excluded.reason",
    params = list(
      pkg = pkg, now = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      stage = stage, build = .build_key(build), wt = as.integer(worker_timeout),
      fetch = as.integer(cls == "fetch"), analyze = as.integer(cls == "analyze"),
      timeout = as.integer(cls == "timeout"),
      lv = as.character(latest_version %||% NA_character_),
      run_id = as.character(run_id %||% NA_character_),
      elapsed = as.numeric(elapsed %||% NA_real_), reason = .redact_reason(reason)))
  invisible(NULL)
}

# Keep the standing over-cap list. A pass past the cap ran uncapped after the cap
# fired somewhere, so it goes on the list; a pass under the cap rewrote every
# stored row without crossing, so it comes off. TRUE when the package is on it.
.note_over_cap <- function(con, pkg, elapsed, build, run_id,
                           worker_timeout = WORKER_TIMEOUT) {
  if (!isTRUE(elapsed >= worker_timeout)) {
    DBI::dbExecute(con, "DELETE FROM cran_over_cap WHERE package = ?", params = list(pkg))
    return(FALSE)
  }
  DBI::dbExecute(con, "
    INSERT INTO cran_over_cap (package, elapsed_s, analyzer_version, last_run_id, recorded_at)
    VALUES (?, ?, ?, ?, ?)
    ON CONFLICT(package) DO UPDATE SET
      elapsed_s = excluded.elapsed_s, analyzer_version = excluded.analyzer_version,
      last_run_id = excluded.last_run_id, recorded_at = excluded.recorded_at",
    params = list(pkg, as.numeric(elapsed), .build_key(build),
                  as.character(run_id %||% NA_character_),
                  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  TRUE
}

# The standing over-cap list, longest analysis first.
.over_cap_packages <- function(con) {
  if (!"cran_over_cap" %in% DBI::dbListTables(con)) return(character(0L))
  as.character(DBI::dbGetQuery(con,
    "SELECT package FROM cran_over_cap ORDER BY elapsed_s DESC, package")$package)
}

# The comma-separated words of an --unpark or --requeue value.
.split_spec <- function(spec) {
  if (is.null(spec) || length(spec) != 1L || is.na(spec)) return(character(0L))
  w <- trimws(strsplit(spec, ",", fixed = TRUE)[[1L]])
  unique(w[nzchar(w)])
}

# The words that name a package this pipeline has rows or a verdict for. Any
# other word is warned about and ignored.
.operator_packages <- function(con, words) {
  tables <- intersect(c("cran_code_summary", "cran_metrics_failures"),
                      DBI::dbListTables(con))
  known <- vapply(words, function(p) {
    grepl("^[A-Za-z][A-Za-z0-9.]*$", p) && any(vapply(tables, function(t) {
      nrow(DBI::dbGetQuery(con, sprintf("SELECT 1 FROM %s WHERE package = ? LIMIT 1", t),
                           params = list(p))) > 0L
    }, logical(1L)))
  }, logical(1L), USE.NAMES = FALSE)
  if (any(!known)) {
    warning(sprintf("ignoring %s: not a package with rows or a verdict here",
                    paste(words[!known], collapse = ", ")),
            call. = FALSE, immediate. = TRUE)
  }
  words[known]
}

# Zero the three counters and the run id on the rows `where` selects, and stamp
# unparked_at. A row is never deleted, so on CRAN a release followed by a new
# failure does not grow the table the retention ceiling counts.
.release_verdicts <- function(con, where, params = list()) {
  DBI::dbExecute(con, sprintf(
    "UPDATE cran_metrics_failures
        SET fetch_failures = 0, analyze_failures = 0, timeout_failures = 0,
            last_run_id = NULL, unparked_at = ?
      WHERE %s", where),
    params = c(list(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), params))
}

#' Release parked verdicts, for --unpark.
#'
#' @param spec "all", "fetch", "analyze" or "timeout" (the class of a row's last
#'   stage), or package names separated by commas.
#' @return The number of rows released.
.unpark <- function(con, spec) {
  words <- .split_spec(spec)
  if (length(words) == 0L) return(0L)
  stages <- list(fetch = c("clone", "extract"), analyze = "analyze",
                 timeout = c("timeout", "crash", "git_timeout"))
  if (identical(words, "all")) return(.release_verdicts(con, "1 = 1"))
  if (length(words) == 1L && words %in% names(stages)) {
    st <- stages[[words]]
    return(.release_verdicts(con, sprintf("stage IN (%s)",
                                          paste(rep("?", length(st)), collapse = ", ")),
                             as.list(st)))
  }
  pkgs <- .operator_packages(con, words)
  if (length(pkgs) == 0L) return(0L)
  .release_verdicts(con, sprintf("package IN (%s)",
                                 paste(rep("?", length(pkgs)), collapse = ", ")),
                    as.list(pkgs))
}

#' Analyse packages again from scratch, for --requeue.
#'
#' Releases their verdicts, forgets their read attempts and clears
#' datasets_scanned on their latest row, so this run's dataset backfill takes
#' them and keeps them until they pass. Their stored rows stay as they are.
#'
#' @param spec Package names separated by commas; over_cap names the standing
#'   over-cap list.
#' @return The packages requeued.
.requeue <- function(con, spec) {
  words <- .split_spec(spec)
  pkgs  <- unique(c(if ("over_cap" %in% words) .over_cap_packages(con),
                    .operator_packages(con, setdiff(words, "over_cap"))))
  if (length(pkgs) == 0L) return(character(0L))
  ph <- paste(rep("?", length(pkgs)), collapse = ", ")
  .release_verdicts(con, sprintf("package IN (%s)", ph), as.list(pkgs))
  tables <- DBI::dbListTables(con)
  if ("cran_analyzer_read_attempts" %in% tables) {
    DBI::dbExecute(con, sprintf(
      "DELETE FROM cran_analyzer_read_attempts WHERE package IN (%s)", ph),
      params = as.list(pkgs))
  }
  if ("cran_code_summary" %in% tables &&
      all(c("datasets_scanned", "latest_release_date") %in%
          DBI::dbListFields(con, "cran_code_summary"))) {
    DBI::dbExecute(con, sprintf(
      "UPDATE cran_code_summary SET datasets_scanned = NULL
        WHERE latest_release_date IS NOT NULL AND package IN (%s)", ph),
      params = as.list(pkgs))
  }
  pkgs
}

# Parked verdicts by class, and the rows from before stages were kept.
.parked_counts <- function(st) {
  list(fetch   = sum(st$class %in% "fetch"),
       analyze = sum(st$class %in% "analyze"),
       timeout = sum(st$class %in% "timeout"),
       legacy  = sum(is.na(st$stage)))
}

# Failures by stage, in the order a package meets the stages; absent ones left out.
.stage_counts <- function(stages) {
  order <- c("clone", "extract", "git_timeout", "analyze", "timeout", "crash")
  n <- vapply(order, function(x) sum(stages == x), integer(1L))
  as.list(n[n > 0L])
}

# The standing over-cap list for a manifest: its size and the first 20 names.
.over_cap_block <- function(con) {
  p <- .over_cap_packages(con)
  list(count = length(p), packages = I(utils::head(p, 20L)))
}

# How many latest rows each analyzer build wrote; "none" for the R fallback.
.latest_by_build <- function(con) {
  empty <- stats::setNames(list(), character(0L))
  if (!"cran_code_summary" %in% DBI::dbListTables(con)) return(empty)
  fields <- DBI::dbListFields(con, "cran_code_summary")
  if (!"latest_release_date" %in% fields) return(empty)
  build <- if ("analyzer_version" %in% fields) {
    "IFNULL(NULLIF(analyzer_version, ''), 'none')"
  } else {
    "'none'"
  }
  df <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s AS build, COUNT(DISTINCT package) AS n FROM cran_code_summary
      WHERE latest_release_date IS NOT NULL GROUP BY 1 ORDER BY 1", build))
  stats::setNames(as.list(as.integer(df$n)), df$build)
}

# The shard plan's verdict line.
.verdict_plan_line <- function(build, n_released, st, n_tried) {
  p <- .parked_counts(st)
  b <- .build_key(build)
  sprintf(paste0("analyzer %s; verdicts released: %d; parked: fetch %d, analyze %d, ",
                 "timeout %d, legacy %d; skipped as tried this run: %d; ",
                 "fetch rechecks due: %d\n"),
          if (nzchar(b)) b else "none", as.integer(n_released), p$fetch, p$analyze,
          p$timeout, p$legacy, as.integer(n_tried), sum(st$recheck_due))
}

# The shard receipt's verdict line.
.verdict_receipt_line <- function(stages, over_cap, n_standing) {
  by <- .stage_counts(stages)
  sprintf("shard verdicts: %d failed%s; passed over the cap: %d%s; standing over-cap list: %d\n",
          length(stages),
          if (length(by)) sprintf(" (%s)", paste(names(by), unlist(by), collapse = ", ")) else "",
          length(over_cap),
          if (length(over_cap)) sprintf(" (%s)", paste(utils::head(over_cap, 20L),
                                                        collapse = ", ")) else "",
          as.integer(n_standing))
}

# Delete a package's failure record (reset after a successful analysis).
.reset_failure <- function(con, pkg) {
  DBI::dbExecute(con,
    "DELETE FROM cran_metrics_failures WHERE package = ?",
    params = list(pkg))
  invisible(NULL)
}

#' Every failure verdict, and whether it parks its package.
#'
#' A verdict parks by build when this build failed the package
#' MAX_CLONE_FAILURES times at analyze, or MAX_TIMEOUT_FAILURES times at a
#' timeout under this cap. It parks by fetch when the package failed to fetch
#' MAX_CLONE_FAILURES times at its current latest_version, unless it has no
#' stored rows and its last attempt is FETCH_RECHECK_DAYS old. A row from before
#' stages were kept (stage NULL) parks nothing.
#'
#' @param universe data.frame(package, latest_version), NA for an archived one.
#' @return data.frame(package, stage, class, parked, recheck_due); class is
#'   "fetch", "analyze" or "timeout" for a parked package and NA otherwise.
.verdict_state <- function(con, build, worker_timeout, universe, now = Sys.time()) {
  tables <- DBI::dbListTables(con)
  if (!"cran_metrics_failures" %in% tables) {
    return(data.frame(package = character(0L), stage = character(0L),
                      class = character(0L), parked = logical(0L),
                      recheck_due = logical(0L), stringsAsFactors = FALSE))
  }
  has_rows <- if ("cran_code_summary" %in% tables) {
    "EXISTS (SELECT 1 FROM cran_code_summary s WHERE s.package = f.package)"
  } else {
    "0"
  }
  df <- DBI::dbGetQuery(con, sprintf("
    SELECT f.package, f.stage, f.fetch_failures, f.fetch_version, f.last_attempt,
           %s AS has_rows,
           (IFNULL(f.analyzer_version, '') = :build
              AND f.analyze_failures >= :max_fail) AS by_analyze,
           (IFNULL(f.analyzer_version, '') = :build AND f.worker_timeout = :wt
              AND f.timeout_failures >= :max_timeouts) AS by_timeout
      FROM cran_metrics_failures f
     ORDER BY f.package", has_rows),
    params = list(build = .build_key(build), wt = as.integer(worker_timeout),
                  max_fail = MAX_CLONE_FAILURES, max_timeouts = MAX_TIMEOUT_FAILURES))
  live <- !is.na(df$stage)
  lv   <- as.character(universe$latest_version)[
    match(df$package, as.character(universe$package))]
  same_release <- ifelse(is.na(df$fetch_version), "", df$fetch_version) ==
    ifelse(is.na(lv), "", lv)
  age <- as.numeric(difftime(
    now, as.POSIXct(df$last_attempt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    units = "days"))
  fetch_capped <- live & df$fetch_failures >= MAX_CLONE_FAILURES & same_release
  recheck_due  <- fetch_capped & df$has_rows %in% 0L & !is.na(age) &
    age >= FETCH_RECHECK_DAYS
  class <- ifelse(live & df$by_analyze %in% 1L, "analyze",
           ifelse(live & df$by_timeout %in% 1L, "timeout",
           ifelse(fetch_capped & !recheck_due, "fetch", NA_character_)))
  data.frame(package = df$package, stage = df$stage, class = class,
             parked = !is.na(class), recheck_due = recheck_due,
             stringsAsFactors = FALSE)
}

# Packages whose verdict parks them, left out of every queue.
.permanent_failures <- function(con, build, worker_timeout, universe,
                                now = Sys.time()) {
  st <- .verdict_state(con, build, worker_timeout, universe, now)
  st$package[st$parked]
}

# The versions of each package whose stored summary row names an analyzer
# build, any build: the rows a failed analyzer must not hand to the R fallback.
# A list of version vectors named by package; a package with none is absent.
.stamped_versions <- function(con, pkgs) {
  pkgs <- as.character(pkgs)
  if (!length(pkgs) || !SUMMARY_TABLE %in% DBI::dbListTables(con) ||
      !"analyzer_version" %in% DBI::dbListFields(con, SUMMARY_TABLE)) {
    return(list())
  }
  rows <- lapply(split(pkgs, ceiling(seq_along(pkgs) / 500)), function(p) {
    DBI::dbGetQuery(con, sprintf(
      'SELECT package, version FROM "%s"
        WHERE LENGTH(analyzer_version) > 0 AND package IN (%s)
        ORDER BY package, version',
      SUMMARY_TABLE, paste(rep("?", length(p)), collapse = ", ")),
      params = as.list(p))
  })
  df <- do.call(rbind, unname(rows))
  if (!nrow(df)) return(list())
  split(as.character(df$version), as.character(df$package))
}

# ---------------------------------------------------------------------------
# The third state of a scan
# ---------------------------------------------------------------------------
# datasets_scanned answers two of the three states a package can be in: the
# reader ran (whatever it found, zero rows included), or nothing looked. The
# third is a package the reader was asked for and could not read, which the
# marker cannot say without claiming a scan that did not happen. Recorded here
# instead, in the shape this pipeline already uses for a package that cannot be
# cloned: a count, a cap, and no place in the queue past it.
#
# Both backfill queues need it, because both wait on fields only the analyzer
# produces: n_fns_r and the dataset rows. A package the pure-R fallback
# analysed carries neither, so each queue hands it straight back, every run,
# for good. `changed` never goes false and the workflow publishes a dated
# release for a database that has not moved.
#
# Recorded per version, because the queues do not ask the same question of the
# same row. The n_fns_r queue flags a package when ANY of its stored rows has
# no count, so one old version the analyzer cannot read holds the package in it
# whatever happens to the newest one. A count kept per package is cleared by
# the read that succeeded on the newest version, so nothing was ever counted
# against the row that keeps the package there.

# Which of a package's versions the analyzer did not produce metrics for.
# analyze_package returns the versions the binary produced, which is the only
# thing that tells those summary rows from the ones the pure-R fallback wrote
# once they are in one frame. A caller that names none of them answers for none
# of them, exactly as .stamp_analyzer_version refuses to: claiming a read
# nobody reported would retire the row from its queue on a guess.
.analyzer_unread_versions <- function(summary_df, binary_versions) {
  if (is.null(summary_df) || !is.data.frame(summary_df) ||
      nrow(summary_df) == 0L || !"version" %in% names(summary_df)) {
    return(character(0L))
  }
  stored <- unique(as.character(summary_df$version))
  stored[!stored %in% as.character(binary_versions %||% character(0L))]
}

# Count one attempt that did not read this version, against the build that made
# it. The build is part of the record: an attempt says nothing about a reader
# other than the one that made it.
.record_analyzer_read_attempt <- function(con, pkg, version,
                                          analyzer_version = NA_character_) {
  if (!"cran_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(invisible(NULL))
  ver <- if (is.null(analyzer_version) || length(analyzer_version) != 1L ||
             is.na(analyzer_version) || !nzchar(analyzer_version)) {
    NA_character_
  } else {
    as.character(analyzer_version)
  }
  now_str  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  existing <- DBI::dbGetQuery(con,
    "SELECT attempts FROM cran_analyzer_read_attempts
      WHERE package = ? AND version = ?",
    params = list(pkg, version))
  if (nrow(existing) == 0L) {
    DBI::dbExecute(con,
      "INSERT INTO cran_analyzer_read_attempts
         (package, version, attempts, analyzer_version, last_attempt)
       VALUES (?, ?, 1, ?, ?)",
      params = list(pkg, version, ver, now_str))
  } else {
    DBI::dbExecute(con,
      "UPDATE cran_analyzer_read_attempts
       SET attempts = attempts + 1, analyzer_version = ?, last_attempt = ?
       WHERE package = ? AND version = ?",
      params = list(ver, now_str, pkg, version))
  }
  invisible(NULL)
}

# Forget a package's attempts, keeping only the versions named.
#
# A count that survived a successful read would retire a version that failed
# once on a bad day sooner than the cap says, so every version this run read is
# forgotten. So is every version the run did not see at all: a tag that is no
# longer in the repository leaves a record nothing can ever answer, and the
# package would sit outside both queues on the strength of a version it does
# not have. `keep` is therefore what this run could not read, not what it
# could, and everything else goes.
.clear_analyzer_read_attempts <- function(con, pkg, keep = character(0L)) {
  if (!"cran_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(invisible(NULL))
  keep <- as.character(keep)
  if (length(keep) == 0L) {
    DBI::dbExecute(con,
      "DELETE FROM cran_analyzer_read_attempts WHERE package = ?",
      params = list(pkg))
    return(invisible(NULL))
  }
  DBI::dbExecute(con, sprintf(
    "DELETE FROM cran_analyzer_read_attempts
      WHERE package = ? AND version NOT IN (%s)",
    paste(rep("?", length(keep)), collapse = ",")),
    params = c(list(pkg), as.list(keep)))
  invisible(NULL)
}

# The builds counted as the running one: all of ANALYZER_SAME_OUTPUT when it
# lists the running build, that build alone when it does not, and none when the
# build cannot be named. Matching is exact, so "0.5.0-test" is not 0.5.0.
.analyzer_output_class <- function(build, same_output = ANALYZER_SAME_OUTPUT) {
  if (is.null(build) || length(build) != 1L || is.na(build) || !nzchar(build)) {
    return(character(0L))
  }
  build <- as.character(build)
  if (build %in% same_output) as.character(same_output) else build
}

# Latest rows written by a build in the running build's class, and all latest
# rows: how far a rescan onto that class has come.
.n_latest_on_class <- function(con, build, same_output = ANALYZER_SAME_OUTPUT) {
  out <- c(on_class = 0L, latest = 0L)
  if (!SUMMARY_TABLE %in% DBI::dbListTables(con)) return(out)
  fields <- DBI::dbListFields(con, SUMMARY_TABLE)
  if (!"latest_release_date" %in% fields) return(out)
  out[["latest"]] <- as.integer(DBI::dbGetQuery(con, sprintf(
    'SELECT COUNT(*) n FROM "%s" WHERE latest_release_date IS NOT NULL',
    SUMMARY_TABLE))$n)
  builds <- .analyzer_output_class(build, same_output)
  if (length(builds) && "analyzer_version" %in% fields) {
    out[["on_class"]] <- as.integer(DBI::dbGetQuery(con, sprintf(
      'SELECT COUNT(*) n FROM "%s" WHERE latest_release_date IS NOT NULL
          AND analyzer_version IN (%s)',
      SUMMARY_TABLE, paste(rep("?", length(builds)), collapse = ",")),
      params = as.list(builds))$n)
  }
  out
}

# Drop attempts made by any build outside the running build's output class.
#
# The count is the verdict of one reader, and a verdict that outlives its
# reader retires a package for good on the say-so of a build nobody runs any
# more. The row it protects is not re-queued by .invalidate_stale_dataset_scans
# either, since that one only clears markers and this package has none, so this
# is the only thing that gives a new build the chance to read it.
#
# Does nothing when the running build cannot be named, for the same reason
# .invalidate_stale_dataset_scans does nothing: a run with no binary records
# its attempts against no build, and clearing those on the next such run would
# reset the count every time and the queue would never drain.
.forget_other_builds_read_attempts <- function(con, current_version,
                                               same_output = ANALYZER_SAME_OUTPUT) {
  if (!"cran_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(0L)
  builds <- .analyzer_output_class(current_version, same_output)
  if (!length(builds)) return(0L)
  DBI::dbExecute(con, sprintf(
    "DELETE FROM cran_analyzer_read_attempts
      WHERE analyzer_version IS NULL OR analyzer_version NOT IN (%s)",
    paste(rep("?", length(builds)), collapse = ",")),
    params = as.list(builds))
}

# Packages the backfill queues have stopped asking about.
#
# One version at the cap is enough, because the queues ask for packages: a
# version that will never be read is a package that will never leave the queue
# waiting on it, however many of its other versions were read on the first try.
.analyzer_read_exhausted <- function(con) {
  if (!"cran_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(character(0L))
  as.character(DBI::dbGetQuery(con,
    "SELECT DISTINCT package FROM cran_analyzer_read_attempts WHERE attempts >= ?",
    params = list(MAX_ANALYZER_READ_ATTEMPTS))$package)
}

#' How many packages the pipeline has stopped asking for datasets.
#'
#' A subset of .n_datasets_unscanned(): every one of these is honestly unread,
#' because the reader that would have scanned them is the binary that could not
#' read them at all. The difference is that this number does not come down on
#' its own, which is the fact worth publishing. It is what the deliberate slow
#' convergence costs, and if it climbs, the reader is failing on packages
#' rather than on one.
#'
#' Scoped to the latest-version row, the way .n_datasets_unscanned() and the
#' dataset queue are, and unlike .analyzer_read_exhausted() beneath it. The
#' queues give a package up when any one of its versions is at the cap, but a
#' package whose newest version was read has its datasets: what went unread
#' there is an older version's metrics, which is a different gap and not this
#' number's.
#'
#' A database holding attempts and no summary rows cannot say which version is
#' a package's newest, so every package with an exhausted version counts. The
#' figure is a ceiling there rather than a guess.
.n_datasets_unreadable <- function(con) {
  exhausted <- .analyzer_read_exhausted(con)
  if (length(exhausted) == 0L) return(0L)
  if (!"cran_code_summary" %in% DBI::dbListTables(con)) return(length(exhausted))
  if (!"latest_release_date" %in% DBI::dbListFields(con, "cran_code_summary")) {
    return(length(exhausted))
  }
  as.integer(DBI::dbGetQuery(con,
    "SELECT COUNT(DISTINCT a.package) n
       FROM cran_analyzer_read_attempts a
       JOIN cran_code_summary s
         ON s.package = a.package AND s.version = a.version
      WHERE a.attempts >= ? AND s.latest_release_date IS NOT NULL",
    params = list(MAX_ANALYZER_READ_ATTEMPTS))$n %||% 0L)
}

#' How many datasets are in the catalog with no profile behind them.
#'
#' A dataset the analyzer described and could not fingerprint keeps its identity
#' row and its version link and gets no profile row, because a profile invented
#' for it would put two objects that were never compared on the row that says
#' "the same data in N packages". That is the right answer and it is also a
#' coverage figure: an S4 object with no reader, a raster packed into bytes, an
#' .R script under data/, a compressed archive data() will not open, a frame
#' whose every column is a generated sequence. A shard where the number climbs
#' is the reader losing objects it used to measure.
#'
#' Taken over every version link rather than the current ones alone, because a
#' version that stopped being measurable is the same finding as a package that
#' never was, and the denominator beside it in the manifest is the count of
#' links the same table holds.
#'
#' Reads the dataset database, not the code one. Zero where the link table does
#' not exist yet, which is a database built from nothing before its first write.
#'
#' @return Count of version links naming no profile.
.n_datasets_unmeasured <- function(con) {
  if (!"cran_dataset_versions" %in% DBI::dbListTables(con)) return(0L)
  as.integer(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM cran_dataset_versions WHERE content_id IS NULL")$n %||% 0L)
}

#' Clear the dataset-scan marker on rows produced by a build outside the running
#' build's output class (.analyzer_output_class).
#'
#' The marker records that a package was scanned, not what scanned it, so after
#' an upgrade every package looks done and nothing re-runs. Comparing against the
#' version the binary reports puts the stale ones back in the queue.
#'
#' Does nothing when the running version cannot be determined: clearing on a
#' guess would re-scan the archive on every run and never settle.
.invalidate_stale_dataset_scans <- function(con, current_version,
                                            same_output = ANALYZER_SAME_OUTPUT) {
  if (!"cran_code_summary" %in% DBI::dbListTables(con)) return(0L)
  fields <- DBI::dbListFields(con, "cran_code_summary")
  if (!"datasets_scanned" %in% fields) return(0L)
  builds <- .analyzer_output_class(current_version, same_output)
  if (!length(builds)) return(0L)
  if (!"analyzer_version" %in% fields) {
    # Nothing on these rows says which build produced them, so none of them can
    # be shown to match the one running now. The column arrives with the first
    # row the analyzer produces, and a database holding nothing but fallback
    # rows never grows it: that database also has no scan marker to clear, so
    # this clears nothing on every run rather than the same rows forever.
    return(DBI::dbExecute(con,
      "UPDATE cran_code_summary SET datasets_scanned = NULL
        WHERE datasets_scanned IS NOT NULL"))
  }
  DBI::dbExecute(con, sprintf(
    "UPDATE cran_code_summary SET datasets_scanned = NULL
      WHERE datasets_scanned IS NOT NULL
        AND (analyzer_version IS NULL OR analyzer_version NOT IN (%s))",
    paste(rep("?", length(builds)), collapse = ",")),
    params = as.list(builds))
}

# Address one summary row the way the shard's producers name it. Package names
# and version strings cannot contain a carriage return, so the pair survives
# being flattened into one key.
.analyzer_row_keys <- function(package, versions) {
  versions <- as.character(versions)
  if (length(versions) == 0L) return(character(0L))
  paste(as.character(package), versions, sep = "\r")
}

#' Record the analyzer build a shard was collected under.
#'
#' The clearing above only settles because the write that follows leaves the
#' build behind: a scanned row that names no build is one the next run cannot
#' show to be current, so it is cleared again, re-queued, re-analysed, and the
#' run reports a change on a universe where nothing changed. analyze_package
#' stamps the build on the rows the analyzer's own output named and leaves the
#' rest to be filled in here, where the run knows which build it is running.
#'
#' Only the rows the analyzer produced. The pure-R fallback writes rows too,
#' and putting the running build on one of those says the analyzer collected
#' data the analyzer never saw. It is the same false claim datasets_scanned is
#' withheld to avoid, on the same row, so the column would contradict the
#' marker beside it. Nothing is lost by leaving those rows blank:
#' .invalidate_stale_dataset_scans only reads rows that carry a scan marker,
#' and a fallback row carries none, so it is never compared against a build in
#' the first place.
#'
#' A build the analyzer already named is left alone: overwriting it would erase
#' the one signal that tells a row collected by an older build from one
#' collected by this one.
#'
#' @param version The build about to run, from rpkg_analyzer_version(). NA when
#'   there is no binary to ask, in which case nothing is written: a guess would
#'   make every row look current and stop the queue noticing an upgrade at all.
#' @param produced Keys, from .analyzer_row_keys(), of the rows the analyzer
#'   binary produced. Empty by default, which stamps nothing: a caller that
#'   cannot say which rows the analyzer wrote must not answer for it.
#' @return summary_df, with analyzer_version filled on those rows where it was
#'   missing.
.stamp_analyzer_version <- function(summary_df, version,
                                    produced = character(0L)) {
  if (is.null(summary_df) || nrow(summary_df) == 0L) return(summary_df)
  if (is.null(version) || length(version) != 1L || is.na(version) ||
      !nzchar(version)) {
    return(summary_df)
  }
  if (length(produced) == 0L) return(summary_df)
  if (!all(c("package", "version") %in% names(summary_df))) return(summary_df)
  mine <- .analyzer_row_keys(summary_df$package, summary_df$version) %in% produced
  if (!"analyzer_version" %in% names(summary_df)) {
    summary_df$analyzer_version <- NA_character_
  }
  have <- as.character(summary_df$analyzer_version)
  gap  <- mine & (is.na(have) | !nzchar(have))
  have[gap] <- as.character(version)
  summary_df$analyzer_version <- have
  summary_df
}

#' How many packages the dataset scan has never reached.
#'
#' The marker lives on the latest-version row, beside latest_release_date, so
#' the question is scoped the same way .recollect_todo scopes the backfill it
#' feeds: a package counts when its latest row has no marker.
#'
#' Deliberately NOT filtered by permanent failures or by the current universe,
#' unlike the to-do pool. Those are exactly the packages that will never be
#' scanned and so never appear in a queue, which is what makes them invisible:
#' bootstrap_complete goes true and stays true with them still unscanned. This
#' is the number that says how many.
#'
#' @return Package count. Zero when there is nothing to measure yet; every
#'   package when the marker column does not exist, because before the first
#'   write that carries it nothing has been scanned.
.n_datasets_unscanned <- function(con) {
  if (!"cran_code_summary" %in% DBI::dbListTables(con)) return(0L)
  fields <- DBI::dbListFields(con, "cran_code_summary")
  if (!"latest_release_date" %in% fields) return(0L)
  sql <- if ("datasets_scanned" %in% fields) {
    "SELECT COUNT(DISTINCT package) n FROM cran_code_summary
      WHERE latest_release_date IS NOT NULL AND datasets_scanned IS NULL"
  } else {
    "SELECT COUNT(DISTINCT package) n FROM cran_code_summary
      WHERE latest_release_date IS NOT NULL"
  }
  as.integer(DBI::dbGetQuery(con, sql)$n %||% 0L)
}

#' Packages needing a metrics backfill: those with a stored row where the
#' sentinel column is NULL, or every stored package when that column has not been
#' added yet. Restricted to the current universe and excluding the packages the
#' caller names.
#'
#' @param perm_fail_pkgs Packages to leave out whatever their sentinel says.
#'   Permanent clone failures on every call, and on the dataset queue the
#'   packages this analyzer build has already asked for and could not read:
#'   both are packages a queue has no way of finishing.
#' @param latest_only When FALSE (default), a package is flagged if ANY of its
#'   rows has a NULL sentinel. Correct for a per-version sentinel like n_fns_r.
#'   When TRUE, the NULL check is confined to the package's latest-version row
#'   (the row carrying a non-NULL latest_release_date). This is required for a
#'   marker written only on the latest row (e.g. detail_scanned): checking any
#'   row would re-flag every multi-version package forever, so the backfill would
#'   never converge. Packages with no latest_release_date row are not flagged.
.recollect_todo <- function(con, universe_pkgs, perm_fail_pkgs,
                            sentinel = "n_fns_r", table = "cran_code_summary",
                            latest_only = FALSE) {
  if (!table %in% DBI::dbListTables(con)) return(character(0L))
  fields <- DBI::dbListFields(con, table)
  pkgs <- if (isTRUE(latest_only)) {
    if (!"latest_release_date" %in% fields) {
      character(0L)
    } else if (!sentinel %in% fields) {
      DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT package FROM %s WHERE latest_release_date IS NOT NULL",
        table))[["package"]]
    } else {
      DBI::dbGetQuery(con, sprintf(
        'SELECT DISTINCT package FROM %s
         WHERE latest_release_date IS NOT NULL AND "%s" IS NULL',
        table, sentinel))[["package"]]
    }
  } else if (!sentinel %in% fields) {
    DBI::dbGetQuery(con, sprintf("SELECT DISTINCT package FROM %s", table))[["package"]]
  } else {
    DBI::dbGetQuery(con, sprintf(
      'SELECT DISTINCT package FROM %s WHERE "%s" IS NULL', table, sentinel
    ))[["package"]]
  }
  pkgs <- pkgs[!pkgs %in% perm_fail_pkgs]
  pkgs <- pkgs[pkgs %in% as.character(universe_pkgs)]
  sort(as.character(pkgs))
}

# ---------------------------------------------------------------------------
# default_io
# ---------------------------------------------------------------------------

#' Build the default production IO interface.
#'
#' @return A list with:
#'   \item{package_list}{function() -> data.frame(package, latest_version)}
#'   \item{clone}{function(pkg, dest) -> logical}
default_io <- function() {
  list(
    package_list = function() {
      # Live packages from CRAN.
      live_df <- tryCatch({
        m <- available.packages(repos = "https://cloud.r-project.org")
        data.frame(
          package        = as.character(m[, "Package"]),
          latest_version = as.character(m[, "Version"]),
          stringsAsFactors = FALSE,
          row.names        = NULL
        )
      }, error = function(e) {
        warning(sprintf("Could not fetch live CRAN package list: %s",
                        conditionMessage(e)))
        data.frame(package = character(0L), latest_version = character(0L),
                   stringsAsFactors = FALSE)
      })

      # Archived packages (not in live_df); latest_version = NA.
      arch_df <- tryCatch({
        arch      <- readRDS(url("https://cran.r-project.org/src/contrib/Meta/archive.rds"))
        arch_pkgs <- names(arch)
        arch_pkgs <- arch_pkgs[!arch_pkgs %in% live_df$package]
        if (length(arch_pkgs) == 0L) {
          data.frame(package = character(0L), latest_version = character(0L),
                     stringsAsFactors = FALSE)
        } else {
          data.frame(
            package        = arch_pkgs,
            latest_version = NA_character_,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        warning(sprintf("Could not fetch CRAN archive index: %s",
                        conditionMessage(e)))
        data.frame(package = character(0L), latest_version = character(0L),
                   stringsAsFactors = FALSE)
      })

      combined <- rbind(live_df, arch_df)
      combined[order(combined$package), ]
    },

    clone = function(pkg, dest) {
      clone_package(pkg, dest, token = Sys.getenv("GITHUB_TOKEN", ""))
    }
  )
}

# ---------------------------------------------------------------------------
# Worker telemetry: each package's analyzer directory and phase times
# ---------------------------------------------------------------------------

# Whether RPA_CACHE turns the analyzer's cache off; every spelling of "no" counts.
.analyzer_cache_off <- function(value = Sys.getenv("RPA_CACHE", unset = "")) {
  tolower(trimws(value)) %in% c("off", "false", "no", "0")
}

# Put environment variables back as Sys.getenv(names, unset = NA) read them.
.restore_envvars <- function(old) {
  for (nm in names(old)) {
    if (is.na(old[[nm]])) Sys.unsetenv(nm)
    else do.call(Sys.setenv, stats::setNames(list(old[[nm]]), nm))
  }
  invisible(NULL)
}

# f, adding its elapsed seconds to the worker tally under `name`; f's value,
# attributes included, is unchanged.
.timed_phase <- function(name, f) {
  force(f)
  function(...) {
    t0 <- proc.time()[["elapsed"]]
    on.exit(.tally_add(name, .secs_since(t0)), add = TRUE)
    f(...)
  }
}

# worker, run in its own analyzer directory WORK_DIR/.rpa/<pkg> (no clone can land
# there) with RPKG_ANALYZER_STATS set and, unless RPA_CACHE is off, a cache dir.
# A list result gains the worker tally and the statistics lines.
.with_worker_telemetry <- function(worker) {
  force(worker)
  function(pkg) {
    t0 <- proc.time()[["elapsed"]]
    .tally_reset()
    rpa   <- file.path(WORK_DIR, ".rpa", pkg)
    stats <- file.path(rpa, "stats.ndjson")
    old   <- Sys.getenv(c("RPKG_ANALYZER_CACHE_DIR", "RPKG_ANALYZER_STATS"),
                        unset = NA_character_, names = TRUE)
    on.exit({
      .restore_envvars(old)
      unlink(rpa, recursive = TRUE, force = TRUE)
    }, add = TRUE)
    Sys.unsetenv(c("RPKG_ANALYZER_CACHE_DIR", "RPKG_ANALYZER_STATS"))
    dir.create(file.path(rpa, "cache"), recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(rpa)) {
      rpa   <- normalizePath(rpa)
      stats <- file.path(rpa, "stats.ndjson")
      Sys.setenv(RPKG_ANALYZER_STATS = stats)
      if (!.analyzer_cache_off()) Sys.setenv(RPKG_ANALYZER_CACHE_DIR = file.path(rpa, "cache"))
    }
    res <- worker(pkg)
    .tally_add("package_s", .secs_since(t0))
    if (is.list(res)) {
      res$tally <- .tally_snapshot()
      res$analyzer_stats <- if (file.exists(stats)) readLines(stats, warn = FALSE) else character(0L)
    }
    res
  }
}

# How many of a shard's largest analyzer peaks run-status.json keeps.
ANALYZER_PEAKS_KEPT <- 5L

# The analyzer's statistics lines (RPKG_ANALYZER_STATS, 0.5.1 and later) summed
# over a shard; a line that does not parse is counted as unreadable. `packages`
# names each line's package, for the memory figures; an absent or null key is NA.
.sum_analyzer_stats <- function(lines, packages = NULL) {
  out <- list(runs = 0L, unreadable = 0L, builds = "",
              ms = 0, ms_compiled = 0, ms_r = 0, ms_tests = 0, ms_data = 0, ms_other = 0,
              compiled_files = 0, compiled_hits = 0, r_files = 0, tests_files = 0,
              data_files = 0, cache_errors = 0, verify_mismatch = 0,
              peak_rss_kb = NA_real_, peak_rss_package = NA_character_,
              peak_vm_kb = NA_real_, peak_vm_package = NA_character_,
              data_kept_max = NA_real_, data_over_budget = I(character(0L)),
              peaks = list())
  num <- function(x) if (is.numeric(x) && length(x) == 1L && !is.na(x)) x else 0
  fig <- function(x) if (is.numeric(x) && length(x) == 1L && !is.na(x)) as.numeric(x) else NA_real_
  packages <- as.character(packages %||% rep(NA_character_, length(lines)))
  builds <- character(0L)
  rss <- vm <- kept <- numeric(0L)
  over <- character(0L)
  for (i in seq_along(lines)) {
    s <- tryCatch(jsonlite::parse_json(lines[[i]]), error = function(e) NULL)
    if (!is.list(s) || !is.numeric(s$ms)) {
      out$unreadable <- out$unreadable + 1L
      next
    }
    out$runs <- out$runs + 1L
    if (is.character(s$build) && length(s$build) == 1L) builds <- c(builds, s$build)
    for (k in c("ms", "ms_compiled", "ms_r", "ms_tests", "ms_data", "ms_other",
                "cache_errors", "verify_mismatch")) {
      out[[k]] <- out[[k]] + num(s[[k]])
    }
    for (kind in c("compiled", "r", "tests", "data")) {
      out[[paste0(kind, "_files")]] <- out[[paste0(kind, "_files")]] + num(s[[kind]]$files)
    }
    out$compiled_hits <- out$compiled_hits + num(s$compiled$hits)
    pkg <- packages[[i]]
    rss <- c(rss, stats::setNames(fig(s$peak_rss_kb), pkg))
    vm  <- c(vm,  stats::setNames(fig(s$peak_vm_kb), pkg))
    kept <- c(kept, fig(s$data_kept_max))
    if (num(s$data_over_budget) > 0 && !is.na(pkg)) over <- c(over, pkg)
  }
  out$builds <- paste(sort(unique(builds)), collapse = " ")
  if (any(!is.na(kept))) out$data_kept_max <- max(kept, na.rm = TRUE)
  if (any(!is.na(rss))) {
    out$peak_rss_kb      <- max(rss, na.rm = TRUE)
    out$peak_rss_package <- names(rss)[[which.max(rss)]]
  }
  if (any(!is.na(vm))) {
    out$peak_vm_kb      <- max(vm, na.rm = TRUE)
    out$peak_vm_package <- names(vm)[[which.max(vm)]]
  }
  out$data_over_budget <- I(sort(unique(over)))
  out$peaks <- .analyzer_peaks(rss, vm)
  out
}

# Each package's largest resident and virtual peak over its versions, largest
# resident first and ties by name. `rss` and `vm` are named by package.
.analyzer_peaks <- function(rss, vm, keep = ANALYZER_PEAKS_KEPT) {
  has <- !is.na(rss) & !is.na(names(rss))
  if (!any(has)) return(list())
  top <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  by_rss <- tapply(rss[has], names(rss)[has], top)
  by_vm  <- tapply(vm[has],  names(rss)[has], top)
  pkgs   <- names(by_rss)[order(-by_rss, names(by_rss))]
  lapply(utils::head(pkgs, keep), function(p) {
    list(package = p, peak_rss_kb = by_rss[[p]], peak_vm_kb = by_vm[[p]])
  })
}

# A shard's analyzer statistics and worker time by phase, from the worker
# results; a fork that crashed returned no list and adds nothing.
.shard_telemetry <- function(results) {
  rs <- Filter(is.list, results)
  tally <- function(name) {
    sum(vapply(rs, function(r) as.numeric(r$tally[[name]] %||% 0), numeric(1L)))
  }
  lines <- lapply(rs, function(r) as.character(r$analyzer_stats))
  analyzer <- .sum_analyzer_stats(
    unlist(lines, use.names = FALSE),
    packages = rep(vapply(rs, function(r) as.character(r$package %||% NA_character_),
                          character(1L)), lengths(lines)))
  analyzer$incomplete_parses <- as.integer(tally("incomplete_parses"))
  versions <- tally("versions_s")
  phases <- list(
    packages   = length(rs),
    clone_s    = tally("clone_s"),
    extract_s  = tally("extract_s"),
    analyzer_s = tally("analyzer_s"),
    parse_s    = tally("parse_s"),
    metrics_s  = max(0, versions - tally("extract_s") - tally("analyzer_s") - tally("parse_s")),
    other_s    = max(0, tally("package_s") - tally("clone_s") - versions),
    dataset_memo_hits   = as.integer(tally("memo_hits")),
    dataset_memo_misses = as.integer(tally("memo_misses")))
  list(analyzer = analyzer, phases = phases)
}

# The shard's analyzer line for the run log.
.analyzer_stats_line <- function(a) {
  if (a$runs == 0L) {
    return(sprintf("analyzer: no statistics from this build; incomplete parses %d",
                   a$incomplete_parses))
  }
  sprintf(paste0("analyzer: %d versions in %.0f s; compiled %.0f files (%.1f%% reused); ",
                 "cache errors %.0f; verify mismatches %.0f; incomplete parses %d"),
          a$runs, a$ms / 1000, a$compiled_files,
          if (a$compiled_files > 0) 100 * a$compiled_hits / a$compiled_files else 0,
          a$cache_errors, a$verify_mismatch, a$incomplete_parses)
}

# kB as MiB, to one decimal.
.kb_mib <- function(kb) sprintf("%.1f MiB", kb / 1024)

# The shard's analyzer memory line for the run log, from .sum_analyzer_stats.
# The peaks come from Linux alone; the data figures only from a build that
# counts what its data files keep.
.analyzer_memory_line <- function(a, max_names = 20L) {
  peaks <- c(
    if (!is.na(a$peak_rss_kb))
      sprintf("peak resident %s (%s)", .kb_mib(a$peak_rss_kb), a$peak_rss_package),
    if (!is.na(a$peak_vm_kb))
      sprintf("peak virtual %s (%s)", .kb_mib(a$peak_vm_kb), a$peak_vm_package))
  over <- as.character(a$data_over_budget)
  data <- if (!is.na(a$data_kept_max) || length(over)) {
    shown <- utils::head(over, max_names)
    c(if (!is.na(a$data_kept_max))
        sprintf("largest data kept %s", .kb_mib(a$data_kept_max / 1024)),
      paste0("over the data budget: ",
             if (length(over)) paste(shown, collapse = " ") else "none",
             if (length(over) > length(shown))
               sprintf(" and %d more", length(over) - length(shown)) else ""))
  }
  if (is.null(peaks) && is.null(data)) {
    return("analyzer memory: no memory figures from this build")
  }
  paste0("analyzer memory: ",
         paste(c(if (length(peaks)) peaks else "no peak figures on this platform", data),
               collapse = ", "))
}

# x with every single NA as a logical NA, which write_manifest writes as null;
# a number that is NA would be written as the string "NA".
.na_as_null <- function(x) {
  if (is.list(x) && !inherits(x, "AsIs")) return(lapply(x, .na_as_null))
  if (length(x) == 1L && is.na(x)) NA else x
}

# The shard's worker time by phase for the run log.
.worker_phase_line <- function(p) {
  sprintf(paste0("worker time: clone %.1f s, extract %.1f s, analyzer %.1f s, ",
                 "record parse %.1f s, metrics %.1f s, other %.1f s"),
          p$clone_s, p$extract_s, p$analyzer_s, p$parse_s, p$metrics_s, p$other_s)
}

# ---------------------------------------------------------------------------
# run_update
# ---------------------------------------------------------------------------

#' Run one sharded update of the cran-code-metrics pipeline.
#'
#' Opens (or creates) the code DB at out_dir/DB_FILENAME and the dataset DB at
#' out_dir/DATA_DB_FILENAME, determines which packages need analysis by querying
#' the DB (not by reading whole tables), processes the next shard, upserts only
#' the shard's rows in-place (bounded to O(shard) memory) with dataset rows
#' written before the code summary, and emits code-manifest.json,
#' data-manifest.json, run-status.json, and the changed-packages.txt
#' accumulator.
#'
#' Clone and analyze failures are tracked per package with their stage. A
#' package whose verdict parks it (.permanent_failures) is left out of the
#' to-do list and counted in the manifest permanent_failures field until the
#' analyzer build, its release or WORKER_TIMEOUT changes, or an operator
#' releases it.
#'
#' @param io         IO interface: list with $package_list() and $clone().
#'   Use default_io() for production; inject a fake for tests.
#' @param out_dir    Directory to read prior DB from and write outputs to.
#' @param shard_size Maximum packages to analyze in this run.
#'   Defaults to SHARD_SIZE from config.R.
#' @param force_full When TRUE, wipes all existing metric rows and re-analyzes
#'   all packages (excluding permanent failures) from scratch.
#' @param recollect When TRUE, re-analyzes only packages whose stored rows
#'   predate the binary metrics (a sentinel column is NULL). Nothing is wiped:
#'   rows are upserted in place, so the served DB stays complete throughout.
#'   Not filtered by the analyzer read attempts, unlike the scheduled path: an
#'   operator asking for a backfill by name is asking for the packages the
#'   scheduled run has given up on as well.
#' @param unpark  --unpark: verdicts to release before any queue is read (see
#'   .unpark). NULL releases nothing.
#' @param requeue --requeue: packages to analyse again from scratch (see
#'   .requeue). NULL requeues nothing.
#' @return Manifest list (invisibly).
run_update <- function(io, out_dir, shard_size = SHARD_SIZE, force_full = FALSE,
                       recollect = FALSE, unpark = NULL, requeue = NULL) {
  # Without the analyzer binary the run still completes and still writes rows,
  # and it makes no progress: the per-package detail sentinel is never
  # populated, so every package analysed stays in the backfill pool and the next
  # run selects the same shard again. The bootstrap never advances and nothing
  # in the output says so, which is a silent stall rather than a failure.
  if (!nzchar(rpkg_analyzer_bin())) {
    warning("rpkg-analyzer not found: per-package detail will not be written, ",
            "the backfill pool will not drain, and the shard will not advance ",
            "between runs. Set RPKG_ANALYZER_BIN or install the binary.",
            call. = FALSE, immediate. = TRUE)
  }

  # Read once, and use the same answer for both halves of the re-scan queue:
  # the build the stored rows are compared against, and the build stamped on
  # the rows this shard writes. Asking twice would let a binary swapped
  # mid-run clear markers it then never restores.
  analyzer_version <- rpkg_analyzer_version()
  # A build that rejected the flag would exit 2 on every package: a crash for
  # each one with analyzer rows and the R fallback for the rest. So a 0.5.0
  # build proves it reads the flag first.
  if (analyzer_at_least(analyzer_version, "0.5.0") &&
      !rpkg_analyzer_selfcheck(ANALYZER_INPUT_KIND)) {
    stop(sprintf(paste0(
      "rpkg-analyzer %s did not answer --input-kind %s with a summary naming it; ",
      "stopping before any shard"), analyzer_version, ANALYZER_INPUT_KIND),
      call. = FALSE)
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  db_path      <- file.path(out_dir, DB_FILENAME)
  data_db_path <- file.path(out_dir, DATA_DB_FILENAME)

  # ---- 1. Open both DBs (creates tables if absent) --------------------------
  con <- open_or_init_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data_con <- open_or_init_data_db(data_db_path)
  on.exit(DBI::dbDisconnect(data_con), add = TRUE)
  text_db_path <- file.path(out_dir, RELEASE_TEXT_DB_FILENAME)
  shared_text  <- identical(RELEASE_TEXT_DB_FILENAME, DB_FILENAME)
  text_con <- open_or_init_release_text_db(text_db_path,
                                           con = if (shared_text) con else NULL)
  if (!shared_text) on.exit(DBI::dbDisconnect(text_con), add = TRUE)

  # ---- 1b. Operator releases, before any queue is read -----------------------
  n_released <- .unpark(con, unpark)
  if (n_released > 0L) message(sprintf("verdicts released by --unpark: %d", n_released))
  requeued <- .requeue(con, requeue)
  if (length(requeued) > 0L) {
    message(sprintf("requeued %d packages: %s", length(requeued),
                    paste(requeued, collapse = ", ")))
  }
  n_released <- n_released + length(requeued)

  # ---- 2. Analyzed state (O(n_packages) query, not full table read) ---------
  if (isTRUE(force_full)) {
    # Wipe all metric rows so everything is treated as unseen.
    tables <- DBI::dbListTables(con)
    for (tbl in c("cran_code_summary", "cran_code_churn", "cran_api_history",
                  VERSION_STATE_TABLE)) {
      if (tbl %in% tables) DBI::dbExecute(con, sprintf("DELETE FROM %s", tbl))
    }
    analyzed <- character(0L)
  } else {
    # Before the queues are read, so a gap in the text history is re-read now.
    .reconcile_release_text(con, text_con)
    analyzed_df <- db_analyzed_state(con)
    analyzed <- if (nrow(analyzed_df) > 0L) {
      setNames(as.character(analyzed_df$version),
               as.character(analyzed_df$package))
    } else {
      character(0L)
    }
  }

  # ---- 3. Universe ----------------------------------------------------------
  universe <- io$package_list()
  if (!is.data.frame(universe) || nrow(universe) == 0L) {
    universe <- data.frame(package = character(0L), latest_version = character(0L),
                           stringsAsFactors = FALSE)
  }
  n_universe <- nrow(universe)

  # ---- 4. Permanent failures: exclude from to-do ----------------------------
  verdicts       <- .verdict_state(con, analyzer_version, WORKER_TIMEOUT, universe)
  perm_fail_pkgs <- verdicts$package[verdicts$parked]
  recheck_pkgs   <- verdicts$package[verdicts$recheck_due]
  run_id <- .current_run_id()
  lv_of  <- stats::setNames(as.character(universe$latest_version),
                            as.character(universe$package))
  # A package that failed earlier in this run waits for the next one, at every
  # stage, so a failing package costs one attempt and one publish per run.
  tried_pkgs <- setdiff(.tried_this_run(con, run_id), perm_fail_pkgs)
  skip_pkgs  <- c(perm_fail_pkgs, tried_pkgs)

  # Which builds count as this one, and how many latest rows they wrote.
  output_class <- .analyzer_output_class(analyzer_version)
  on_class     <- .n_latest_on_class(con, analyzer_version)
  message(sprintf("analyzer %s, output class %s; latest rows on class: %d of %d",
                  analyzer_version %||% "none",
                  if (length(output_class)) paste(output_class, collapse = " ") else "none",
                  on_class[["on_class"]], on_class[["latest"]]))

  # ---- 5. To-do: packages that need analysis --------------------------------
  if (isTRUE(force_full)) {
    todo_pkgs <- sort(as.character(
      universe$package[!universe$package %in% skip_pkgs]
    ))
  } else if (isTRUE(recollect)) {
    # Backfill: only packages whose rows predate the binary metrics. No wipe;
    # upsert_shard replaces each package's rows in place.
    todo_pkgs <- .recollect_todo(con, universe$package, skip_pkgs)
  } else {
    is_todo <- vapply(seq_len(n_universe), function(i) {
      pkg <- as.character(universe$package[i])
      if (pkg %in% skip_pkgs) return(FALSE)  # parked, or failed this run
      lv  <- universe$latest_version[i]
      if (!pkg %in% names(analyzed)) return(TRUE)   # never analyzed
      stored_v <- analyzed[[pkg]]
      # Archived packages (NA latest_version): skip once analyzed.
      if (is.na(lv)) return(FALSE)
      # New release detected: CRAN version differs from what is stored.
      !identical(as.character(lv), as.character(stored_v))
    }, logical(1L))
    changed <- as.character(universe$package[is_todo])
    # An analyzer upgrade changes what a scan finds, so rows produced by an older
    # build are stale even though they are marked scanned. Clearing the marker on
    # those puts them back in the queue below, which drains a shard at a time and
    # settles once every row carries the running build's version.
    n_stale <- .invalidate_stale_dataset_scans(con, analyzer_version)
    message(sprintf("dataset scans invalidated by analyzer change: %d", n_stale))
    # The same change gives back the packages the previous build could not read.
    # Their rows carry no marker to invalidate, so this is the only thing that
    # puts them in front of a new reader. Before the queues are read, so this
    # run is the one that asks again.
    n_retry <- .forget_other_builds_read_attempts(con, analyzer_version)
    message(sprintf("packages to re-read under this analyzer: %d", n_retry))
    # The packages this build has already been given MAX_ANALYZER_READ_ATTEMPTS
    # times and did not read. Both backfill queues below wait on fields only the
    # binary produces, so both would hand these back every run for good. They
    # are excluded the same way and in the same place a package that cannot be
    # cloned is, because it is the same problem: a package with no way out of a
    # queue keeps every run reporting a change. The changed-version path above
    # is deliberately not filtered, because a new release is a new question and
    # answering it clears the record.
    unread_pkgs <- .analyzer_read_exhausted(con)
    # Also drain any packages whose rows predate the binary metrics, so a normal
    # scheduled run finishes the one-time backfill and then reverts to just the
    # changed packages once none remain.
    backfill <- .recollect_todo(con, universe$package,
                                c(skip_pkgs, unread_pkgs))
    # And drain any packages whose latest-version row was stored before the
    # per-function/per-edge detail scan (detail_scanned IS NULL on that row).
    # Latest-row-scoped so it converges: a package re-analyzed once is marked and
    # never re-flagged, even if it produced zero functions. Not filtered by the
    # read attempts: this marker is written by the run itself under either
    # producer, so the queue drains without the analyzer.
    detail_backfill <- .recollect_todo(con, universe$package, skip_pkgs,
                                        sentinel = "detail_scanned",
                                        latest_only = TRUE)
    # And drain any package whose latest-version row predates the dataset reader
    # (datasets_scanned IS NULL), so cran_datasets fills in without a manual
    # recollect. Also latest-row-scoped, so it converges once re-analyzed.
    dataset_backfill <- .recollect_todo(
      con, universe$package, c(skip_pkgs, unread_pkgs),
      sentinel = "datasets_scanned", latest_only = TRUE)
    todo_pkgs <- sort(unique(c(changed, backfill, detail_backfill, dataset_backfill)))
  }

  # Take the first shard_size packages from the to-do list (deterministic order).
  shard_pkgs <- if (length(todo_pkgs) > shard_size) {
    todo_pkgs[seq_len(shard_size)]
  } else {
    todo_pkgs
  }

  # ---- 5b. Shard plan + wall-clock start ------------------------------------
  # One line printed BEFORE the blocking parallel analyze so the operator sees
  # the shard's size, the to-do pool composition, and resources at the top of the
  # gap. changed/backfill/detail_backfill are the raw (overlapping) to-do pools
  # and exist only on the scheduled path; guard with exists() so --bootstrap and
  # --recollect runs still print (they show 0/0/0).
  t_shard0   <- Sys.time()
  n_changed  <- if (exists("changed",         inherits = FALSE)) length(changed)         else 0L
  n_backfill <- if (exists("backfill",        inherits = FALSE)) length(backfill)        else 0L
  n_detail   <- if (exists("detail_backfill", inherits = FALSE)) length(detail_backfill) else 0L
  cat(sprintf(
    "shard plan: %d pkgs this shard; to-do pool %d (changed %d / backfill %d / detail %d, overlapping), %d will remain; %d cores, %ds/pkg timeout\n",
    length(shard_pkgs), length(todo_pkgs), n_changed, n_backfill, n_detail,
    length(todo_pkgs) - length(shard_pkgs), ANALYSIS_CORES, WORKER_TIMEOUT),
    file = stdout())
  cat(.verdict_plan_line(analyzer_version, n_released, verdicts, length(tried_pkgs)),
      file = stdout())
  flush(stdout())

  # ---- 6. Analyze the shard (parallel) -------------------------------------
  shard_summary_list   <- list()
  shard_churn_list     <- list()
  shard_api_list       <- list()
  shard_functions_list <- list()
  shard_edges_list     <- list()
  shard_datasets_list  <- list()
  shard_vignettes_list <- list()
  shard_text_list      <- list()
  shard_state_list     <- list()
  shard_failures       <- character(0L)
  shard_stages         <- character(0L)
  # Verdicts this shard wrote. Each is news the next run needs, so the shard
  # publishes; a weekly recheck that failed where it failed before is not.
  n_verdicts_written   <- 0L
  shard_over_cap       <- character(0L)
  # Which of the rows about to be written the analyzer binary produced, keyed
  # by package and version. Only those get the running build stamped on them.
  shard_binary_keys    <- character(0L)

  if (!dir.exists(WORK_DIR)) dir.create(WORK_DIR, recursive = TRUE)

  # Each clone's seconds go to the worker tally; its value and status do not change.
  io$clone <- .timed_phase("clone_s", io$clone)

  # Read before the fork: a worker cannot ask which versions have analyzer rows.
  stamped_of <- .stamped_versions(con, shard_pkgs)

  # Worker: clone + analyze one package. No database access.
  # Returns list(package, ok = TRUE, elapsed, summary, churn, ...) or, when the
  # package failed, list(package, ok = FALSE, stage, elapsed, reason).
  .pkg_worker <- function(pkg) {
    .t0  <- Sys.time()
    .idx <- match(pkg, shard_pkgs)          # queue position; shard_pkgs is unique
    .n   <- length(shard_pkgs)
    .elapsed <- function() as.numeric(difftime(Sys.time(), .t0, units = "secs"))
    # Thinned per-worker completion line, emitted FROM the fork so it streams live
    # during the otherwise-silent parallel phase. Prints only on every 25th queue
    # position, every failure, every slow (>=30s) package, every package past
    # the cap, every package whose analyzer exited non-zero, and the last position.
    # One fully-formed cat() to stdout: forks reorder whole lines but never
    # byte-interleave, and fd 1 is disjoint from mclapply's result pipe. Staying
    # under PIPE_BUF is what makes that true, and .worker_line is where it is
    # enforced, because the line now carries a condition message. The emit is
    # wrapped in try() so a broken-stream write can never turn an ok package
    # into a recorded failure.
    .done <- function(ok, stage, nver, el, reason = NULL) {
      exits <- .analyzer_exit_text(.tally_snapshot())
      if (isTRUE(ok) && .idx %% 25L != 0L && el < 30 && el < WORKER_TIMEOUT &&
          !identical(.idx, .n) && !nzchar(exits)) {
        return(invisible())
      }
      try({
        cat(.worker_line(.idx, .n, ok, pkg, stage, nver, el, reason,
                         analyzer_exit = exits),
            file = stdout())
        flush(stdout())
      }, silent = TRUE)
      invisible()
    }
    # The reason rides out on the line the failure prints and in the result, so
    # the parent can store it; redacted first, since the clone URL holds a token.
    .fail <- function(stage, reason) {
      el     <- .elapsed()
      reason <- .redact_reason(reason)
      .done(FALSE, stage, 0L, el, reason)
      list(package = pkg, ok = FALSE, stage = stage, elapsed = el, reason = reason)
    }
    dest <- file.path(WORK_DIR, pkg)
    on.exit(unlink(dest, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(setTimeLimit(), add = TRUE)
    setTimeLimit(elapsed = WORKER_TIMEOUT, transient = TRUE)
    ok <- tryCatch(io$clone(pkg, dest),
                   error = function(e) structure(FALSE, reason = conditionMessage(e)))
    if (!isTRUE(ok)) return(.fail(.clone_stage(ok), .clone_reason(ok)))
    err <- NULL
    res <- tryCatch(
      analyze_package(dest, pkg, stamped = stamped_of[[pkg]] %||% character(0L)),
      error = function(e) {
        err <<- e
        NULL
      }
    )
    if (is.null(res)) {
      return(.fail(.classify_failure(err, .elapsed()),
                   if (is.null(err)) "analyze_package returned nothing"
                   else conditionMessage(err)))
    }
    el <- .elapsed()
    .done(TRUE, "ok", nrow(res$summary), el)
    list(package = pkg, ok = TRUE, elapsed = el,
         summary = res$summary, churn = res$churn, api = res$api,
         functions = res$functions, edges = res$edges, datasets = res$datasets,
         vignettes = res$vignettes, text = res$text,
         binary_versions = res$binary_versions,
         state = .with_read_at(res$state))
  }

  results <- parallel::mclapply(shard_pkgs, .with_worker_telemetry(.pkg_worker),
                                mc.cores       = ANALYSIS_CORES,
                                mc.preschedule = FALSE)

  # Collect results in input order (shard_pkgs is sorted, so DB is deterministic).
  # All DB writes happen here in the parent process.
  for (i in seq_along(results)) {
    r   <- results[[i]]
    pkg <- shard_pkgs[[i]]
    # mclapply returns NULL for a fork that died and a try-error for one that
    # raised outside the worker's handlers; neither printed its line.
    v <- .classify_result(r)
    if (!isTRUE(v$ok)) {
      if (isTRUE(v$from_parent)) {
        cat(.worker_line(i, length(shard_pkgs), FALSE, pkg, v$stage, 0L,
                         v$elapsed, v$reason), file = stdout())
        flush(stdout())
      }
      shard_failures <- c(shard_failures, pkg)
      shard_stages   <- c(shard_stages, v$stage)
      same_recheck <- pkg %in% recheck_pkgs &&
        identical(verdicts$stage[match(pkg, verdicts$package)], v$stage)
      if (!same_recheck) n_verdicts_written <- n_verdicts_written + 1L
      .record_failure(con, pkg, v$stage, analyzer_version, WORKER_TIMEOUT, run_id,
                      v$elapsed, v$reason, unname(lv_of[pkg]))
    } else {
      shard_summary_list[[pkg]]   <- r$summary
      shard_churn_list[[pkg]]     <- r$churn
      shard_api_list[[pkg]]       <- r$api
      shard_functions_list[[pkg]] <- r$functions
      shard_edges_list[[pkg]]     <- r$edges
      shard_datasets_list[[pkg]]  <- r$datasets
      shard_vignettes_list[[pkg]] <- r$vignettes
      shard_text_list[[pkg]]      <- r$text
      shard_state_list[[pkg]]     <- r$state
      shard_binary_keys <- c(shard_binary_keys,
                             .analyzer_row_keys(pkg, r$binary_versions))
      .reset_failure(con, pkg)
      if (.note_over_cap(con, pkg, v$elapsed, analyzer_version, run_id)) {
        shard_over_cap <- c(shard_over_cap, pkg)
      }
      # Analysed, but which versions were read? A version the analyzer did not
      # read carries none of the fields the backfill queues wait on, and that
      # attempt is what eventually takes its package out of them. Every version
      # this run did read starts over from nothing, so one bad run does not
      # count against the next, and so does one this package no longer has.
      unread <- .analyzer_unread_versions(r$summary, r$binary_versions)
      .clear_analyzer_read_attempts(con, pkg, keep = unread)
      for (v in unread) .record_analyzer_read_attempt(con, pkg, v, analyzer_version)
    }
  }

  # ---- 7. Upsert shard into DB in-place (O(shard) memory) ------------------
  fresh_pkgs      <- names(shard_summary_list)
  fresh_summary   <- .stamp_analyzer_version(
    .rbind_union_all(shard_summary_list) %||% .empty_summary(),
    analyzer_version, shard_binary_keys)
  fresh_churn     <- .rbind_union_all(shard_churn_list)     %||% .empty_churn()
  fresh_api       <- .rbind_union_all(shard_api_list)       %||% .empty_api()
  fresh_functions <- .rbind_union_all(shard_functions_list) %||% .empty_functions_df()
  fresh_edges     <- .rbind_union_all(shard_edges_list)     %||% .empty_edges_df()
  fresh_datasets  <- .rbind_union_all(shard_datasets_list)  %||% .empty_datasets_df()
  fresh_vignettes <- .rbind_union_all(shard_vignettes_list) %||% .empty_vignettes_rows()
  fresh_text      <- .bind_release_text(shard_text_list)
  fresh_state     <- .rbind_union_all(shard_state_list)     %||% .empty_version_state()

  if (length(fresh_pkgs) > 0L) {
    # Write dataset rows before the code summary stamps datasets_scanned = TRUE,
    # so a scanned code row always implies its dataset rows were written. The
    # dataset write is delete-then-insert (idempotent), so if the code write
    # fails afterwards the package stays on the to-do list and the next run
    # redoes both cleanly, rather than being marked done with datasets missing.
    upsert_datasets(data_con, fresh_datasets, fresh_pkgs)
    # Before the code rows, so a failed text write leaves these packages unmarked.
    upsert_release_text(text_con, fresh_text$description,
                        fresh_text$release_notes, fresh_text$versions)
    upsert_shard(con, fresh_summary, fresh_churn, fresh_api,
                 fresh_functions, fresh_edges, fresh_vignettes,
                 description_df = fresh_text$description_latest,
                 release_notes_df = fresh_text$release_notes_latest,
                 analyzer_version = analyzer_version,
                 state_df = fresh_state)
  }

  # ---- 7b. Project archived-package metadata into the narrow lookup table ----
  # Re-shape each archived package's last-version identity fields into the
  # WITHOUT ROWID cran_archived_meta table the viewer point-looks-up. Runs every
  # shard so the table stays complete as the bootstrap accumulates archived
  # packages; it only re-reads data already in the DB (no download, no re-scan).
  archived_pkgs <- universe$package[is.na(universe$latest_version)]
  project_archived_meta(con, archived_pkgs)

  # Rebuild the author/package span table from the same already-stored rows, so
  # the viewer can say when an author actually joined a package instead of
  # quoting the package's first release. Also a pure projection: no download,
  # no re-scan.
  project_author_spans(con)

  # ---- 8. Manifest ---------------------------------------------------------
  # cran_code_summary is created lazily by upsert_shard; may not exist yet if
  # this is the first run and every package in the shard failed.
  n_analyzed_pkgs <- {
    tbls <- DBI::dbListTables(con)
    if ("cran_code_summary" %in% tbls) {
      DBI::dbGetQuery(
        con, "SELECT COUNT(DISTINCT package) AS n FROM cran_code_summary")$n %||% 0L
    } else {
      0L
    }
  }
  new_fp <- db_fingerprint(con)

  # Re-read the verdicts after this shard (some may have just parked).
  verdicts_after       <- .verdict_state(con, analyzer_version, WORKER_TIMEOUT, universe)
  n_permanent_failures <- sum(verdicts_after$parked)
  verdict_counts <- list(
    parked            = .parked_counts(verdicts_after),
    failed_this_run   = if (is.na(run_id)) length(shard_failures)
                        else length(.tried_this_run(con, run_id)),
    over_cap_ok       = .over_cap_block(con),
    over_cap_this_run = if (is.na(run_id)) length(shard_over_cap)
                        else as.integer(DBI::dbGetQuery(con,
                          "SELECT COUNT(*) n FROM cran_over_cap WHERE last_run_id = ?",
                          params = list(run_id))$n))

  prior_fp <- tryCatch({
    prev_path <- file.path(out_dir, "prev-code-manifest.json")
    cur_path  <- file.path(out_dir, "code-manifest.json")
    src <- if (file.exists(prev_path)) prev_path else if (file.exists(cur_path)) cur_path else NULL
    if (is.null(src)) NULL else jsonlite::fromJSON(src)[["fingerprint"]]
  }, error = function(e) NULL)

  # bootstrap_complete: no deferred packages remain AND DB covers the universe
  # minus permanently-failed packages.
  remaining_after    <- setdiff(todo_pkgs, shard_pkgs)
  bootstrap_complete <- length(remaining_after) == 0L &&
    n_analyzed_pkgs >= (n_universe - n_permanent_failures)

  # changed: something substantive happened OR the content hash shifted OR a
  # verdict was written, which only persists if the shard publishes.
  changed <- isTRUE(force_full) ||
    length(fresh_pkgs) > 0L ||
    !identical(prior_fp, new_fp) ||
    n_verdicts_written > 0L ||
    n_released > 0L

  manifest <- list(
    generated_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    n_universe           = n_universe,
    n_analyzed           = n_analyzed_pkgs,
    n_shard              = length(shard_pkgs),
    shard_failures       = list(
      count    = length(shard_failures),
      packages = head(shard_failures, 20L)
    ),
    permanent_failures   = n_permanent_failures,
    bootstrap_complete   = bootstrap_complete,
    fingerprint          = new_fp,
    changed              = changed,
    n_remaining          = length(remaining_after),
    n_fresh              = length(fresh_pkgs),
    n_versions           = nrow(fresh_summary)
  )

  # ---- 8a. Analyzer statistics and worker time ------------------------------
  telemetry <- .shard_telemetry(results)
  cat(.analyzer_stats_line(telemetry$analyzer), "\n",
      .analyzer_memory_line(telemetry$analyzer), "\n",
      .worker_phase_line(telemetry$phases), "\n", sep = "", file = stdout())

  # ---- 8b. Shard receipt ----------------------------------------------------
  # One-line closing summary, printed after the collection loop and before the
  # manifest is written, so the merged CI log shows what this shard accomplished.
  cat(sprintf(
    "shard done in %.0fs: %d/%d ok, %d failed; %d versions, %d functions, %d edges written; DB %d/%d; %d queued; complete=%s\n",
    as.numeric(difftime(Sys.time(), t_shard0, units = "secs")),
    length(fresh_pkgs), length(shard_pkgs), length(shard_failures),
    nrow(fresh_summary), nrow(fresh_functions), nrow(fresh_edges),
    n_analyzed_pkgs, n_universe, length(remaining_after),
    tolower(as.character(bootstrap_complete))),
    file = stdout())
  cat(.verdict_receipt_line(shard_stages, shard_over_cap,
                            verdict_counts$over_cap_ok$count), file = stdout())
  flush(stdout())

  # ---- 8c. Reclaim the space the deletes did not give back ------------------
  # Every re-scanned package is a delete followed by an insert, on both sides,
  # and SQLite keeps the pages a delete frees on the database's own free list
  # rather than returning them to the filesystem. A database that rewrites the
  # same rows for months therefore stays at its high-water mark whatever it
  # currently holds: cran-data-metrics.db was published byte-identical four
  # days running while its contents changed every one of them, and the code
  # database reached 90% of the size at which the workflow refuses to publish
  # at all.
  #
  # Only on a run that is going to publish. VACUUM rewrites the whole file, and
  # a shard with nothing to report ends the loop without uploading anything, so
  # reclaiming there would spend minutes on a database that is then thrown
  # away. It runs before the manifests are built so they describe the file that
  # is actually published, and before the retention guard so a shard that is
  # about to be refused does not pay for it either. It is allowed to decline:
  # see vacuum_db().
  if (isTRUE(changed)) {
    for (spec in list(
      list(con = con,      path = db_path,
           baseline = file.path(out_dir, "prev-code-manifest.json")),
      list(con = data_con, path = data_db_path,
           baseline = file.path(out_dir, "prev-data-manifest.json")),
      list(con = text_con, path = text_db_path,
           baseline = file.path(out_dir, "prev-text-manifest.json")))) {
      vac <- vacuum_db(spec$con, spec$path)
      if (isTRUE(vac$ran)) {
        # The retention guard reads a smaller file as history that went
        # missing. Restate the baseline by what came back, so it compares like
        # with like and still refuses a run that lost more than it reclaimed.
        credit_reclaim_to_baseline(spec$baseline, vac$reclaimed)
        cat(sprintf("reclaimed %s from %s (%s -> %s)\n",
                    format_bytes(vac$reclaimed), basename(spec$path),
                    format_bytes(vac$before), format_bytes(vac$after)),
            file = stdout())
      } else {
        cat(sprintf("left %s as it is: %s\n", basename(spec$path), vac$reason),
            file = stdout())
      }
      flush(stdout())
    }
  }

  bootstrap <- list(n_analyzed = n_analyzed_pkgs, n_universe = n_universe,
                    n_remaining = length(remaining_after),
                    bootstrap_complete = bootstrap_complete,
                    n_datasets_unscanned = .n_datasets_unscanned(con),
                    n_datasets_unreadable = .n_datasets_unreadable(con),
                    # The dataset database, not the code one: this is the only
                    # figure in the block counted per dataset rather than per
                    # package, and it is counted where the datasets are.
                    n_datasets_unmeasured = .n_datasets_unmeasured(data_con),
                    analyzer_version = analyzer_version,
                    output_class = I(output_class),
                    n_latest_on_build = .n_latest_on_class(con, analyzer_version)[["on_class"]],
                    parked = verdict_counts$parked,
                    failed_this_run = verdict_counts$failed_this_run,
                    over_cap_ok = verdict_counts$over_cap_ok,
                    over_cap_this_run = verdict_counts$over_cap_this_run)
  code_db_bytes <- as.numeric(file.info(db_path)$size %||% 0)
  data_db_bytes <- as.numeric(file.info(data_db_path)$size %||% 0)
  text_db_bytes <- as.numeric(file.info(text_db_path)$size %||% 0)

  # ---- 8d. What the dataset columns actually hold ---------------------------
  # A declared column that is NULL for every row in the corpus is not an honest
  # NA, it is a column nobody is filling, and it reads to a viewer exactly like
  # a fact that happens to be unknown. That went unnoticed for a year across a
  # hundred columns at once. Said in the run output, so the shard that produced
  # it says so, and counted in the manifest, so the finding outlives the log.
  #
  # Only the dataset side has this to report: the code summary has its own
  # coverage table, written beside the rows it describes.
  dataset_coverage <- dataset_column_coverage(data_con)
  dataset_alerts   <- dataset_coverage_alerts(dataset_coverage)
  if (length(dataset_alerts) > 0L) {
    shown <- head(dataset_alerts, 20L)
    cat(sprintf("dataset coverage: %d of %d declared columns hold nothing for anybody\n  %s\n%s",
                length(dataset_alerts), nrow(dataset_coverage),
                paste(shown, collapse = "\n  "),
                if (length(dataset_alerts) > length(shown))
                  sprintf("  ... and %d more\n", length(dataset_alerts) - length(shown)) else ""),
        file = stdout())
    flush(stdout())
  }

  code_manifest <- build_manifest(
    con, series = "code", repo = PUBLISH_REPO, db_filename = DB_FILENAME,
    db_bytes = code_db_bytes,
    # cran_metrics_failures is here for the guard rather than for the reader:
    # it is the one count in this manifest that growing is the bad news, and
    # the retention ceiling has nothing to compare against until it is
    # published beside the rest.
    tables = c("cran_code_summary", "cran_api_history", "cran_functions",
               "cran_call_edges", "cran_code_churn", "cran_metrics_failures"),
    fp_table = "cran_code_summary", fp_cols = c("package", "version"),
    pkg_table = "cran_code_summary", ver_table = "cran_code_summary",
    stat_table = "cran_code_summary", stat_cols = c("loc_r", "n_fns_r"),
    bootstrap = bootstrap)

  data_manifest <- build_manifest(
    data_con, series = "data", repo = PUBLISH_REPO, db_filename = DATA_DB_FILENAME,
    db_bytes = data_db_bytes,
    tables = c("cran_datasets", "cran_dataset_versions", "cran_dataset_contents"),
    fp_table = "cran_datasets", fp_cols = c("package", "name", "current_content_id"),
    pkg_table = "cran_datasets", ver_table = "cran_dataset_versions",
    stat_table = "cran_dataset_contents", stat_cols = c("nrow", "ncol"),
    bootstrap = bootstrap, coverage = dataset_coverage)

  text_manifest <- build_manifest(
    text_con, series = "text", repo = PUBLISH_REPO,
    db_filename = RELEASE_TEXT_DB_FILENAME, db_bytes = text_db_bytes,
    tables = c(DESCRIPTION_HISTORY_TABLE, RELEASE_NOTES_HISTORY_TABLE,
               RELEASE_TEXT_VERSIONS_TABLE),
    fp_table = RELEASE_TEXT_VERSIONS_TABLE, fp_cols = c("package", "version"),
    pkg_table = RELEASE_TEXT_VERSIONS_TABLE, ver_table = RELEASE_TEXT_VERSIONS_TABLE,
    stat_table = RELEASE_TEXT_VERSIONS_TABLE, stat_cols = "n_fields",
    bootstrap = bootstrap)
  # Names the code database it was published beside, so the next run can tell a mixed pair.
  text_manifest$code_fingerprint <- code_manifest$fingerprint
  text_check <- read_manifest_file(file.path(out_dir, "text-code-check.json"))

  write_manifest(file.path(out_dir, "code-manifest.json"), code_manifest)
  write_manifest(file.path(out_dir, "data-manifest.json"), data_manifest)
  write_manifest(file.path(out_dir, "text-manifest.json"), text_manifest)
  write_manifest(file.path(out_dir, "run-status.json"),
                 list(changed = changed, bootstrap_complete = bootstrap_complete,
                      n_analyzed = n_analyzed_pkgs, n_universe = n_universe,
                      n_remaining = length(remaining_after), n_fresh = length(fresh_pkgs),
                      n_shard = length(shard_pkgs),
                      n_versions = nrow(fresh_summary),
                      shard_failures = length(shard_failures),
                      text_code_mismatch = isTRUE(text_check$text_code_mismatch),
                      analyzer_version = bootstrap$analyzer_version,
                      output_class = bootstrap$output_class,
                      n_latest_on_build = bootstrap$n_latest_on_build,
                      failed_by_stage = .stage_counts(shard_stages),
                      parked = verdict_counts$parked,
                      failed_this_run = verdict_counts$failed_this_run,
                      over_cap_ok = verdict_counts$over_cap_ok,
                      over_cap_this_run = verdict_counts$over_cap_this_run,
                      n_released = n_released,
                      n_tried_skipped = length(tried_pkgs),
                      n_recheck_due = length(recheck_pkgs),
                      latest_by_build = .latest_by_build(con),
                      analyzer_stats = .na_as_null(telemetry$analyzer),
                      worker_phases = telemetry$phases))

  # ---- 8e. Retention guard --------------------------------------------------
  # The published database is the pipeline's accumulated state, so publishing a
  # smaller one overwrites collection nobody can recover except from an older
  # release. The check belongs here rather than in the workflow's
  # publish_metrics(): this is the last point where both this run's figures and
  # the previous release's are in hand as data, publish_metrics is shell, and
  # rewriting the comparison in jq would put the one check nobody can unit-test
  # in the one place nobody reads. Refusing here IS refusing to publish: the
  # step runs under `set -euo pipefail` and calls this script before it renders
  # notes or touches `gh release`, so a non-zero exit stops the run first.
  # The manifests above are written before the check on purpose. They are the
  # evidence for the message, and nothing publishes them once we stop.
  for (w in retention_warnings("code", code_manifest)) {
    warning(w, call. = FALSE, immediate. = TRUE)
  }
  rebuilding <- isTRUE(force_full) || full_rebuild_requested()
  violations <- c(
    retention_violations(
      "code", code_manifest,
      read_manifest_file(file.path(out_dir, "prev-code-manifest.json")),
      prior_tag = Sys.getenv("PREV_CODE_TAG", ""), force_full = rebuilding),
    retention_violations(
      "data", data_manifest,
      read_manifest_file(file.path(out_dir, "prev-data-manifest.json")),
      prior_tag = Sys.getenv("PREV_DATA_TAG", ""), force_full = rebuilding),
    retention_violations(
      "text", text_manifest,
      read_manifest_file(file.path(out_dir, "prev-text-manifest.json")),
      prior_tag = Sys.getenv("PREV_TEXT_TAG", ""), force_full = rebuilding))
  if (length(violations) > 0L) {
    stop(retention_refusal(violations), call. = FALSE)
  }

  # This shard passed, so it is what the next shard of this run inherits. The
  # ceilings are calibrated per shard (100 new failures is a quarter of one)
  # and the baseline they read is downloaded once for the whole run, so
  # without this the gain is measured over the day: a run of twelve shards
  # that each fail forty packages is refused at shard three for a burst none
  # of them had. The floors are deliberately left where they are, still
  # measuring the release this run started from. After the refusal above, so a
  # shard that was stopped does not raise the ceiling its re-run has to meet.
  advance_ceiling_baseline(file.path(out_dir, "prev-code-manifest.json"),
                           "code", code_manifest)
  advance_ceiling_baseline(file.path(out_dir, "prev-data-manifest.json"),
                           "data", data_manifest)

  if (length(fresh_pkgs) > 0L) {
    record_changed_packages(file.path(out_dir, "changed-packages.txt"), fresh_pkgs)
  }

  invisible(manifest)
}

# ---------------------------------------------------------------------------
# --harvest-descriptions: backfill title/description for archived packages
# ---------------------------------------------------------------------------
# A heavy, out-of-band pass (~8,600 archived packages) that fills the identity
# fields cran_archived_meta could not project because the stored rows predate
# the pipeline emitting Title/Description. For each archived package whose
# projected row still lacks a title it downloads ONLY that package's last
# archived tarball, extracts ONLY its DESCRIPTION, and upserts the derived
# fields. Idempotent and resumable: a filled row is never re-fetched, and a
# forced re-fetch whose DESCRIPTION is byte-identical (matching desc_sha) is a
# provable no-op.

# Polite default User-Agent so CRAN can attribute (and throttle) the traffic.
# A function, not a top-level constant: PUBLISH_REPO is defined in config.R, which
# update.R only sources inside its CLI entrypoint, so resolving it must be deferred
# to call time rather than evaluated when this file is sourced.
harvest_user_agent <- function() sprintf(
  "cran-code-metrics harvest (%s; %s)", PUBLISH_REPO, R.version.string)

#' Download one archived package's last tarball from the CRAN cloud mirror.
#'
#' Retries with linear backoff; honours the caller's options(timeout=...) via
#' download.file. Returns TRUE only when a non-empty file lands at destfile.
#'
#' @param package  Package name.
#' @param version  Last archived version string.
#' @param destfile Path to write the tarball to.
#' @param mirror   Base mirror URL (default the cloud CDN).
#' @param tries    Maximum download attempts.
#' @param sleep    Base backoff seconds (multiplied by the attempt number).
#' @return logical TRUE on success.
.download_archived_tarball <- function(package, version, destfile,
                                       mirror = "https://cloud.r-project.org",
                                       tries = 3L, sleep = 1) {
  url <- sprintf("%s/src/contrib/Archive/%s/%s_%s.tar.gz",
                 mirror, package, package, version)
  for (attempt in seq_len(tries)) {
    ok <- tryCatch({
      suppressWarnings(
        utils::download.file(url, destfile, mode = "wb", quiet = TRUE))
      file.exists(destfile) && file.info(destfile)$size > 0
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
    if (attempt < tries) Sys.sleep(sleep * attempt)  # linear backoff
  }
  FALSE
}

#' Derive the cran_archived_meta identity fields from an already-parsed
#' DESCRIPTION (a named list, as read.dcf/parse_dcf produce).
#'
#' Reuses metrics_meta() for title/description/authors/maintainer/deps by wrapping
#' the DESCRIPTION in a minimal context; license and url are read straight off the
#' field (metrics_meta does not carry them).
#'
#' @param desc Named list of DESCRIPTION fields.
#' @return Named list of the projected fields (no package/version/desc_sha).
.archived_fields_from_desc <- function(desc) {
  ctx <- list(desc = desc, exists = function(p) identical(p, "DESCRIPTION"))
  m   <- metrics_meta(ctx)
  .nz <- function(x) { x <- trimws(x %||% ""); if (nzchar(x)) x else NA_character_ }
  list(
    title            = m$title,
    description      = m$description,
    authors          = m$authors,
    maintainer       = m$maintainer,
    maintainer_email = m$maintainer_email,
    license          = .nz(desc[["License"]]),
    url              = .nz(desc[["URL"]]),
    depends          = m$depends,
    imports          = m$imports,
    suggests         = m$suggests,
    linkingto        = m$linking_to,
    enhances         = m$enhances
  )
}

#' Extract ONLY the DESCRIPTION from a package tarball and derive its identity
#' fields plus the sha256 of the DESCRIPTION bytes.
#'
#' Reads the member `<package>/DESCRIPTION`; if that exact path is absent (top-dir
#' casing differs from the package name) it falls back to the first `*/DESCRIPTION`
#' the archive lists. The declared `Encoding:` is honoured when re-parsing.
#'
#' @param tarfile Path to the downloaded .tar.gz.
#' @param package Package name (expected top-level directory).
#' @return Named list of fields plus $desc_sha, or NULL when no DESCRIPTION
#'   could be extracted or parsed.
.harvest_parse_description <- function(tarfile, package) {
  ex <- tempfile("ccm_harv_")
  dir.create(ex, recursive = TRUE)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)

  # A member absent from the archive makes the external tar emit a warning and a
  # non-zero code; that is an expected, handled miss (we fall back / return
  # NULL), so suppress the warning rather than let it surface as a failure.
  .untar_member <- function(member) {
    suppressWarnings(tryCatch(
      utils::untar(tarfile, files = member, exdir = ex),
      error = function(e) NULL))
    file.path(ex, member)
  }

  member <- paste0(package, "/DESCRIPTION")
  dpath  <- .untar_member(member)

  if (!file.exists(dpath)) {
    # Fallback: the top directory may be cased differently than the package.
    lst  <- suppressWarnings(tryCatch(utils::untar(tarfile, list = TRUE),
                                      error = function(e) character(0L)))
    cand <- lst[grepl("^[^/]+/DESCRIPTION$", lst)]
    if (length(cand) == 0L) return(NULL)
    dpath <- .untar_member(cand[[1L]])
    if (!file.exists(dpath)) return(NULL)
  }

  bytes    <- readBin(dpath, "raw", n = file.info(dpath)$size)
  desc_sha <- digest::digest(bytes, algo = "sha256", serialize = FALSE)

  enc <- tryCatch({
    m <- read.dcf(dpath, fields = "Encoding")
    e <- if ("Encoding" %in% colnames(m)) m[1L, "Encoding"] else NA_character_
    if (is.na(e)) "" else e
  }, error = function(e) "")

  fcon <- file(dpath, encoding = if (nzchar(enc)) enc else "")
  dcf  <- tryCatch(read.dcf(fcon), error = function(e) NULL)
  close(fcon)
  if (is.null(dcf) || nrow(dcf) == 0L) return(NULL)

  desc   <- stats::setNames(as.list(dcf[1L, ]), colnames(dcf))
  fields <- .archived_fields_from_desc(desc)
  fields$desc_sha <- desc_sha
  fields
}

#' Backfill title/description (and top up missing projected fields) for archived
#' packages whose cran_archived_meta row lacks a title.
#'
#' One tarball per package, DESCRIPTION-only. Each package is isolated in a
#' tryCatch so a missing tarball or malformed DESCRIPTION logs a warning and the
#' batch continues. Politeness: a descriptive User-Agent, a Sys.sleep between
#' packages, and retry-with-backoff inside the downloader.
#'
#' @param con         Open DBI connection to the pipeline database.
#' @param packages    Restrict to these packages (default: every row missing a
#'   title). Non-NULL is mainly for tests / targeted re-runs.
#' @param mirror      Base mirror URL.
#' @param user_agent  HTTPUserAgent set for the duration of the batch.
#' @param sleep       Seconds to sleep between packages (be polite).
#' @param tries       Max download attempts per package.
#' @param limit       Optional cap on packages processed this call (resumable).
#' @param download_fn Injectable downloader (package, version, destfile, mirror,
#'   tries, sleep) -> logical; defaults to the real cloud-mirror download.
#' @return invisible(list(todo, ok, skipped, failed)).
harvest_descriptions <- function(con, packages = NULL,
                                 mirror = "https://cloud.r-project.org",
                                 user_agent = harvest_user_agent(),
                                 sleep = 0.5, tries = 3L, limit = NULL,
                                 download_fn = .download_archived_tarball) {
  .ensure_archived_meta_table(con)

  todo <- if (is.null(packages)) {
    DBI::dbGetQuery(con,
      "SELECT package, last_version FROM cran_archived_meta
       WHERE title IS NULL AND last_version IS NOT NULL
       ORDER BY package")
  } else {
    pk <- unique(as.character(packages))
    if (length(pk) == 0L) {
      data.frame(package = character(0L), last_version = character(0L),
                 stringsAsFactors = FALSE)
    } else {
      ph <- paste(rep("?", length(pk)), collapse = ", ")
      DBI::dbGetQuery(con, sprintf(
        "SELECT package, last_version FROM cran_archived_meta
         WHERE package IN (%s) AND last_version IS NOT NULL
         ORDER BY package", ph), params = as.list(pk))
    }
  }
  if (!is.null(limit) && nrow(todo) > limit) todo <- todo[seq_len(limit), , drop = FALSE]

  old_ua <- getOption("HTTPUserAgent")
  options(HTTPUserAgent = user_agent)
  on.exit(options(HTTPUserAgent = old_ua), add = TRUE)

  n_ok <- 0L; n_skip <- 0L; n_fail <- 0L
  for (i in seq_len(nrow(todo))) {
    pkg <- todo$package[i]
    ver <- todo$last_version[i]
    res <- tryCatch(
      .harvest_one(con, pkg, ver, mirror, tries, sleep, download_fn),
      error = function(e) {
        warning(sprintf("harvest failed for '%s' (%s): %s",
                        pkg, ver, conditionMessage(e)))
        "fail"
      })
    if      (identical(res, "ok"))   n_ok   <- n_ok   + 1L
    else if (identical(res, "skip")) n_skip <- n_skip + 1L
    else                             n_fail <- n_fail + 1L
    if (sleep > 0) Sys.sleep(sleep)   # be polite between packages
  }

  invisible(list(todo = nrow(todo), ok = n_ok, skipped = n_skip, failed = n_fail))
}

# Harvest a single package: download -> DESCRIPTION-only parse -> idempotency
# gate -> upsert. Returns "ok", "skip" (byte-identical to a filled row), or
# "fail". Never throws for an expected miss; the caller's tryCatch is a backstop.
.harvest_one <- function(con, package, version, mirror, tries, sleep, download_fn) {
  tf <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  if (!isTRUE(download_fn(package, version, tf, mirror, tries, sleep))) {
    return("fail")
  }
  fields <- .harvest_parse_description(tf, package)
  if (is.null(fields) || is.na(fields$title)) return("fail")

  # Idempotency: a row already filled from a byte-identical DESCRIPTION needs no
  # write. Combined with the title-IS-NULL selection, a re-run is a no-op.
  existing <- DBI::dbGetQuery(con,
    "SELECT title, desc_sha FROM cran_archived_meta WHERE package = ?",
    params = list(package))
  if (nrow(existing) == 1L && !is.na(existing$title) &&
      identical(existing$desc_sha, fields$desc_sha)) {
    return("skip")
  }

  row <- data.frame(
    package           = package,
    last_version      = version,
    title             = fields$title,
    description       = fields$description,
    authors           = fields$authors,
    maintainer        = fields$maintainer,
    maintainer_email  = fields$maintainer_email,
    license           = fields$license,
    url               = fields$url,
    depends           = fields$depends,
    imports           = fields$imports,
    suggests          = fields$suggests,
    linkingto         = fields$linkingto,
    enhances          = fields$enhances,
    desc_sha          = fields$desc_sha,
    source_scanned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors  = FALSE
  )
  upsert_archived_meta_row(con, row)
  "ok"
}

#' CLI wrapper for the harvest pass: open the DB, refresh the archived-meta
#' projection (so every archived package has a last_version to fetch), then
#' backfill the rows still missing a title.
#'
#' @param io      IO interface providing $package_list() (for the archived set).
#' @param out_dir Directory holding the pipeline DB.
#' @param ...     Passed through to harvest_descriptions().
#' @return invisible(harvest_descriptions() result).
run_harvest <- function(io, out_dir, ...) {
  db_path <- file.path(out_dir, DB_FILENAME)
  con <- open_or_init_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  universe <- io$package_list()
  archived <- if (is.data.frame(universe) && nrow(universe) > 0L) {
    universe$package[is.na(universe$latest_version)]
  } else {
    character(0L)
  }
  project_archived_meta(con, archived)
  res <- harvest_descriptions(con, ...)
  cat(sprintf("harvest: %d/%d ok, %d skipped, %d failed\n",
              res$ok, res$todo, res$skipped, res$failed), file = stdout())
  flush(stdout())
  invisible(res)
}

# The flags update.R takes after <out_dir>.
.parse_cli_flags <- function(args) {
  out <- list(shard = SHARD_SIZE, force_full = FALSE, recollect = FALSE,
              harvest = FALSE, unpark = NULL, requeue = NULL)
  for (arg in args[startsWith(args, "--")]) {
    if (startsWith(arg, "--shard=")) {
      n <- suppressWarnings(as.integer(sub("^--shard=", "", arg, perl = TRUE)))
      if (!is.na(n) && n > 0L) out$shard <- n
    } else if (identical(arg, "--bootstrap")) {
      out$force_full <- TRUE
    } else if (identical(arg, "--recollect")) {
      out$recollect <- TRUE
    } else if (identical(arg, "--harvest-descriptions")) {
      out$harvest <- TRUE
    } else if (startsWith(arg, "--unpark=")) {
      out$unpark <- sub("^--unpark=", "", arg)
    } else if (startsWith(arg, "--requeue=")) {
      out$requeue <- sub("^--requeue=", "", arg)
    }
  }
  out
}

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
if (identical(sys.nframe(), 0L)) {
  # R prints at most warning.length bytes of an error and silently drops the
  # rest, and the default is 1000. The retention refusal is longer than that
  # once it lists the violations, and the half that falls off the end is the
  # half telling the operator what to actually do, which is the whole point of
  # writing it. 8170 is the documented maximum.
  options(warning.length = 8170L)

  # Standalone invocation (Rscript scripts/update.R): source the pipeline in
  # dependency order. Locate this script's directory so it works from any cwd.
  .script_dir <- {
    fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(fa) >= 1L) dirname(sub("^--file=", "", fa[1L])) else "scripts"
  }
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "git.R"))
  source(file.path(.script_dir, "context.R"))
  source(file.path(.script_dir, "binary.R"))
  for (.f in sort(list.files(file.path(.script_dir, "metrics"),
                             pattern = "[.]R$", full.names = TRUE))) source(.f)
  source(file.path(.script_dir, "analyze.R"))
  source(file.path(.script_dir, "export.R"))
  source(file.path(.script_dir, "release_text.R"))
  source(file.path(.script_dir, "retention.R"))

  args <- commandArgs(trailingOnly = TRUE)

  # First non-flag argument is out_dir.
  positional <- args[!startsWith(args, "--")]
  out_dir    <- if (length(positional) >= 1L) {
    positional[1L]
  } else {
    stop(
      "Usage: Rscript scripts/update.R <out_dir> [--shard=N] [--bootstrap] [--recollect] [--harvest-descriptions] [--unpark=all|fetch|analyze|timeout|<pkg,...>] [--requeue=<pkg,...>|over_cap]",
      call. = FALSE
    )
  }

  flags <- .parse_cli_flags(args)

  io <- default_io()
  if (isTRUE(flags$harvest)) {
    # Out-of-band backlog pass: does not analyze a shard, only backfills the
    # archived-metadata table's title/description from per-package DESCRIPTIONs.
    run_harvest(io, out_dir)
  } else {
    run_update(io, out_dir, shard_size = flags$shard, force_full = flags$force_full,
               recollect = flags$recollect, unpark = flags$unpark,
               requeue = flags$requeue)
  }
  message("Done.")
}
