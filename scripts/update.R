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

# Increment consecutive_failures for a package in cran_metrics_failures.
.record_failure <- function(con, pkg) {
  now_str  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  existing <- DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM cran_metrics_failures WHERE package = ?",
    params = list(pkg))
  if (nrow(existing) == 0L) {
    DBI::dbExecute(con,
      "INSERT INTO cran_metrics_failures (package, consecutive_failures, last_attempt)
       VALUES (?, 1, ?)",
      params = list(pkg, now_str))
  } else {
    DBI::dbExecute(con,
      "UPDATE cran_metrics_failures
       SET consecutive_failures = consecutive_failures + 1, last_attempt = ?
       WHERE package = ?",
      params = list(now_str, pkg))
  }
  invisible(NULL)
}

# Delete a package's failure record (reset after a successful analysis).
.reset_failure <- function(con, pkg) {
  DBI::dbExecute(con,
    "DELETE FROM cran_metrics_failures WHERE package = ?",
    params = list(pkg))
  invisible(NULL)
}

# Return packages with consecutive_failures >= MAX_CLONE_FAILURES.
.permanent_failures <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT package FROM cran_metrics_failures WHERE consecutive_failures >= ?",
    params = list(MAX_CLONE_FAILURES))$package
}

#' Packages needing a metrics backfill: those with a stored row where the
#' sentinel column is NULL, or every stored package when that column has not been
#' added yet. Restricted to the current universe and excluding permanent
#' failures.
#'
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
#' Clone and analyze failures are tracked per-package. Packages that have
#' failed >= MAX_CLONE_FAILURES consecutive times are permanently excluded from
#' the to-do list and counted in the manifest permanent_failures field.
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
#' @return Manifest list (invisibly).
run_update <- function(io, out_dir, shard_size = SHARD_SIZE, force_full = FALSE,
                       recollect = FALSE) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  db_path      <- file.path(out_dir, DB_FILENAME)
  data_db_path <- file.path(out_dir, DATA_DB_FILENAME)

  # ---- 1. Open both DBs (creates tables if absent) --------------------------
  con <- open_or_init_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data_con <- open_or_init_data_db(data_db_path)
  on.exit(DBI::dbDisconnect(data_con), add = TRUE)

  # ---- 2. Analyzed state (O(n_packages) query, not full table read) ---------
  if (isTRUE(force_full)) {
    # Wipe all metric rows so everything is treated as unseen.
    tables <- DBI::dbListTables(con)
    for (tbl in c("cran_code_summary", "cran_code_churn", "cran_api_history")) {
      if (tbl %in% tables) DBI::dbExecute(con, sprintf("DELETE FROM %s", tbl))
    }
    analyzed <- character(0L)
  } else {
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
  perm_fail_pkgs <- .permanent_failures(con)

  # ---- 5. To-do: packages that need analysis --------------------------------
  if (isTRUE(force_full)) {
    todo_pkgs <- sort(as.character(
      universe$package[!universe$package %in% perm_fail_pkgs]
    ))
  } else if (isTRUE(recollect)) {
    # Backfill: only packages whose rows predate the binary metrics. No wipe;
    # upsert_shard replaces each package's rows in place.
    todo_pkgs <- .recollect_todo(con, universe$package, perm_fail_pkgs)
  } else {
    is_todo <- vapply(seq_len(n_universe), function(i) {
      pkg <- as.character(universe$package[i])
      if (pkg %in% perm_fail_pkgs) return(FALSE)  # permanently excluded
      lv  <- universe$latest_version[i]
      if (!pkg %in% names(analyzed)) return(TRUE)   # never analyzed
      stored_v <- analyzed[[pkg]]
      # Archived packages (NA latest_version): skip once analyzed.
      if (is.na(lv)) return(FALSE)
      # New release detected: CRAN version differs from what is stored.
      !identical(as.character(lv), as.character(stored_v))
    }, logical(1L))
    changed <- as.character(universe$package[is_todo])
    # Also drain any packages whose rows predate the binary metrics, so a normal
    # scheduled run finishes the one-time backfill and then reverts to just the
    # changed packages once none remain.
    backfill <- .recollect_todo(con, universe$package, perm_fail_pkgs)
    # And drain any packages whose latest-version row was stored before the
    # per-function/per-edge detail scan (detail_scanned IS NULL on that row).
    # Latest-row-scoped so it converges: a package re-analyzed once is marked and
    # never re-flagged, even if it produced zero functions.
    detail_backfill <- .recollect_todo(con, universe$package, perm_fail_pkgs,
                                        sentinel = "detail_scanned",
                                        latest_only = TRUE)
    # And drain any package whose latest-version row predates the dataset reader
    # (datasets_scanned IS NULL), so cran_datasets fills in without a manual
    # recollect. Also latest-row-scoped, so it converges once re-analyzed.
    dataset_backfill <- .recollect_todo(con, universe$package, perm_fail_pkgs,
                                        sentinel = "datasets_scanned",
                                        latest_only = TRUE)
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
  flush(stdout())

  # ---- 6. Analyze the shard (parallel) -------------------------------------
  shard_summary_list   <- list()
  shard_churn_list     <- list()
  shard_api_list       <- list()
  shard_functions_list <- list()
  shard_edges_list     <- list()
  shard_datasets_list  <- list()
  shard_failures       <- character(0L)

  if (!dir.exists(WORK_DIR)) dir.create(WORK_DIR, recursive = TRUE)

  # Worker: clone + analyze one package. No database access.
  # Returns list(package, ok, [summary, churn, api]).
  .pkg_worker <- function(pkg) {
    .t0  <- Sys.time()
    .idx <- match(pkg, shard_pkgs)          # queue position; shard_pkgs is unique
    .n   <- length(shard_pkgs)
    # Thinned per-worker completion line, emitted FROM the fork so it streams live
    # during the otherwise-silent parallel phase. Prints only on every 25th queue
    # position, every failure, every slow (>=30s) package, and the last position.
    # One fully-formed cat() to stdout (< PIPE_BUF): forks reorder whole lines but
    # never byte-interleave, and fd 1 is disjoint from mclapply's result pipe. The
    # emit is wrapped in try() so a broken-stream write can never turn an ok
    # package into a recorded failure.
    .done <- function(ok, stage, nver) {
      el <- as.numeric(difftime(Sys.time(), .t0, units = "secs"))
      if (isTRUE(ok) && .idx %% 25L != 0L && el < 30 && !identical(.idx, .n)) {
        return(invisible())
      }
      try({
        cat(sprintf("[%d/%d] %s %s: %s in %.1fs\n",
                    .idx, .n,
                    if (isTRUE(ok)) "ok" else "FAIL", pkg,
                    if (isTRUE(ok)) sprintf("%d versions", nver)
                    else paste0(stage, " failed"),
                    el),
            file = stdout())
        flush(stdout())
      }, silent = TRUE)
      invisible()
    }
    dest <- file.path(WORK_DIR, pkg)
    on.exit(unlink(dest, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(setTimeLimit(), add = TRUE)
    setTimeLimit(elapsed = WORKER_TIMEOUT, transient = TRUE)
    ok <- tryCatch(io$clone(pkg, dest), error = function(e) FALSE)
    if (!isTRUE(ok)) {
      .done(FALSE, "clone", 0L)
      return(list(package = pkg, ok = FALSE))
    }
    res <- tryCatch(
      analyze_package(dest, pkg),
      error = function(e) {
        warning(sprintf("analyze_package failed for '%s': %s",
                        pkg, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(res)) {
      .done(FALSE, "analyze", 0L)
      return(list(package = pkg, ok = FALSE))
    }
    .done(TRUE, "ok", nrow(res$summary))
    list(package = pkg, ok = TRUE,
         summary = res$summary, churn = res$churn, api = res$api,
         functions = res$functions, edges = res$edges, datasets = res$datasets)
  }

  results <- parallel::mclapply(shard_pkgs, .pkg_worker,
                                mc.cores       = ANALYSIS_CORES,
                                mc.preschedule = FALSE)

  # Collect results in input order (shard_pkgs is sorted, so DB is deterministic).
  # All DB writes happen here in the parent process.
  for (i in seq_along(results)) {
    r   <- results[[i]]
    pkg <- shard_pkgs[[i]]
    # Guard: mclapply may return a try-error on worker crash.
    if (inherits(r, "try-error") || is.null(r[["ok"]]) || !isTRUE(r$ok)) {
      shard_failures <- c(shard_failures, pkg)
      .record_failure(con, pkg)
    } else {
      shard_summary_list[[pkg]]   <- r$summary
      shard_churn_list[[pkg]]     <- r$churn
      shard_api_list[[pkg]]       <- r$api
      shard_functions_list[[pkg]] <- r$functions
      shard_edges_list[[pkg]]     <- r$edges
      shard_datasets_list[[pkg]]  <- r$datasets
      .reset_failure(con, pkg)
    }
  }

  # ---- 7. Upsert shard into DB in-place (O(shard) memory) ------------------
  fresh_pkgs      <- names(shard_summary_list)
  fresh_summary   <- .rbind_union_all(shard_summary_list)   %||% .empty_summary()
  fresh_churn     <- .rbind_union_all(shard_churn_list)     %||% .empty_churn()
  fresh_api       <- .rbind_union_all(shard_api_list)       %||% .empty_api()
  fresh_functions <- .rbind_union_all(shard_functions_list) %||% .empty_functions_df()
  fresh_edges     <- .rbind_union_all(shard_edges_list)     %||% .empty_edges_df()
  fresh_datasets  <- .rbind_union_all(shard_datasets_list)  %||% .empty_datasets_df()

  if (length(fresh_pkgs) > 0L) {
    # Write dataset rows before the code summary stamps datasets_scanned = TRUE,
    # so a scanned code row always implies its dataset rows were written. The
    # dataset write is delete-then-insert (idempotent), so if the code write
    # fails afterwards the package stays on the to-do list and the next run
    # redoes both cleanly, rather than being marked done with datasets missing.
    upsert_datasets(data_con, fresh_datasets, fresh_pkgs)
    upsert_shard(con, fresh_summary, fresh_churn, fresh_api,
                 fresh_functions, fresh_edges)
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

  # Re-query permanent failures after this run (some may have just hit the limit).
  n_permanent_failures <- length(.permanent_failures(con))

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

  # changed: something substantive happened OR the content hash shifted.
  changed <- isTRUE(force_full) ||
    length(fresh_pkgs) > 0L ||
    !identical(prior_fp, new_fp)

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
  flush(stdout())

  bootstrap <- list(n_analyzed = n_analyzed_pkgs, n_universe = n_universe,
                    n_remaining = length(remaining_after),
                    bootstrap_complete = bootstrap_complete)
  code_db_bytes <- as.numeric(file.info(db_path)$size %||% 0)
  data_db_bytes <- as.numeric(file.info(data_db_path)$size %||% 0)

  code_manifest <- build_manifest(
    con, series = "code", repo = PUBLISH_REPO, db_filename = DB_FILENAME,
    db_bytes = code_db_bytes,
    tables = c("cran_code_summary", "cran_api_history", "cran_functions",
               "cran_call_edges", "cran_code_churn"),
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
    bootstrap = bootstrap)

  write_manifest(file.path(out_dir, "code-manifest.json"), code_manifest)
  write_manifest(file.path(out_dir, "data-manifest.json"), data_manifest)
  write_manifest(file.path(out_dir, "run-status.json"),
                 list(changed = changed, bootstrap_complete = bootstrap_complete,
                      n_analyzed = n_analyzed_pkgs, n_universe = n_universe,
                      n_remaining = length(remaining_after), n_fresh = length(fresh_pkgs),
                      n_shard = length(shard_pkgs),
                      n_versions = nrow(fresh_summary),
                      shard_failures = length(shard_failures)))

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

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
if (identical(sys.nframe(), 0L)) {
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

  args <- commandArgs(trailingOnly = TRUE)

  # First non-flag argument is out_dir.
  positional <- args[!startsWith(args, "--")]
  out_dir    <- if (length(positional) >= 1L) {
    positional[1L]
  } else {
    stop(
      "Usage: Rscript scripts/update.R <out_dir> [--shard=N] [--bootstrap] [--recollect] [--harvest-descriptions]",
      call. = FALSE
    )
  }

  shard_override <- SHARD_SIZE
  force_full     <- FALSE
  recollect      <- FALSE
  harvest        <- FALSE

  for (arg in args[startsWith(args, "--")]) {
    if (startsWith(arg, "--shard=")) {
      n <- suppressWarnings(
        as.integer(sub("^--shard=", "", arg, perl = TRUE))
      )
      if (!is.na(n) && n > 0L) shard_override <- n
    } else if (identical(arg, "--bootstrap")) {
      force_full <- TRUE
    } else if (identical(arg, "--recollect")) {
      recollect <- TRUE
    } else if (identical(arg, "--harvest-descriptions")) {
      harvest <- TRUE
    }
  }

  io <- default_io()
  if (isTRUE(harvest)) {
    # Out-of-band backlog pass: does not analyze a shard, only backfills the
    # archived-metadata table's title/description from per-package DESCRIPTIONs.
    run_harvest(io, out_dir)
  } else {
    run_update(io, out_dir, shard_size = shard_override, force_full = force_full,
               recollect = recollect)
  }
  message("Done.")
}
