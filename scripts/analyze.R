# scripts/analyze.R: metric dispatcher + per-package analysis orchestration.
#
# Load order: config.R -> git.R -> context.R -> metrics/structure.R -> analyze.R
# This file does NOT auto-source its dependencies so the caller controls order.

#' Registry of metric group functions.
#' Each value is a function(ctx) -> named list of scalar metric values.
#' Add more groups here when new metric modules are implemented.
METRIC_GROUPS <- list(
  structure   = metrics_structure,
  functions   = metrics_functions,
  docs        = metrics_docs,
  tests       = metrics_tests,
  security    = metrics_security,
  health      = metrics_health,
  portability = metrics_portability,
  legal       = metrics_legal,
  meta        = metrics_meta
)

#' Compute all registered metrics for one package version.
#'
#' Iterates over METRIC_GROUPS.  A group that throws an error:
#'   - emits a warning naming the group, the package, and the error message
#'   - contributes a sentinel NA entry (.error.<group>) to the result
#'   - does NOT abort the call
#'
#' @param ctx  A context environment from build_context().
#' @return Named list of scalars (metric name -> value).  Failed groups add
#'   `.error.<group_name> = NA` entries so callers can detect them.
analyze_version <- function(ctx) {
  result <- list()
  for (nm in names(METRIC_GROUPS)) {
    fn <- METRIC_GROUPS[[nm]]
    group_out <- tryCatch(
      fn(ctx),
      error = function(e) {
        warning(sprintf(
          "metric group '%s' failed for %s %s: %s",
          nm,
          ctx$package %||% "?",
          ctx$version %||% "?",
          conditionMessage(e)
        ))
        NULL
      }
    )
    if (is.null(group_out)) {
      # Record failure with a sentinel; column names are not known without success
      result[[paste0(".error.", nm)]] <- NA
    } else {
      result <- c(result, group_out)
    }
  }
  result
}

#' Analyze all versions of a cloned package repository.
#'
#' For each version (oldest first):
#'   1. Extracts the version tree via git archive.
#'   2. Builds a read_fn over the extracted files.
#'   3. Subsets the full-history churn to this version's commit.
#'   4. Builds a context with prev_exports from the preceding version.
#'   5. Calls analyze_version() to compute metrics.
#'   6. Records the per-version API diff (exports added/removed).
#'   7. Cleans up the extraction directory.
#'
#' Returns a list with three data.frames:
#'   $summary  one row per version, all metric columns + package/version/released
#'   $churn    package-stamped churn (package, version, file, added, deleted)
#'   $api      per-version export diff (package, version, exports_added,
#'             exports_removed, n_exports as JSON strings)
#'
#' Hook for Task 10: cross-version metrics should be computed from $summary after
#' this function returns, before writing to the database.
#'
#' @param repo_dir  Path to the cloned git repository.
#' @param package   Package name string.
#' @return Named list: $summary, $churn, $api.
analyze_package <- function(repo_dir, package) {
  versions_df <- list_versions(repo_dir)
  churn_all   <- package_churn(repo_dir)

  summary_rows <- vector("list", nrow(versions_df))
  api_rows     <- vector("list", nrow(versions_df))
  prev_exports <- NULL

  for (i in seq_len(nrow(versions_df))) {
    v      <- versions_df$version[i]
    ref    <- versions_df$ref[i]
    date   <- versions_df$date[i]
    commit <- versions_df$commit[i]

    # Per-version work is wrapped in local() so on.exit fires per iteration
    # rather than accumulating in the outer function's exit handlers.
    # This ensures temp-dir cleanup even when the loop body throws.
    iter <- local({
      tmp <- tempfile(pattern = paste0("ccm_", package, "_"))
      dir.create(tmp, recursive = TRUE)
      on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

      files <- tryCatch(
        extract_version(repo_dir, ref, tmp),
        error = function(e) character(0L)
      )

      # Build a read_fn closed over this iteration's extraction directory
      read_fn <- local({
        .d <- tmp
        function(path) {
          full <- file.path(.d, path)
          if (!file.exists(full)) return("")
          paste(readLines(full, warn = FALSE), collapse = "\n")
        }
      })

      # Subset churn_all to this version's commit
      churn_v <- if (nrow(churn_all) > 0L && !is.na(commit)) {
        # Commit SHAs from list_versions may be full or abbreviated;
        # match on startsWith to handle both
        mask <- startsWith(churn_all$commit, commit) |
                startsWith(commit, churn_all$commit)
        ch <- churn_all[mask, c("file", "added", "deleted"), drop = FALSE]
        rownames(ch) <- NULL
        ch
      } else {
        data.frame(file = character(0L), added = integer(0L),
                   deleted = integer(0L), stringsAsFactors = FALSE)
      }

      ctx <- build_context(
        package      = package,
        version      = v,
        ref          = ref,
        date         = date,
        files        = files,
        read_fn      = read_fn,
        churn_df     = churn_v,
        prev_exports = prev_exports
      )

      metrics <- analyze_version(ctx)

      # Stamp identity columns
      metrics[["package"]]  <- package
      metrics[["version"]]  <- v
      metrics[["released"]] <- date %||% NA_character_

      # Coerce each metric to a length-1 scalar (guard against bad group output)
      safe_metrics <- lapply(metrics, function(x) {
        if (is.null(x) || length(x) != 1L) NA else x
      })

      # API diff
      curr_exports <- tryCatch(ctx$namespace$exports, error = function(e) character(0L))
      curr_exports <- curr_exports %||% character(0L)
      added_exp    <- setdiff(curr_exports, prev_exports %||% character(0L))
      removed_exp  <- setdiff(prev_exports %||% character(0L), curr_exports)

      api_row <- data.frame(
        package         = package,
        version         = v,
        exports_added   = as.character(
          jsonlite::toJSON(added_exp,   auto_unbox = FALSE)),
        exports_removed = as.character(
          jsonlite::toJSON(removed_exp, auto_unbox = FALSE)),
        n_exports       = length(curr_exports),
        stringsAsFactors = FALSE
      )

      list(safe_metrics = safe_metrics, api_row = api_row, prev_exports = curr_exports)
    })

    summary_rows[[i]] <- iter$safe_metrics
    api_rows[[i]]     <- iter$api_row
    prev_exports      <- iter$prev_exports
  }

  # Assemble summary data.frame from list of named lists
  summary_df <- if (length(summary_rows) > 0L) {
    # Collect all column names across rows (groups may differ if some fail)
    all_cols <- unique(unlist(lapply(summary_rows, names)))
    rows_df  <- lapply(summary_rows, function(r) {
      row <- lapply(all_cols, function(cn) {
        v <- r[[cn]]
        if (is.null(v)) NA else v
      })
      names(row) <- all_cols
      as.data.frame(row, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows_df)
  } else {
    data.frame()
  }

  # Stamp churn with package name
  churn_df <- if (nrow(churn_all) > 0L) {
    data.frame(
      package = package,
      version = churn_all$version,
      file    = churn_all$file,
      added   = churn_all$added,
      deleted = churn_all$deleted,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      package = character(0L), version = character(0L),
      file    = character(0L), added   = integer(0L),
      deleted = integer(0L),  stringsAsFactors = FALSE
    )
  }

  api_df <- if (length(api_rows) > 0L) {
    do.call(rbind, api_rows)
  } else {
    data.frame(
      package         = character(0L), version         = character(0L),
      exports_added   = character(0L), exports_removed = character(0L),
      n_exports       = integer(0L),
      stringsAsFactors = FALSE
    )
  }

  list(
    summary = summary_df,
    churn   = churn_df,
    api     = api_df
  )
}
