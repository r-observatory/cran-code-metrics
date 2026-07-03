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

# Drop rows whose 'package' column is in pkgs.
.drop_packages <- function(df, pkgs) {
  if (is.null(df) || nrow(df) == 0L || length(pkgs) == 0L) return(df)
  if (!"package" %in% names(df)) return(df)
  df[!df$package %in% pkgs, , drop = FALSE]
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
#' Reads the prior published DB (if present in out_dir) as accumulated state,
#' determines which packages need analysis (new or updated), processes the next
#' shard, merges fresh rows with the carry-forward, writes the updated DB, and
#' emits a manifest.json.
#'
#' @param io         IO interface: list with $package_list() and $clone().
#'   Use default_io() for production; inject a fake for tests.
#' @param out_dir    Directory to read prior DB from and write outputs to.
#' @param shard_size Maximum packages to analyze in this run.
#'   Defaults to SHARD_SIZE from config.R.
#' @param force_full When TRUE, re-analyze all packages in the universe
#'   regardless of carry-forward state.
#' @return Manifest list (invisibly).
run_update <- function(io, out_dir, shard_size = SHARD_SIZE, force_full = FALSE) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  db_path <- file.path(out_dir, DB_FILENAME)

  # ---- 1. Carry-forward: read prior DB if present --------------------------
  acc_summary <- NULL
  acc_churn   <- NULL
  acc_api     <- NULL
  analyzed    <- character(0L)  # named vector: package -> latest version in DB

  if (file.exists(db_path)) {
    con <- tryCatch(
      DBI::dbConnect(RSQLite::SQLite(), db_path),
      error = function(e) {
        warning(sprintf("Could not open prior DB '%s': %s",
                        db_path, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(con)) {
      tryCatch({
        tables <- DBI::dbListTables(con)
        if ("cran_code_summary" %in% tables) {
          acc_summary <- DBI::dbReadTable(con, "cran_code_summary")
        }
        if ("cran_code_churn" %in% tables) {
          acc_churn <- DBI::dbReadTable(con, "cran_code_churn")
        }
        if ("cran_api_history" %in% tables) {
          acc_api <- DBI::dbReadTable(con, "cran_api_history")
        }
      }, error = function(e) {
        warning(sprintf("Error reading prior DB tables: %s", conditionMessage(e)))
      }, finally = {
        DBI::dbDisconnect(con)
      })
    }
  }

  # Build analyzed: package -> latest version already stored.
  # Primary: add_cross_version_metrics() sets latest_release_date only on the
  # last-version row per package. Fallback: last version row seen in DB.
  if (!is.null(acc_summary) && nrow(acc_summary) > 0L &&
      "package" %in% names(acc_summary) && "version" %in% names(acc_summary)) {
    if ("latest_release_date" %in% names(acc_summary)) {
      date_rows <- acc_summary[!is.na(acc_summary$latest_release_date), ,
                               drop = FALSE]
    } else {
      date_rows <- acc_summary[integer(0L), , drop = FALSE]
    }

    analyzed <- if (nrow(date_rows) > 0L) {
      setNames(as.character(date_rows$version),
               as.character(date_rows$package))
    } else {
      character(0L)
    }

    # Fallback: packages with no non-NA latest_release_date row.
    missing_pkgs <- setdiff(
      unique(as.character(acc_summary$package)),
      names(analyzed)
    )
    for (pkg in missing_pkgs) {
      vers <- acc_summary$version[acc_summary$package == pkg]
      analyzed[[pkg]] <- as.character(vers[length(vers)])
    }
  }

  # ---- 2. Universe ---------------------------------------------------------
  universe <- io$package_list()
  if (!is.data.frame(universe) || nrow(universe) == 0L) {
    universe <- data.frame(package = character(0L), latest_version = character(0L),
                           stringsAsFactors = FALSE)
  }
  n_universe <- nrow(universe)

  # ---- 3. To-do: packages that need analysis --------------------------------
  if (isTRUE(force_full)) {
    todo_pkgs <- sort(as.character(universe$package))
  } else {
    is_todo <- vapply(seq_len(n_universe), function(i) {
      pkg <- as.character(universe$package[i])
      lv  <- universe$latest_version[i]

      if (!pkg %in% names(analyzed)) return(TRUE)   # never analyzed

      stored_v <- analyzed[[pkg]]
      # Archived packages (NA latest_version): skip once analyzed.
      if (is.na(lv)) return(FALSE)
      # New release detected: CRAN version differs from what is stored.
      !identical(as.character(lv), as.character(stored_v))
    }, logical(1L))
    todo_pkgs <- sort(as.character(universe$package[is_todo]))
  }

  # Take the first shard_size packages from the to-do list (deterministic order).
  shard_pkgs <- if (length(todo_pkgs) > shard_size) {
    todo_pkgs[seq_len(shard_size)]
  } else {
    todo_pkgs
  }

  # ---- 4. Analyze the shard ------------------------------------------------
  shard_summary_list <- list()
  shard_churn_list   <- list()
  shard_api_list     <- list()
  shard_failures     <- character(0L)

  if (!dir.exists(WORK_DIR)) dir.create(WORK_DIR, recursive = TRUE)

  for (pkg in shard_pkgs) {
    dest <- file.path(WORK_DIR, pkg)

    # Attempt clone. Return value of FALSE (or any error) means failure.
    ok <- tryCatch(io$clone(pkg, dest), error = function(e) FALSE)

    if (!isTRUE(ok)) {
      unlink(dest, recursive = TRUE, force = TRUE)
      shard_failures <- c(shard_failures, pkg)
      next
    }

    # Analyze; wrap in tryCatch so one package failure never aborts the shard.
    res <- tryCatch(
      analyze_package(dest, pkg),
      error = function(e) {
        warning(sprintf("analyze_package failed for '%s': %s",
                        pkg, conditionMessage(e)))
        NULL
      }
    )

    # Always delete the clone directory, even when analysis failed.
    unlink(dest, recursive = TRUE, force = TRUE)

    if (is.null(res)) {
      shard_failures <- c(shard_failures, pkg)
    } else {
      shard_summary_list[[pkg]] <- res$summary
      shard_churn_list[[pkg]]   <- res$churn
      shard_api_list[[pkg]]     <- res$api
    }
  }

  # ---- 5. Merge: drop refreshed packages, row-bind carry-forward + fresh ----
  fresh_pkgs <- names(shard_summary_list)

  # Remove any prior rows for packages being refreshed this shard.
  kept_summary <- .drop_packages(acc_summary, fresh_pkgs)
  kept_churn   <- .drop_packages(acc_churn,   fresh_pkgs)
  kept_api     <- .drop_packages(acc_api,     fresh_pkgs)

  # Combine the shard's fresh rows (union-column-tolerant).
  fresh_summary <- .rbind_union_all(shard_summary_list)
  fresh_churn   <- .rbind_union_all(shard_churn_list)
  fresh_api     <- .rbind_union_all(shard_api_list)

  # Merge carry-forward with fresh (union columns; fill NA for new schema cols).
  full_summary <- .rbind_union_all(list(kept_summary, fresh_summary)) %||%
    .empty_summary()
  full_churn   <- .rbind_union_all(list(kept_churn,   fresh_churn))   %||%
    .empty_churn()
  full_api     <- .rbind_union_all(list(kept_api,     fresh_api))     %||%
    .empty_api()

  # ---- 6. Export -----------------------------------------------------------
  export_metrics(db_path, full_summary, full_churn, full_api)

  # ---- 7. Manifest ---------------------------------------------------------
  n_analyzed_pkgs <- if (nrow(full_summary) > 0L &&
                         "package" %in% names(full_summary)) {
    length(unique(full_summary$package))
  } else {
    0L
  }
  new_fp <- metrics_fingerprint(full_summary)

  manifest_path <- file.path(out_dir, "manifest.json")
  prior_fp <- tryCatch({
    if (file.exists(manifest_path)) {
      m <- jsonlite::fromJSON(manifest_path)
      m[["fingerprint"]]
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # bootstrap_complete: all to-do is covered by this shard AND the DB now
  # accounts for (universe - failures).
  remaining_after    <- setdiff(todo_pkgs, shard_pkgs)
  n_failures         <- length(shard_failures)
  bootstrap_complete <- length(remaining_after) == 0L &&
    n_analyzed_pkgs >= (n_universe - n_failures)

  # changed: something substantive happened OR the content hash shifted.
  changed <- isTRUE(force_full) ||
    length(fresh_pkgs) > 0L ||
    !identical(prior_fp, new_fp)

  manifest <- list(
    generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    n_universe         = n_universe,
    n_analyzed         = n_analyzed_pkgs,
    n_shard            = length(shard_pkgs),
    shard_failures     = list(
      count    = n_failures,
      packages = head(shard_failures, 20L)
    ),
    bootstrap_complete = bootstrap_complete,
    fingerprint        = new_fp,
    changed            = changed
  )

  write_manifest(manifest_path, manifest)
  invisible(manifest)
}

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
if (identical(sys.nframe(), 0L)) {
  args <- commandArgs(trailingOnly = TRUE)

  # First non-flag argument is out_dir.
  positional <- args[!startsWith(args, "--")]
  out_dir    <- if (length(positional) >= 1L) {
    positional[1L]
  } else {
    stop(
      "Usage: Rscript scripts/update.R <out_dir> [--shard=N] [--bootstrap]",
      call. = FALSE
    )
  }

  shard_override <- SHARD_SIZE
  force_full     <- FALSE

  for (arg in args[startsWith(args, "--")]) {
    if (startsWith(arg, "--shard=")) {
      n <- suppressWarnings(
        as.integer(sub("^--shard=", "", arg, perl = TRUE))
      )
      if (!is.na(n) && n > 0L) shard_override <- n
    } else if (identical(arg, "--bootstrap")) {
      force_full <- TRUE
    }
  }

  io <- default_io()
  run_update(io, out_dir, shard_size = shard_override, force_full = force_full)
  message("Done.")
}
