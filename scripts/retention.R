# scripts/retention.R: what a run is allowed to publish, given what the
# previous release published.
#
# Load order: config.R -> retention.R
# This file does NOT auto-source its dependencies; the caller controls source order.
#
# The pipeline is incremental: the released database IS the accumulated state,
# rebuilt into and republished every run. That makes "publish" and "overwrite
# four months of collection" the same gesture, and the only thing separating
# them is whether the run started from the prior database. A sibling pipeline,
# cran-queue, published an empty database as latest on 2026-07-16 after one 503
# on the asset download, and stayed green for weeks.
#
# The workflow's download step is what stops that (an advertised asset that
# does not arrive fails the run). This file is the second line: it compares the
# figures this run is about to publish against the figures the previous release
# published, and refuses the publish when history would be lost. The tolerances
# are not guesses; each one is calibrated against the largest movement that
# field has actually made, measured across the 30 releases of 2026-07-15..08-14.
#
# A guard on a pipeline that runs three times a day also has to fail toward
# recovery. Its job is to stop a bad publish, not to become a state nobody can
# get out of, and its message is part of the mechanism rather than decoration:
# an operator who follows it has to end up better off, never holding a wiped
# database. That is why the comparison against the shipped manifest is
# one-sided, why a release that published no manifest gets a baseline measured
# from its database rather than a refusal that repeats every run, and why
# retention_repair_advice() names the release-level repair and rules force_full
# out. It is also why a refusal is assembled by retention_refusal() from the
# kind of guard that tripped rather than from one fixed wording: this file
# holds one check that fires when nothing was lost at all, and telling that
# operator to delete a release would be worse than saying nothing.

# The floor for a field is the MORE PERMISSIVE of a ratio and a flat row
# allowance: max_loss = 0 means "ratio only". A flat allowance matters on a
# small corpus, where a legitimate per-package delete-then-insert is a large
# fraction of a small total.
#
# One field runs the other way. A check carrying max_gain instead is a CEILING:
# rows it may gain, not rows it may lose. Only cran_metrics_failures has one,
# because it is the only count here that grows when the pipeline is going wrong
# rather than when it is working.
#
# The two also measure different spans, and the same downloaded baseline serves
# both. A floor asks what the previous RELEASE published: rows lost are lost
# however many shards ago it happened, so every shard of a run is measured
# against the same release. A ceiling asks what one SHARD failed, which is what
# its calibration below is written in, and advance_ceiling_baseline() is what
# makes the number it reads say that.
.RETENTION_CHECKS <- list(
  code = list(
    # No tolerance. The only paths that remove summary rows are force_full's
    # DELETE (exempted below, and reachable only by workflow_dispatch) and
    # upsert_shard's per-package delete, and the latter cannot remove a
    # package: it takes its package list from the frame it is about to insert,
    # so a package that yields zero rows is skipped rather than emptied.
    # Archival does not delete either. A lost download drops this by 98.8%.
    list(path = "n_packages",                min_ratio = 1,     max_loss = 0),
    # A package whose tag set on the cran mirror shrank while staying non-empty
    # writes fewer summary rows than it had. Bounded by that package's own
    # version count; the corpus mean is 6.2 versions/package, so 250 rows
    # covers a package with 40x the mean shedding its entire history. Also ~2x
    # the largest one-day change ever observed in this field (+129).
    list(path = "n_versions",                min_ratio = 0.999, max_loss = 250),
    # upsert_shard deletes ALL of a re-analysed package's detail rows and
    # re-inserts only its latest version's, so a release that removed functions
    # lowers the corpus total with nothing wrong. Worst measured: -0.053%
    # (cran_functions) and -0.022% (cran_call_edges) on 2026-07-24. 2% is 37x
    # that and ~4x the largest one-day movement in either direction (+0.53%).
    # These are also the tables force_full does NOT wipe, which is what tells a
    # deliberate rebuild apart from a lost download.
    list(path = "tables.cran_functions",     min_ratio = 0.98,  max_loss = 0),
    list(path = "tables.cran_call_edges",    min_ratio = 0.98,  max_loss = 0),
    list(path = "tables.cran_code_churn",    min_ratio = 0.98,  max_loss = 0),
    # Monotone by construction on the scheduled path: SQLite does not shrink a
    # file on DELETE, it moves the page to the free list. Never fell across 30
    # releases (1.163 GB -> 1.256 GB). It is also what tolerates force_full,
    # whose DELETEs leave the file size where it was. The one legitimate
    # shrink is the reclaim on the publish path, and that one does not arrive
    # here as a fall: credit_reclaim_to_baseline() takes the bytes it returned
    # off `was` first, so both sides are measured the same way.
    list(path = "db_bytes",                  min_ratio = 0.90,  max_loss = 0),
    # n_universe is network-derived and degrades silently: package_list()
    # returns an empty frame with only a warning when a fetch fails, and it is
    # the denominator of bootstrap_complete, so a collapsed universe makes
    # "complete" true exactly when the database is empty. A single failed fetch
    # is a 26% or 74% drop; the largest measured is -1 (-0.003%).
    list(path = "bootstrap.n_universe",      min_ratio = 0.90,  max_loss = 0),
    # The ceiling, and the only one. A package enters this table when its
    # clone or its analysis fails and leaves it the moment either succeeds, so
    # a run that suddenly cannot analyse anything shows up here first and
    # nowhere else: the row counts it did not write are not a fall, they are an
    # absence of a rise, and no floor can see that.
    #
    # 100 is a quarter of a shard. A shard that newly fails that many packages
    # has something wrong with it rather than a bad day, and publishing it
    # would make its baseline the one tomorrow is measured against.
    #
    # A shard, and not a day. The re-scan a new analyzer generation asks for
    # walks the whole catalog at 400 packages a shard over weeks, so a day
    # whose shards each fail a handful is what that looks like, and a ceiling
    # read straight off the downloaded baseline would charge shard 5 for what
    # shards 1 to 4 already published and were passed for.
    # advance_ceiling_baseline() is what keeps the span the one written here.
    #
    # It is deliberately a per-shard ceiling and not an absolute cap. This
    # table only sheds a package when that package is analysed successfully
    # again, and a package that has failed MAX_CLONE_FAILURES times is excluded
    # from the queue, so its row can never leave on its own. An absolute cap on
    # a count that cannot come down is a refusal that repeats every run
    # forever, which is the state this whole file is written to avoid. The
    # standing level is reported by retention_warnings() instead.
    list(path = "tables.cran_metrics_failures", max_gain = 100)
  ),
  data = list(
    # The dataset database is downloaded by its own request and can be lost
    # while the code side looks perfect, and `changed` is computed from the
    # CODE fingerprint alone, so nothing else in the run would notice. The same
    # tolerances also catch a regressed dataset reader, which deletes rows with
    # no download failing at all: .write_datasets_normalized() deletes by
    # package BEFORE its empty-frame early return.
    # 1% is 52x the worst measured decrease (cran_datasets -10, -0.019%, on
    # 2026-07-26); contents gets a second point for the orphan GC.
    list(path = "n_packages",                     min_ratio = 0.99, max_loss = 0),
    list(path = "tables.cran_dataset_versions",   min_ratio = 0.99, max_loss = 0),
    list(path = "tables.cran_datasets",           min_ratio = 0.99, max_loss = 0),
    list(path = "tables.cran_dataset_contents",   min_ratio = 0.98, max_loss = 0),
    list(path = "db_bytes",                       min_ratio = 0.90, max_loss = 0)
  )
)

# The two key counts per series, named once so the pre-flight check and its
# messages cannot drift apart.
.RETENTION_KEY_TABLES <- list(
  code = list(ver_table = "cran_code_summary",     pkg_table = "cran_code_summary"),
  data = list(ver_table = "cran_dataset_versions", pkg_table = "cran_datasets")
)

# Read one dotted path ("tables.cran_functions") out of a parsed manifest.
# Returns NULL when any level is absent or the value is not a single number, so
# a manifest written before a field existed is skipped rather than read as 0.
.ret_at <- function(x, path) {
  for (part in strsplit(path, ".", fixed = TRUE)[[1L]]) {
    if (!is.list(x)) return(NULL)
    x <- x[[part]]
    if (is.null(x)) return(NULL)
  }
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) return(NULL)
  as.numeric(x)
}

# Row counts and byte counts, printed in full. format="d" would overflow on
# db_bytes and the default would render 1256263680 as 1.256264e+09.
.ret_fmt <- function(x) sprintf("%.0f", as.numeric(x))

#' Whether the operator asked for a full rebuild of this run.
#'
#' The flag itself is not enough. The workflow passes --bootstrap to the first
#' shard only, because a second one would wipe what the first just collected,
#' so an exemption keyed on the flag would cover shard 1 and refuse shard 2 of
#' the operator's own rebuild for holding 400 packages instead of 33,282. The
#' dispatch input is exported for the whole step instead, and a scheduled run
#' cannot set it: workflow_dispatch inputs are empty outside a dispatch.
#'
#' @return TRUE when the run was dispatched with force_full checked.
full_rebuild_requested <- function() {
  identical(tolower(Sys.getenv("FORCE_FULL_REBUILD", "")), "true")
}

#' Read a manifest JSON file, or NULL when it is absent or unparseable.
#'
#' @param path Path to a *-manifest.json file.
#' @return Parsed list, or NULL.
read_manifest_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
}

#' Figures this run must not publish, given the previous release's figures.
#'
#' @param series     "code" or "data".
#' @param current    Manifest list this run is about to publish.
#' @param prior      Manifest list the previous release published, or NULL.
#' @param prior_tag  Tag of the previous release, "" when none exists. A
#'   non-empty tag with a NULL prior is the wipe state itself (a release we
#'   could not read), and is a violation; an empty tag is a genuine cold start
#'   and is not.
#' @param force_full TRUE for an operator-requested full rebuild, which is the
#'   one path that legitimately empties tables.
#' @return Character vector of violations, empty when the run may publish. Each
#'   element is NAMED with the kind of guard that produced it, "floor" or
#'   "ceiling", because the two are opposite failures with opposite repairs and
#'   retention_refusal() has to tell them apart without reading the text.
retention_violations <- function(series, current, prior, prior_tag = "",
                                 force_full = FALSE) {
  # --bootstrap wipes three code tables before re-analysing, so its first shard
  # publishes 400 packages by design. It is reachable only by workflow_dispatch,
  # so the exemption is an explicit operator act rather than a hole the
  # scheduler can fall through, and the download step's assertion still runs:
  # a failed download cannot imitate this, because it cannot set the flag.
  # --recollect needs no exemption (it upserts in place and wipes nothing) and
  # --harvest-descriptions returns before the shard loop.
  if (isTRUE(force_full)) return(character(0L))

  checks <- .RETENTION_CHECKS[[series]]
  if (is.null(checks)) return(character(0L))

  if (is.null(prior) || length(prior) == 0L) {
    if (nzchar(prior_tag)) {
      # A release whose manifest we cannot read is the lost-download state, so
      # it belongs to the floor family however differently it is worded.
      return(c(floor = sprintf(
        paste0("no %s baseline to compare against: release %s exists but its ",
               "manifest was not read, which is the state a lost download ",
               "leaves behind"),
        series, prior_tag)))
    }
    return(character(0L))  # genuinely the first release
  }

  out <- character(0L)
  for (chk in checks) {
    was <- .ret_at(prior, chk$path)
    if (is.null(was)) next          # the prior manifest predates this field
    now <- .ret_at(current, chk$path) %||% 0
    # A check carries a floor, a ceiling, or one day both. Each is read on its
    # own so adding the second to an existing entry cannot quietly disable the
    # first.
    if (!is.null(chk$max_gain)) {
      ceiling_v <- was + chk$max_gain
      if (now > ceiling_v) {
        out <- c(out, ceiling = sprintf("%s %s rose to %s from %s (ceiling %s)",
                                        series, chk$path, .ret_fmt(now),
                                        .ret_fmt(was), .ret_fmt(ceiling_v)))
      }
    }
    if (!is.null(chk$min_ratio)) {
      floor_v <- min(was * chk$min_ratio, was - chk$max_loss)
      if (now < floor_v) {
        out <- c(out, floor = sprintf("%s %s fell to %s from %s (floor %s)",
                                      series, chk$path, .ret_fmt(now),
                                      .ret_fmt(was), .ret_fmt(floor_v)))
      }
    }
  }
  out
}

# The share of the universe that may sit in cran_metrics_failures before the
# run says so.
#
# The ceiling in .RETENTION_CHECKS compares one shard to the next, so a table
# that creeps up two packages at a time passes it every time, which is exactly
# how this one reached 200 rows from 3 inside a month with nothing said. The
# level itself is the other half of that finding, and it is a warning rather
# than a gate because a package failing is not history being lost: its stored
# rows are still there, and refusing to publish over it would throw away the
# collection of every package that did work.
#
# 0.5% of 33,307 is 166. A package leaves this table the moment it is analysed
# successfully, so anything standing at that level is not a bad afternoon, it
# is a part of the catalog nobody is collecting any more.
#
# The share needs a floor under it as well, because a share on its own says
# nothing about a small corpus: one failing package out of five is 20% and is
# not a finding. Both have to be met.
FAILURE_SHARE_WARN <- 0.005
FAILURE_COUNT_WARN <- 25L

#' Figures worth saying out loud without halting the run.
#'
#' analyze.R stamps one api row per version row, so the three counts have been
#' equal in every release measured. It stays a warning rather than a gate
#' because upsert_shard dedups the summary frame on (package, version) but not
#' the api frame, so a package emitting a duplicate version row would break the
#' equality without losing anything.
#'
#' @param series  "code" or "data".
#' @param current Manifest list this run is about to publish.
#' @return Character vector of warnings, possibly empty.
retention_warnings <- function(series, current) {
  if (!identical(series, "code")) return(character(0L))
  out <- character(0L)

  fails    <- .ret_at(current, "tables.cran_metrics_failures")
  universe <- .ret_at(current, "bootstrap.n_universe")
  if (!is.null(fails) && !is.null(universe) && universe > 0 &&
      fails >= FAILURE_COUNT_WARN && fails > universe * FAILURE_SHARE_WARN) {
    out <- c(out, sprintf(paste0(
      "cran_metrics_failures holds %s packages, %.2f%% of the %s in the ",
      "universe: that many packages are failing to clone or to analyse and ",
      "the ones past %d attempts have been dropped from the queue for good"),
      .ret_fmt(fails), 100 * fails / universe, .ret_fmt(universe),
      MAX_CLONE_FAILURES))
  }

  n_ver <- .ret_at(current, "n_versions")
  summ  <- .ret_at(current, "tables.cran_code_summary")
  api   <- .ret_at(current, "tables.cran_api_history")
  if (is.null(n_ver) || is.null(summ) || is.null(api)) return(out)
  if (api == summ && summ == n_ver) return(out)
  c(out, sprintf(
    paste0("cran_api_history holds %s rows and cran_code_summary %s ",
           "(n_versions %s): a version was written without its api row"),
    .ret_fmt(api), .ret_fmt(summ), .ret_fmt(n_ver)))
}

#' What an operator should actually do when one of these guards refuses.
#'
#' The message is part of the mechanism, so it gets the same care as the
#' comparison. The first version of it named force_full as the only way
#' forward, and force_full IS the wipe: update.R runs DELETE FROM
#' cran_code_summary, cran_code_churn and cran_api_history, the shard publishes
#' the resulting 400-package database as latest, and FORCE_FULL_REBUILD then
#' exempts every later shard and every re-dispatch from this check. Rebuilding
#' 33,282 packages at 400 a shard takes days, and the viewer serves a gutted
#' catalog throughout. An operator following that advice out of a recoverable
#' mishap would have destroyed exactly what the guard was protecting.
#'
#' @return A single string, ready to append to a refusal.
retention_repair_advice <- function() {
  paste0(
    "\nLook at the PREVIOUS release first. A later shard on the same day ",
    "replaces the four assets one `gh release upload --clobber` at a time, ",
    "which deletes each existing asset before uploading its replacement, so ",
    "an interrupted publish can leave one shard's database beside another ",
    "shard's manifest, or a manifest whose database is gone.\n",
    "If `gh release list` also shows a Draft under the tag the download step ",
    "resolved as code src / data src, a failed publish left it beside the ",
    "published release. It was never published, but a download by that tag ",
    "can come from either. List the releases under the tag by id with ",
    "`gh api 'repos/{owner}/{repo}/releases?per_page=100' --paginate ",
    "-q '.[] | select(.tag_name == \"<tag>\") | \"\\(.id) draft=\\(.draft)\"'`, ",
    "delete the draft=true ones with ",
    "`gh api -X DELETE repos/{owner}/{repo}/releases/<id>`, and re-run. Not by ",
    "tag: a delete by tag can take the published one and keep the draft.\n",
    "Otherwise open that release and make its assets agree again: re-upload the ",
    "database and the manifest that belong together, or delete that release ",
    "so the day before it becomes latest again. Then re-run.\n",
    "If that release is consistent, this run really did lose the rows, and ",
    "the cause is upstream of the publish. Do not paper over it here.\n",
    "force_full is not the repair either way. It deletes cran_code_summary, ",
    "cran_code_churn and cran_api_history and republishes a 400-package ",
    "catalog as latest, which is the outcome this check exists to prevent. ",
    "Use it only for a rebuild of the whole catalog that you actually want.")
}

#' What an operator should actually do when the failures ceiling refuses.
#'
#' The ceiling needs its own text because the advice above would actively harm
#' here. Nothing was lost when this guard fires: the packages that failed still
#' have every row they had, and the repair above opens with the previous
#' release and offers deleting it, which in the middle of a mirror outage means
#' throwing away a release that is perfectly fine while the 250 packages that
#' actually failed go unmentioned.
#'
#' @return A single string, ready to append to a refusal.
retention_failure_advice <- function() {
  sprintf(paste0(
    "\nNothing was dropped. Every row these packages had is still in the ",
    "database. What this run recorded is that they would not clone or would ",
    "not analyse, and cran_metrics_failures counts them.\n",
    "Look at the run log first: each failing package prints its name and the ",
    "reason it failed on one line, and a jump this size is usually one cause ",
    "shared by all of them and upstream of the pipeline (the cran mirror ",
    "refusing clones, the runner losing the network, an analyzer meeting a ",
    "shape it did not handle before). `SELECT package, consecutive_failures, ",
    "last_attempt FROM cran_metrics_failures ORDER BY last_attempt DESC` in ",
    "this run's database lists them.\n",
    "A package leaves this table the moment it is analysed successfully, so ",
    "the repair is to fix that cause and re-run. Leaving it is not free: a ",
    "package that fails %d times in a row is dropped from the queue for good. ",
    "Do not reach for the previous release, which did not cause this, and do ",
    "not reach for force_full, which re-analyses through the same failure and ",
    "republishes a 400-package catalog while doing it."),
    MAX_CLONE_FAILURES)
}

#' The whole refusal a run stops with, worded for the guards that tripped.
#'
#' retention_violations() reports two opposite failures through one vector, and
#' one fixed headline cannot cover both: told that a burst of failures "would
#' drop history the previous release carried" and handed the release-level
#' repair, an operator deletes a good release during an incident whose cause is
#' entirely upstream. The kinds present in the vector pick both the headline
#' and which advice is appended, and a run that managed both gets both.
#'
#' @param violations Character vector from retention_violations(), named by
#'   guard kind. An unnamed element is treated as a floor, which is what every
#'   check but one is.
#' @return A single string, ready to pass to stop().
retention_refusal <- function(violations) {
  kinds <- names(violations) %||% rep("", length(violations))
  kinds[!nzchar(kinds)] <- "floor"

  headline <- character(0L)
  advice   <- character(0L)
  if ("floor" %in% kinds) {
    headline <- c(headline,
                  "this run would drop history the previous release carried")
    advice   <- c(advice, retention_repair_advice())
  }
  if ("ceiling" %in% kinds) {
    headline <- c(headline,
                  "this shard failed far more packages than the shard before it did")
    advice   <- c(advice, retention_failure_advice())
  }

  paste0("refusing to publish: ", paste(headline, collapse = ", and "), ".\n  ",
         paste(violations, collapse = "\n  "),
         paste(advice, collapse = ""))
}

#' Whether a downloaded prior database has LESS in it than the manifest that
#' shipped with it recorded.
#'
#' One-sided on purpose. Only `now < was` is the signature this guard is for: a
#' truncated file, or a database from before the rows the manifest counted. The
#' other direction, a database with MORE rows than its manifest, is what an
#' interrupted `gh release upload --clobber` leaves when shard N's database
#' lands and shard N-1's manifest is still attached, and it costs nothing: a
#' smaller baseline only makes the retention floor more permissive, never less.
#' Demanding equality in both directions made that mishap permanent, because
#' the same release is still `latest_tag metrics` tomorrow and the day after,
#' so every scheduled run failed in the download step before analysing a single
#' package. See prior_db_notes() for how the stale side is reported instead.
#'
#' In rows rather than bytes: a --harvest-descriptions run clobbers the
#' database asset while leaving the published manifest stale, so the file size
#' can legitimately differ from db_bytes, but harvest only ever writes
#' cran_archived_meta and can never move these two counts.
#'
#' @param series "code" or "data".
#' @param counts list(n_packages, n_versions) measured from the downloaded DB.
#' @param prior  Manifest published alongside that DB, or NULL (nothing to check).
#' @return Character vector of violations, possibly empty.
prior_db_violations <- function(series, counts, prior) {
  tbls <- .ret_prior_tables(series, prior)
  if (is.null(tbls)) return(character(0L))
  if (is.character(tbls)) return(tbls)

  out <- character(0L)
  was_ver <- .ret_at(prior, "n_versions")
  now_ver <- as.numeric(counts$n_versions %||% 0)
  if (!is.null(was_ver) && now_ver < was_ver) {
    out <- c(out, sprintf(
      "the downloaded %s database holds %s rows in %s; the manifest published with it says %s",
      series, .ret_fmt(now_ver), tbls$ver_table, .ret_fmt(was_ver)))
  }
  was_pkg <- .ret_at(prior, "n_packages")
  now_pkg <- as.numeric(counts$n_packages %||% 0)
  if (!is.null(was_pkg) && now_pkg < was_pkg) {
    out <- c(out, sprintf(
      "the downloaded %s database covers %s packages in %s; the manifest published with it says %s",
      series, .ret_fmt(now_pkg), tbls$pkg_table, .ret_fmt(was_pkg)))
  }
  out
}

#' A downloaded prior database that is AHEAD of the manifest shipped with it.
#'
#' Not a violation, but not nothing either: it says the previous publish was
#' interrupted partway through its four assets, and the release will keep
#' handing out a mismatched pair until someone fixes it. Worth an annotation in
#' the log every run, so it gets noticed before something less benign lands in
#' the same window.
#'
#' @inheritParams prior_db_violations
#' @return Character vector of notes, possibly empty.
prior_db_notes <- function(series, counts, prior) {
  tbls <- .ret_prior_tables(series, prior)
  if (is.null(tbls) || is.character(tbls)) return(character(0L))

  out <- character(0L)
  was_ver <- .ret_at(prior, "n_versions")
  now_ver <- as.numeric(counts$n_versions %||% 0)
  if (!is.null(was_ver) && now_ver > was_ver) {
    out <- c(out, sprintf(paste0(
      "the downloaded %s database holds %s rows in %s but the manifest ",
      "published with it says %s: the previous publish did not finish ",
      "uploading its four assets. Building on it anyway (a smaller baseline ",
      "only loosens the retention floor), but re-upload the manifest that ",
      "belongs with that database."),
      series, .ret_fmt(now_ver), tbls$ver_table, .ret_fmt(was_ver)))
  }
  was_pkg <- .ret_at(prior, "n_packages")
  now_pkg <- as.numeric(counts$n_packages %||% 0)
  if (!is.null(was_pkg) && now_pkg > was_pkg) {
    out <- c(out, sprintf(paste0(
      "the downloaded %s database covers %s packages in %s but the manifest ",
      "published with it says %s"),
      series, .ret_fmt(now_pkg), tbls$pkg_table, .ret_fmt(was_pkg)))
  }
  out
}

# Shared preamble for the two functions above: NULL when there is nothing to
# compare, a character violation when the manifest describes a different
# series, and the key-table names when the comparison can go ahead.
.ret_prior_tables <- function(series, prior) {
  if (is.null(prior) || length(prior) == 0L) return(NULL)
  tbls <- .RETENTION_KEY_TABLES[[series]]
  if (is.null(tbls)) return(NULL)
  declared <- as.character(prior$series %||% "")
  if (!identical(declared, series)) {
    # Measuring the dataset database against a code manifest would compare
    # cran_dataset_versions to a code n_versions and call a healthy database
    # broken.
    return(sprintf(
      "the %s baseline manifest declares series \"%s\"; it does not describe %s",
      series, declared, tbls$ver_table))
  }
  tbls
}

# Count rows and distinct packages in one table of a database file. A table
# that does not exist counts 0, which is exactly the empty-database case the
# caller is looking for.
.ret_db_counts <- function(db_path, ver_table, pkg_table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  present <- DBI::dbListTables(con)
  n_ver <- if (ver_table %in% present) {
    as.numeric(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) n FROM "%s"', ver_table))$n)
  } else 0
  n_pkg <- if (pkg_table %in% present) {
    as.numeric(DBI::dbGetQuery(con,
      sprintf('SELECT COUNT(DISTINCT package) n FROM "%s"', pkg_table))$n)
  } else 0
  list(n_versions = n_ver, n_packages = n_pkg)
}

# The two series the download step brings back, named once.
.ret_prior_specs <- function() list(
  list(series = "code", manifest = "prev-code-manifest.json", db = DB_FILENAME),
  list(series = "data", manifest = "prev-data-manifest.json", db = DATA_DB_FILENAME)
)

# The row counts a series' checks actually read, so a baseline measured from a
# database carries exactly the fields retention_violations() will look for.
.ret_check_tables <- function(series) {
  checks <- .RETENTION_CHECKS[[series]]
  if (is.null(checks)) return(character(0L))
  paths <- vapply(checks, function(chk) chk$path, character(1L))
  sub("^tables\\.", "", grep("^tables\\.", paths, value = TRUE))
}

#' A baseline measured from a downloaded database, for a release that
#' published no manifest.
#'
#' A same-day republish is not atomic (four assets, each existing one deleted by
#' --clobber before its replacement lands), so a run that died in that window can
#' leave a release carrying its database and no code-manifest.json. Refusing on
#' that was a permanent outage: the same release stays latest, so every later
#' run refused too, and the only recovery the refusal named was the wipe.
#'
#' The database is right there and it is the thing worth protecting, so measure
#' it. The result is a real floor for retention_violations(): a run that then
#' publishes less than what came back is still refused. It deliberately carries
#' no bootstrap.n_universe, because nothing in the file records what the
#' universe was; .ret_at() skips a field the baseline does not carry.
#'
#' Returns NULL when there is nothing to measure. An absent or empty database
#' is exactly what a lost download leaves, and must stay indistinguishable from
#' it, so it yields no baseline and retention_violations() refuses on the tag.
#'
#' @param series  "code" or "data".
#' @param db_path Path to the downloaded database.
#' @return A manifest-shaped list, or NULL.
derive_baseline_manifest <- function(series, db_path) {
  tbls <- .RETENTION_KEY_TABLES[[series]]
  if (is.null(tbls) || !file.exists(db_path)) return(NULL)
  size <- as.numeric(file.info(db_path)$size)
  if (length(size) != 1L || is.na(size) || size <= 0) return(NULL)
  counts <- .ret_db_counts(db_path, tbls$ver_table, tbls$pkg_table)
  if (counts$n_versions <= 0 || counts$n_packages <= 0) return(NULL)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  present <- DBI::dbListTables(con)
  wanted <- .ret_check_tables(series)
  table_counts <- stats::setNames(lapply(wanted, function(t) {
    if (!t %in% present) return(0)
    as.numeric(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) n FROM "%s"', t))$n)
  }), wanted)

  list(schema_version = 1L, series = series,
       measured_from = basename(db_path), db_bytes = round(size),
       n_packages = counts$n_packages, n_versions = counts$n_versions,
       tables = table_counts)
}

#' Give a series a baseline when the prior release published none.
#'
#' Writes prev-<series>-manifest.json from the downloaded database, and only
#' when that file is absent. A manifest that IS present is never replaced, even
#' when it disagrees with the database: absence of a record is not evidence
#' that nothing was lost, but a record saying the database used to be bigger
#' is, and prior_db_violations() has to keep seeing it.
#'
#' @param out_dir Directory holding the downloaded assets.
#' @return Character vector of notes describing what was measured, empty when
#'   every series already had a published manifest.
ensure_prior_baseline <- function(out_dir) {
  notes <- character(0L)
  for (spec in .ret_prior_specs()) {
    mpath <- file.path(out_dir, spec$manifest)
    if (file.exists(mpath)) next
    derived <- derive_baseline_manifest(spec$series, file.path(out_dir, spec$db))
    if (is.null(derived)) next
    jsonlite::write_json(derived, mpath, auto_unbox = TRUE, pretty = TRUE)
    notes <- c(notes, sprintf(paste0(
      "the prior release carries %s but no %s manifest, which is what an ",
      "interrupted `gh release upload --clobber` leaves. The baseline for ",
      "this run was measured from the database instead: %s packages, %s rows ",
      "in %s. Re-upload the manifest that belongs with that database so the ",
      "next run has a published record to check against."),
      spec$db, spec$series, .ret_fmt(derived$n_packages),
      .ret_fmt(derived$n_versions),
      .RETENTION_KEY_TABLES[[spec$series]]$ver_table))
  }
  notes
}

#' Restate a baseline in the units the reclaimed database is now measured in.
#'
#' The db_bytes check reads a smaller file as history that went missing, and
#' that is right for every cause but one: a VACUUM makes the file smaller while
#' changing nothing that is in it. The pages it hands back were free pages in
#' the file this run inherited, so the previous release's database, vacuumed,
#' would have been exactly that much smaller too. Subtracting the measured
#' reclaim from the baseline is therefore not an exemption, it is the same
#' number expressed the same way on both sides, and it excuses precisely the
#' shrink the run caused and not one byte more: a run that reclaims 630 MB and
#' also loses half its rows is still refused.
#'
#' It is written to the downloaded prev-*-manifest.json rather than held in
#' memory because the check runs once per shard and the reclaim happens on the
#' first one. Later shards of the same run re-read that file, and a run only
#' ever sees a baseline it downloaded itself, so the credit expires when the
#' run does. Nothing else reads db_bytes out of it: prior_db_violations()
#' compares rows.
#'
#' @param path      Path to a prev-<series>-manifest.json. An absent file is
#'   the cold-start case and is not an error.
#' @param reclaimed Bytes the vacuum actually returned to the filesystem.
#' @return TRUE when the baseline was rewritten, FALSE when there was nothing
#'   to credit.
credit_reclaim_to_baseline <- function(path, reclaimed) {
  reclaimed <- as.numeric(reclaimed %||% 0)
  if (!is.finite(reclaimed) || reclaimed <= 0) return(invisible(FALSE))
  prior <- read_manifest_file(path)
  was <- .ret_at(prior, "db_bytes")
  if (is.null(was)) return(invisible(FALSE))
  prior$db_bytes <- max(0, round(was - reclaimed))
  jsonlite::write_json(prior, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(TRUE)
}

# The paths in a series' checks that carry a ceiling rather than a floor. Read
# off .RETENTION_CHECKS rather than named here, so a second ceiling added there
# is measured the same way as the first without anyone remembering this.
.ret_ceiling_paths <- function(series) {
  checks <- .RETENTION_CHECKS[[series]]
  if (is.null(checks)) return(character(0L))
  has <- vapply(checks, function(chk) !is.null(chk$max_gain), logical(1L))
  vapply(checks[has], function(chk) chk$path, character(1L))
}

# Write one dotted path into a parsed manifest. The caller settles whether the
# field is there to write; this only reaches it.
.ret_set_at <- function(x, path, value) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1L]]
  if (length(parts) == 1L) {
    x[[parts[[1L]]]] <- value
    return(x)
  }
  x[[parts[[1L]]]] <- .ret_set_at(x[[parts[[1L]]]],
                                  paste(parts[-1L], collapse = "."), value)
  x
}

#' Restate a baseline's ceilings as what the shard that just passed published.
#'
#' A floor and a ceiling measure different spans. A floor asks what the
#' previous RELEASE published, because losing rows is losing them however many
#' shards ago it happened, and every shard of a run is rightly measured against
#' the same release. A ceiling asks what one SHARD newly failed: 100 is a
#' quarter of a shard, and a shard that newly fails that many has something
#' wrong with it rather than a bad day.
#'
#' Nothing made that difference true. prev-*-manifest.json is downloaded once
#' per run, by the step before the shard loop, and every shard re-reads that
#' same file, so the ceiling measured the whole day: shard 5 was charged for
#' the packages shards 1 to 4 already failed, published, and were passed for.
#' The re-scan behind a new analyzer generation walks the whole catalog over
#' weeks, so a day whose shards each fail a handful is its ordinary shape, and
#' a guard that refuses it is a guard that stops the re-scan.
#'
#' Moving the baseline forward after each shard passes is what makes the
#' comparison the one the calibration describes, and it is not an exemption:
#' the next shard may still fail only max_gain packages more than the table
#' holds when it starts. It moves down as readily as up, because a shard that
#' finally analysed a pile of failing packages empties their rows, and a
#' baseline left where the day started would let the shard after it fail that
#' many again without a word.
#'
#' Written to the downloaded file for the same reason credit_reclaim_to_baseline()
#' writes there: each shard is a fresh R process, and a run only ever sees a
#' baseline it downloaded itself, so the restatement expires when the run does.
#' Only the ceiling fields move. Every floor keeps measuring the release the
#' run started from, and a baseline that carries no such field is left without
#' one, so a release published before a count existed keeps the ceiling off for
#' the whole run rather than switching it on at shard 2.
#'
#' @param path    Path to a prev-<series>-manifest.json. An absent file is the
#'   cold-start case and is not an error.
#' @param series  "code" or "data".
#' @param current Manifest the shard just passed the check with.
#' @return TRUE when the baseline was rewritten, FALSE when there was nothing
#'   to move.
advance_ceiling_baseline <- function(path, series, current) {
  prior <- read_manifest_file(path)
  if (is.null(prior) || length(prior) == 0L) return(invisible(FALSE))
  moved <- FALSE
  for (p in .ret_ceiling_paths(series)) {
    was <- .ret_at(prior, p)
    now <- .ret_at(current, p)
    if (is.null(was) || is.null(now) || was == now) next
    prior <- .ret_set_at(prior, p, now)
    moved <- TRUE
  }
  if (!moved) return(invisible(FALSE))
  jsonlite::write_json(prior, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(TRUE)
}

#' Check both downloaded prior databases against their manifests.
#'
#' Called once per run, from the download step, before any shard writes: the
#' comparison only holds on the first shard, because every later shard has
#' legitimately added rows to the same file while prev-*-manifest.json still
#' describes yesterday's release.
#'
#' @param out_dir Directory holding the downloaded prev-*-manifest.json files
#'   and the two databases.
#' @return list(violations, notes). violations is empty when the run may build
#'   on what came back; notes carries the recoverable mismatches, which are
#'   worth saying out loud and are not worth stopping a daily pipeline for.
preflight_prior_dbs <- function(out_dir) {
  out   <- character(0L)
  notes <- character(0L)
  for (spec in .ret_prior_specs()) {
    mpath <- file.path(out_dir, spec$manifest)
    if (!file.exists(mpath)) next          # no baseline: nothing to check here
    prior <- read_manifest_file(mpath)
    if (is.null(prior)) {
      out <- c(out, sprintf("%s could not be parsed as JSON", spec$manifest))
      next
    }
    dbpath <- file.path(out_dir, spec$db)
    size <- if (file.exists(dbpath)) as.numeric(file.info(dbpath)$size) else 0
    if (size <= 0) {
      # Named, so preflight_refusal() can say the database is missing rather
      # than smaller.
      out <- c(out, absent = sprintf(
        "%s came back from the prior release but %s did not",
        spec$manifest, spec$db))
      next
    }
    tbls <- .RETENTION_KEY_TABLES[[spec$series]]
    counts <- .ret_db_counts(dbpath, tbls$ver_table, tbls$pkg_table)
    out   <- c(out, prior_db_violations(spec$series, counts, prior))
    notes <- c(notes, prior_db_notes(spec$series, counts, prior))
  }
  list(violations = out, notes = notes)
}

#' The refusal preflight stops with, headed for what actually came back.
#'
#' "The prior database holds less than the manifest recorded" describes a
#' truncated file. After 2026-09-13 every scheduled run stopped under that
#' headline about a database that had not arrived at all: a draft left by a
#' failed publish carried the two manifests and nothing else. The advice below
#' it named the right repair, but a headline about a smaller file sends an
#' operator looking at row counts. A violation named "absent" gets its own
#' headline; every other one keeps the old headline, and a run with both gets
#' both.
#'
#' @param violations Character vector from preflight_prior_dbs().
#' @return A single string, ready to pass to stop().
preflight_refusal <- function(violations) {
  kinds <- names(violations) %||% rep("", length(violations))
  headline <- character(0L)
  if ("absent" %in% kinds) {
    headline <- c(headline, paste0(
      "the prior release handed back a manifest without the database it ",
      "describes"))
  }
  if (any(kinds != "absent")) {
    headline <- c(headline, paste0(
      "the prior database holds less than the manifest published with it ",
      "recorded"))
  }
  paste0(paste(headline, collapse = ", and "),
         "; refusing to build a release on top of it.",
         retention_repair_advice())
}
