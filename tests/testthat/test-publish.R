# tests/testthat/test-publish.R
#
# scripts/publish.sh resolves the release a run builds on and publishes the one
# it produced. On 2026-09-13 `gh release create metrics-2026-09-13 <4 assets>`
# got HTTP 500 on both database uploads, gh's own delete of the draft it had
# made got a 500 too, and a draft holding only the two manifests was left
# behind. `gh release list` returns drafts to this repository's token and the
# tag sorted above every real release, so each run after it resolved the draft
# as the prior release, came back with manifests and no databases, and was
# refused. Nothing could publish past it.
#
# These drive the real shell functions through bash, against a fake `gh` that
# keeps its releases in a JSON file and fails where it is told to. What they
# can say is how the functions behave against gh's documented semantics, as
# fixtures/fake-gh.sh models them; they are not a test of GitHub.

.pub_script <- function() {
  normalizePath(test_path("..", "..", "scripts", "publish.sh"), mustWork = TRUE)
}

.pub_assets <- c(
  "cran-code-metrics.db" = 5000L, "cran-data-metrics.db" = 3000L,
  "code-manifest.json" = 40L, "data-manifest.json" = 60L)

# A release as the fake keeps it. `assets` is a named integer vector of sizes,
# and `extra` a list of .pub_asset() entries for the half-uploaded and
# temporary ones a named vector cannot describe.
#
# Every asset carries an id, as an uploaded one does, because a replacement
# turns on ids: the live asset keeps its id through the rename that moves it
# out of the way, and the asset that takes its name is a different one.
.pub_release <- function(id, tag, draft = FALSE, assets = integer(0L),
                         latest = FALSE, prerelease = FALSE,
                         extra = list()) {
  numbered <- lapply(seq_along(assets), function(i) {
    .pub_asset(id * 100L + i, names(assets)[[i]], assets[[i]])
  })
  list(id = id, tagName = tag, isDraft = draft, isPrerelease = prerelease,
       isLatest = latest,
       hasTag = !draft, name = tag, body = "",
       assets = unname(c(numbered, extra)))
}

# One asset. A starter is what an upload that was cut off leaves: the full
# declared size, no digest, and invisible to every listing but the per-release
# one.
.pub_asset <- function(id, name, size, state = "uploaded") {
  list(id = id, name = name, size = size, state = state)
}

# The 09-12 release: published, Latest, and complete.
.pub_0912 <- function() {
  .pub_release(1L, "metrics-2026-09-12", assets = .pub_assets - 1000L,
               latest = TRUE)
}

# What the failed 09-13 create left: a draft carrying the manifests only.
.pub_stranded_0913 <- function(id = 2L) {
  .pub_release(id, "metrics-2026-09-13", draft = TRUE,
               assets = .pub_assets[c("code-manifest.json", "data-manifest.json")])
}

#' A throwaway world: a fake gh on PATH, its release state, and a working
#' directory holding the out/ files the workflow would publish.
#'
#' @param releases List of .pub_release() entries, oldest first.
#' @param faults   Named integer vector: fault name to how many times it fires.
#' @param frame    Where the temporary directory is cleaned up.
.pub_world <- function(releases, faults = integer(0L), frame = parent.frame()) {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("bash")), "bash is not installed")
  skip_if(!nzchar(Sys.which("jq")), "jq is not installed")

  dir <- withr::local_tempdir(.local_envir = frame)
  bin <- file.path(dir, "bin")
  dir.create(bin)
  file.copy(test_path("fixtures", "fake-gh.sh"), file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), mode = "0755")

  work <- file.path(dir, "work")
  dir.create(file.path(work, "out"), recursive = TRUE)
  for (n in names(.pub_assets)) {
    writeBin(as.raw(rep(0x2e, .pub_assets[[n]])), file.path(work, "out", n))
  }
  writeLines("notes for today", file.path(work, "out", "release-notes-code.md"))

  world <- list(dir = dir, bin = bin, work = work,
                state = file.path(dir, "state.json"),
                log = file.path(dir, "gh.log"),
                faults = file.path(dir, "faults"))
  dir.create(world$faults)
  file.create(world$log)
  .pub_set_state(world, releases)
  .pub_set_faults(world, faults)
  world
}

.pub_set_state <- function(world, releases) {
  jsonlite::write_json(releases, world$state, auto_unbox = TRUE)
}

.pub_set_faults <- function(world, faults) {
  unlink(list.files(world$faults, full.names = TRUE))
  for (n in names(faults)) writeLines(as.character(faults[[n]]), file.path(world$faults, n))
}

.pub_state <- function(world) {
  jsonlite::read_json(world$state, simplifyVector = FALSE)
}

.pub_log <- function(world) readLines(world$log, warn = FALSE)

#' Run a bash snippet the way a workflow step runs it: under
#' `set -euo pipefail`, with scripts/publish.sh sourced, from the directory
#' holding out/. Retries do not sleep.
#'
#' @return list(status, output), status 0 on success.
.pub_run <- function(world, ..., env = character(0L)) {
  script <- file.path(world$dir, "step.sh")
  writeLines(c("set -euo pipefail",
               sprintf("source %s", shQuote(.pub_script())),
               sprintf("cd %s", shQuote(world$work)),
               ...), script)
  vars <- c(PATH = paste(world$bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
            GH_STATE = world$state, GH_LOG = world$log, GH_FAULTS = world$faults,
            PUBLISH_RETRY_SECONDS = "0", env)
  out <- withr::with_envvar(vars, suppressWarnings(
    system2("bash", shQuote(script), stdout = TRUE, stderr = TRUE)))
  list(status = attr(out, "status") %||% 0L, output = out)
}

.pub_releases_named <- function(world, tag) {
  Filter(function(r) identical(r$tagName, tag), .pub_state(world))
}

# The one release under `tag`, asserting there is exactly one.
.pub_only <- function(world, tag) {
  rs <- .pub_releases_named(world, tag)
  expect_length(rs, 1L)
  rs[[1L]]
}

.pub_asset_sizes <- function(release) {
  sizes <- vapply(release$assets, function(a) as.integer(a$size), integer(1L))
  names(sizes) <- vapply(release$assets, function(a) a$name, character(1L))
  sizes[order(names(sizes))]
}

.pub_asset_ids <- function(release) {
  ids <- vapply(release$assets, function(a) as.integer(a$id), integer(1L))
  names(ids) <- vapply(release$assets, function(a) a$name, character(1L))
  ids[order(names(ids))]
}

.pub_asset_states <- function(release) {
  st <- vapply(release$assets, function(a) as.character(a$state), character(1L))
  names(st) <- vapply(release$assets, function(a) a$name, character(1L))
  st[order(names(st))]
}

# How each asset is served. An asset the fake uploaded carries the type its
# name's extension names, and one placed in the world's opening state does not,
# because nothing uploaded it.
.pub_asset_types <- function(release) {
  ct <- vapply(release$assets,
               function(a) as.character(a$content_type %||% NA_character_),
               character(1L))
  names(ct) <- vapply(release$assets, function(a) a$name, character(1L))
  ct[order(names(ct))]
}

# The four assets a reader asks for by name, with the copy a replacement sets
# aside and the one an interrupted replacement leaves left out.
.pub_live_sizes <- function(release) {
  sizes <- .pub_asset_sizes(release)
  sizes[!grepl("^swap-(prev|next)-", names(sizes))]
}

.pub_expect_whole <- function(world, tag) {
  r <- .pub_only(world, tag)
  expect_false(isTRUE(r$isDraft))
  expect_equal(.pub_live_sizes(r), .pub_assets[order(names(.pub_assets))])
  invisible(r)
}

.pub_today <- 'publish_metrics metrics-2026-09-13 "CRAN Metrics - 2026-09-13" || exit 1'

# ---------------------------------------------------------------------------
# Resolution
# ---------------------------------------------------------------------------

test_that("latest_tag skips a draft that sorts above every published release", {
  world <- .pub_world(list(
    .pub_release(10L, "code-2026-07-01", assets = .pub_assets[1L]),
    .pub_0912(), .pub_stranded_0913()))

  res <- .pub_run(world,
    'echo "metrics=$(latest_tag metrics)"',
    'echo "code=$(latest_tag code)"',
    'echo "data=$(latest_tag data)"')
  expect_equal(res$status, 0L)
  # The live shape of 2026-09-14: the draft sorts first and is not a release.
  expect_true("metrics=metrics-2026-09-12" %in% res$output)
  # The legacy split series still resolves, and an empty one is empty.
  expect_true("code=code-2026-07-01" %in% res$output)
  expect_true("data=" %in% res$output)
})

test_that("latest_tag keeps a pre-release as a baseline", {
  # This pipeline never makes a pre-release, so one here was marked by hand,
  # and skipping it would move the baseline back a day.
  # The fake drops pre-releases when told --exclude-pre-releases, so a listing
  # that leaves them out resolves 09-12 here and fails this.
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(3L, "metrics-2026-09-13", assets = .pub_assets, prerelease = TRUE)))
  res <- .pub_run(world, 'echo "metrics=$(latest_tag metrics)"')
  expect_equal(res$status, 0L)
  expect_true("metrics=metrics-2026-09-13" %in% res$output)
})

test_that("a listing latest_tag could not read stops the step instead of reading as no release", {
  # An empty answer is a cold start, and a cold start publishes an empty
  # database as latest.
  world <- .pub_world(list(.pub_0912()), faults = c(list = 99L))
  res <- .pub_run(world,
    'METRICS_TAG=$(latest_tag metrics)',
    'echo "resolved:${METRICS_TAG}"')
  expect_false(res$status == 0L)
  expect_false(any(grepl("^resolved:", res$output)))
  expect_length(grep("^gh release list", .pub_log(world)), 5L)
})

test_that("a listing latest_tag could not read once is read again", {
  # It is the first call to GitHub in every run, in the download step, so one
  # 500 on it stopped the run before anything else had a chance. The tag is
  # the function's stdout, so the attempt messages must stay out of it.
  world <- .pub_world(list(.pub_0912(), .pub_stranded_0913()),
                      faults = c(list = 1L))
  res <- .pub_run(world,
    'METRICS_TAG=$(latest_tag metrics)',
    'echo "resolved:<${METRICS_TAG}>"')
  expect_equal(res$status, 0L)
  expect_true("resolved:<metrics-2026-09-12>" %in% res$output)
  expect_true(any(grepl("attempt 1: could not list", res$output, fixed = TRUE)))
  expect_length(grep("^gh release list", .pub_log(world)), 2L)

  # 10 s, then 20 s, as release_state waits.
  world <- .pub_world(list(.pub_0912()), faults = c(list = 2L))
  slept <- file.path(world$dir, "slept")
  res <- .pub_run(world,
    "unset PUBLISH_RETRY_SECONDS",
    sprintf('sleep() { echo "$1" >> %s; }', shQuote(slept)),
    "latest_tag metrics >/dev/null || exit 1")
  expect_equal(res$status, 0L)
  expect_equal(readLines(slept), c("10", "20"))
})

# ---------------------------------------------------------------------------
# Publishing a new day's release
# ---------------------------------------------------------------------------

test_that("a draft stranded under today's tag is replaced and today's release is published whole", {
  world <- .pub_world(list(.pub_0912(), .pub_stranded_0913()))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)

  r <- .pub_expect_whole(world, "metrics-2026-09-13")
  expect_true(isTRUE(r$isLatest))
  expect_identical(r$name, "CRAN Metrics - 2026-09-13")
  log <- .pub_log(world)
  expect_true("gh release delete metrics-2026-09-13 --yes" %in% log)
  # A draft has no tag, so --cleanup-tag deletes it and then fails the step.
  expect_false(any(grepl("--cleanup-tag", log, fixed = TRUE)))

  res <- .pub_run(world, 'echo "metrics=$(latest_tag metrics)"')
  expect_true("metrics=metrics-2026-09-13" %in% res$output)
})

test_that("a new day's release is created empty and published only after its assets", {
  world <- .pub_world(list(.pub_0912()))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")

  log <- .pub_log(world)
  create <- grep("^gh release create", log, value = TRUE)
  expect_length(create, 1L)
  expect_true(grepl("--draft", create, fixed = TRUE))
  expect_false(grepl("\\.db|manifest\\.json", create))
  publish <- grep("--draft=false", log, fixed = TRUE)
  expect_length(publish, 1L)
  expect_true(grepl("--latest", log[publish], fixed = TRUE))
  expect_gt(publish, max(grep("^gh release upload", log)))
})

test_that("databases upload before manifests", {
  # A manifest newer than the database beside it is the pair preflight has to
  # refuse, so the order a publish can be interrupted in matters.
  world <- .pub_world(list(.pub_0912()))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  uploads <- grep("^gh release upload", .pub_log(world), value = TRUE)
  expect_length(uploads, 4L)
  db <- grep("\\.db ", uploads)
  manifest <- grep("manifest\\.json ", uploads)
  expect_length(db, 2L)
  expect_length(manifest, 2L)
  expect_lt(max(db), min(manifest))
})

test_that("an upload that fails twice is retried and the release is still published", {
  world <- .pub_world(list(.pub_0912()),
                      faults = c("upload-cran-data-metrics.db" = 2L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
  expect_length(grep("^gh release upload .*cran-data-metrics\\.db",
                     .pub_log(world)), 3L)
})

test_that("an upload that never lands fails the step, keeps yesterday as the baseline, and the next attempt publishes", {
  world <- .pub_world(list(.pub_0912()),
                      faults = c("upload-cran-code-metrics.db" = 99L))
  res <- .pub_run(world, .pub_today, 'echo "went on past the publish"')
  expect_false(res$status == 0L)
  expect_false("went on past the publish" %in% res$output)
  expect_length(grep("^gh release upload .*cran-code-metrics\\.db",
                     .pub_log(world)), 5L)

  res <- .pub_run(world, 'echo "metrics=$(latest_tag metrics)"')
  expect_true("metrics=metrics-2026-09-12" %in% res$output)

  .pub_set_faults(world, integer(0L))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
})

test_that("a publish edit that fails once is made again, and the release is published", {
  # The edit comes after every asset has uploaded and been checked, so one 500
  # on it threw away a verified upload, left a complete draft, and the next
  # run repeated the day's analysis.
  world <- .pub_world(list(.pub_0912()), faults = c(edit = 1L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  r <- .pub_expect_whole(world, "metrics-2026-09-13")
  expect_true(isTRUE(r$isLatest))
  log <- .pub_log(world)
  expect_length(grep("--draft=false", log, fixed = TRUE), 2L)
  expect_length(grep("^gh release create", log), 1L)
  expect_false(any(grepl("^gh release delete", log)))
})

test_that("a publish edit that applied but reported failure is safe to make again", {
  # A PATCH that returns 500 can still have landed. The next attempt finds the
  # now published release under the tag and sets the same fields on it.
  world <- .pub_world(list(.pub_0912()), faults = c("edit-after" = 1L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  r <- .pub_expect_whole(world, "metrics-2026-09-13")
  expect_true(isTRUE(r$isLatest))
  expect_false(isTRUE(.pub_only(world, "metrics-2026-09-12")$isLatest))
  expect_length(grep("--draft=false", .pub_log(world), fixed = TRUE), 2L)
})

test_that("a publish edit that never lands leaves a draft that the next attempt replaces", {
  world <- .pub_world(list(.pub_0912()), faults = c(edit = 99L))
  expect_false(.pub_run(world, .pub_today)$status == 0L)
  expect_length(grep("--draft=false", .pub_log(world), fixed = TRUE), 5L)
  expect_true(isTRUE(.pub_only(world, "metrics-2026-09-13")$isDraft))
  res <- .pub_run(world, 'echo "metrics=$(latest_tag metrics)"')
  expect_true("metrics=metrics-2026-09-12" %in% res$output)

  .pub_set_faults(world, integer(0L))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
})

test_that("an edit waits ten seconds longer after each failed attempt", {
  world <- .pub_world(list(.pub_0912()), faults = c(edit = 2L))
  slept <- file.path(world$dir, "slept")
  res <- .pub_run(world,
    "unset PUBLISH_RETRY_SECONDS",
    sprintf('sleep() { echo "$1" >> %s; }', shQuote(slept)),
    .pub_today)
  expect_equal(res$status, 0L)
  expect_equal(readLines(slept), c("10", "20"))
})

test_that("a draft that will not delete is not deleted again in the same call", {
  # The delete goes by tag, and when the listing has not caught up with a
  # release published under the same tag it can take that release instead, so
  # it is not repeated. The draft is left to the next publish.
  world <- .pub_world(list(.pub_0912(), .pub_stranded_0913()),
                      faults = c(delete = 1L))
  expect_false(.pub_run(world, .pub_today)$status == 0L)
  expect_length(grep("^gh release delete", .pub_log(world)), 1L)
  expect_false(any(grepl("^gh release (create|upload|edit)", .pub_log(world))))
})

test_that("a create that failed after making the draft is not retried, and the next attempt replaces it", {
  # A POST that comes back 500 may still have made the release, so trying
  # again in the same run can make a second one under the same tag.
  world <- .pub_world(list(.pub_0912()), faults = c("create-after" = 1L))
  expect_false(.pub_run(world, .pub_today)$status == 0L)
  expect_length(grep("^gh release create", .pub_log(world)), 1L)
  expect_true(isTRUE(.pub_only(world, "metrics-2026-09-13")$isDraft))

  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
})

test_that("an asset that lands the wrong size is refused and today's release stays unpublished", {
  world <- .pub_world(list(.pub_0912()),
                      faults = c("short-cran-data-metrics.db" = 99L))
  res <- .pub_run(world, .pub_today)
  expect_false(res$status == 0L)
  expect_true(any(grepl("cran-data-metrics.db at 3000 bytes", res$output, fixed = TRUE)))
  expect_true(isTRUE(.pub_only(world, "metrics-2026-09-13")$isDraft))
  expect_false(any(grepl("--draft=false", .pub_log(world), fixed = TRUE)))
  # Read five times first, in case the release had not caught up.
  expect_length(grep("^gh release view", .pub_log(world)), 5L)
})

test_that("a read-back that has not caught up with the uploads is read again, not refused", {
  # Nothing promises that a release lists an asset the moment its upload
  # returns. Refusing on the first read that disagrees fails a publish whose
  # assets are all there, and the next run repeats the whole day's work.
  world <- .pub_world(list(.pub_0912()),
                      faults = c("stale-data-manifest.json" = 2L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  r <- .pub_expect_whole(world, "metrics-2026-09-13")
  expect_true(isTRUE(r$isLatest))
  expect_length(grep("^gh release view", .pub_log(world)), 3L)
})

test_that("two releases under today's tag are refused before anything is touched", {
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets),
    .pub_stranded_0913(id = 3L)))
  res <- .pub_run(world, .pub_today)
  expect_false(res$status == 0L)
  expect_true(any(grepl("more than one release", res$output, fixed = TRUE)))
  expect_false(any(grepl("^gh release (upload|create|delete|edit)", .pub_log(world))))
  expect_false(any(grepl("-X DELETE", .pub_log(world), fixed = TRUE)))
})

test_that("the refusal over two releases under one tag names them by id, not by tag", {
  # `gh release delete TAG` looks the tag up as a published release and as a
  # draft at the same time and deletes whichever answer arrives first, so the
  # advice to run it could take the published release and keep the draft.
  twins <- list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets),
    .pub_stranded_0913(id = 3L))
  world <- .pub_world(twins)
  res <- .pub_run(world, .pub_today)
  expect_false(res$status == 0L)
  expect_true(any(grepl("id 2: published", res$output, fixed = TRUE)))
  expect_true(any(grepl("id 3: draft", res$output, fixed = TRUE)))
  expect_false(any(grepl("id 1:", res$output, fixed = TRUE)))
  expect_true(any(grepl("gh api -X DELETE repos/{owner}/{repo}/releases/<id>",
                        res$output, fixed = TRUE)))
  expect_false(any(grepl("gh release delete", res$output, fixed = TRUE)))
  expect_length(.pub_state(world), 3L)

  # The ids could not be read, five times over: still refused, and it says
  # where to find them.
  world <- .pub_world(twins, faults = c(api = 5L))
  res <- .pub_run(world, .pub_today)
  expect_false(res$status == 0L)
  expect_true(any(grepl("could not list their ids", res$output, fixed = TRUE)))
  expect_length(.pub_state(world), 3L)
})

# A `stat` that answers the way GNU coreutils does on the runner, whatever this
# machine has: -c takes a format, and -f means --file-system and takes none, so
# the BSD form `-f%z` is a usage error.
.pub_gnu_stat <- function(world) {
  writeLines(c(
    "#!/usr/bin/env bash",
    'case "$1" in',
    '  -c%s)',
    '    [ -e "$2" ] || { echo "stat: cannot statx $2: No such file or directory" >&2; exit 1; }',
    '    wc -c < "$2" | tr -d " " ;;',
    "  *) echo \"stat: invalid option -- '%'\" >&2; exit 1 ;;",
    "esac"), file.path(world$bin, "stat"))
  Sys.chmod(file.path(world$bin, "stat"), mode = "0755")
}

test_that("a file missing from out/ is named before the release is touched", {
  # file_bytes tries GNU stat and then BSD stat, and on the runner the BSD form
  # is a usage error, so a missing database failed the step with nothing but
  # "stat: invalid option -- '%'". A missing manifest is not measured for the
  # size budget at all: it got as far as an empty draft and five uploads.
  for (missing in c("cran-data-metrics.db", "data-manifest.json")) {
    world <- .pub_world(list(.pub_0912()))
    .pub_gnu_stat(world)
    unlink(file.path(world$work, "out", missing))
    res <- .pub_run(world, .pub_today)
    expect_false(res$status == 0L)
    expect_true(any(grepl(sprintf("::error::out/%s does not exist", missing),
                          res$output, fixed = TRUE)))
    expect_false(any(grepl("invalid option", res$output, fixed = TRUE)))
    expect_length(.pub_log(world), 0L)
  }
})

test_that("a database over the size budget is refused before the release is touched", {
  world <- .pub_world(list(.pub_0912()))
  res <- .pub_run(world, .pub_today, env = c(PUBLISH_MAX_BYTES = "4000"))
  expect_false(res$status == 0L)
  expect_true(any(grepl("cran-code-metrics.db is 5000 bytes", res$output, fixed = TRUE)))
  expect_length(.pub_log(world), 0L)
})

# ---------------------------------------------------------------------------
# Republishing a release that is already out
# ---------------------------------------------------------------------------

test_that("a later shard replaces today's published assets in place", {
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L, latest = TRUE)))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  r <- .pub_expect_whole(world, "metrics-2026-09-13")
  expect_identical(r$body, "notes for today")
  expect_identical(r$id, 2L)
  expect_false(any(grepl("^gh release (create|delete)", .pub_log(world))))
})

test_that("a same-day notes edit that fails once is made again", {
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L, latest = TRUE)),
    faults = c(edit = 1L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  r <- .pub_expect_whole(world, "metrics-2026-09-13")
  expect_identical(r$body, "notes for today")
  expect_length(grep("^gh release edit", .pub_log(world)), 2L)
})

test_that("a replacement that never uploads fails the step", {
  # bash ignores `set -e` inside a function called as `f || exit 1`, which is
  # how the shard loop calls publish_metrics. A failed upload followed by a
  # notes edit that worked returned 0, and the run went green with today's
  # release missing its database.
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L, latest = TRUE)),
    faults = c("upload-swap-next-cran-code-metrics.db" = 99L))
  res <- .pub_run(world, .pub_today, 'echo "went on past the publish"')
  expect_false(res$status == 0L)
  expect_false("went on past the publish" %in% res$output)
  expect_false(any(grepl("^gh release edit", .pub_log(world))))
})

test_that("a listing that fails once is read again rather than taken as no release", {
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L, latest = TRUE)),
    faults = c(list = 1L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  expect_false(any(grepl("^gh release create", .pub_log(world))))
  .pub_expect_whole(world, "metrics-2026-09-13")
})

test_that("the harvest upload goes only into a published release", {
  harvest <- 'replace_published_asset metrics-2026-09-13 out/cran-code-metrics.db || exit 1'

  # No release today: nothing to update.
  world <- .pub_world(list(.pub_0912()))
  res <- .pub_run(world, harvest)
  expect_false(res$status == 0L)
  expect_false(any(grepl("^gh release upload", .pub_log(world))))

  # A draft today was never published, and uploading into it publishes nothing.
  world <- .pub_world(list(.pub_0912(), .pub_stranded_0913()))
  res <- .pub_run(world, harvest)
  expect_false(res$status == 0L)
  expect_false(any(grepl("^gh release upload", .pub_log(world))))
  expect_length(.pub_only(world, "metrics-2026-09-13")$assets, 2L)

  # A published release gets the database replaced, retried through a 500.
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L, latest = TRUE)),
    faults = c("upload-swap-next-cran-code-metrics.db" = 1L))
  res <- .pub_run(world, harvest)
  expect_equal(res$status, 0L)
  sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
  expect_equal(sizes[["cran-code-metrics.db"]], 5000L)
  expect_equal(sizes[["cran-data-metrics.db"]], 2993L)
  # The copy it replaced is left for a download that is already running.
  expect_equal(sizes[["swap-prev-cran-code-metrics.db"]], 4993L)
})

test_that("the harvest names a database missing from out/ before the release is touched", {
  # The harvest went straight to the listing and the upload, so a database the
  # harvest did not leave behind cost five upload attempts, about 300 s of
  # backoff on the runner, and an error that named the upload, not the file.
  harvest <- 'replace_published_asset metrics-2026-09-13 out/cran-code-metrics.db || exit 1'
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L, latest = TRUE)))
  .pub_gnu_stat(world)
  unlink(file.path(world$work, "out", "cran-code-metrics.db"))
  res <- .pub_run(world, harvest)
  expect_false(res$status == 0L)
  expect_true(any(grepl("::error::out/cran-code-metrics.db does not exist",
                        res$output, fixed = TRUE)), info = res$output)
  expect_false(any(grepl("invalid option", res$output, fixed = TRUE)))
  expect_false(any(grepl("did not upload", res$output, fixed = TRUE)))
  expect_length(.pub_log(world), 0L)
})

test_that("the harvest refuses two releases under one tag and names them by id", {
  # The refusal is the only thing that tells the operator how to clear the
  # tag, and a delete by tag can take the published release and keep the draft.
  harvest <- 'replace_published_asset metrics-2026-09-13 out/cran-code-metrics.db || exit 1'
  twins <- list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L),
    .pub_stranded_0913(id = 3L))
  world <- .pub_world(twins)
  res <- .pub_run(world, harvest)
  expect_false(res$status == 0L)
  expect_true(any(grepl("more than one release", res$output, fixed = TRUE)))
  expect_true(any(grepl("id 2: published", res$output, fixed = TRUE)))
  expect_true(any(grepl("id 3: draft", res$output, fixed = TRUE)))
  expect_false(any(grepl("id 1:", res$output, fixed = TRUE)))
  expect_true(any(grepl("gh api -X DELETE repos/{owner}/{repo}/releases/<id>",
                        res$output, fixed = TRUE)))
  expect_false(any(grepl("gh release delete", res$output, fixed = TRUE)))
  expect_false(any(grepl("^gh release (upload|create|delete|edit)", .pub_log(world))))
  expect_false(any(grepl("-X DELETE", .pub_log(world), fixed = TRUE)))
  expect_length(.pub_state(world), 3L)

  # The ids could not be read, five times over: still refused, and it says
  # where to find them.
  world <- .pub_world(twins, faults = c(api = 5L))
  res <- .pub_run(world, harvest)
  expect_false(res$status == 0L)
  expect_true(any(grepl("could not list their ids", res$output, fixed = TRUE)))
  expect_false(any(grepl("^gh release upload", .pub_log(world))))
  expect_length(.pub_state(world), 3L)
})

# ---------------------------------------------------------------------------
# Replacing an asset of a release that is already out
# ---------------------------------------------------------------------------
# A same-day republish used to upload each file with `gh release upload
# --clobber`, which deletes the live asset and only then uploads its
# replacement. When every attempt failed, the published release was left
# without that asset for good: it is still `latest_tag metrics` tomorrow, so
# the download step came back with a manifest and no database and preflight
# refused every run after it.
#
# The replacement uploads under a temporary name and swaps the names once the
# bytes are on the release, so the gap in which a reader finds no asset of that
# name is one rename wide rather than one upload wide, and the copy it replaces
# stays under swap-prev-NAME for a download that is already running.
#
# The fault names say where a run was interrupted:
#   upload-swap-next-NAME   the temporary upload never lands
#   starter-swap-next-NAME  it is cut off part way, leaving a half-uploaded asset
#   rename-to-swap-prev-NAME  the rename that moves the live asset out of the way
#   rename-to-NAME       the rename that gives the new bytes the name, and the
#                        rollback that puts the old asset back

.pub_db <- "cran-code-metrics.db"

.pub_out_today <- function(id = 2L, extra = list()) {
  .pub_release(id, "metrics-2026-09-13", assets = .pub_assets - 7L,
               latest = TRUE, extra = extra)
}

# What a reader that asks for one asset by name gets: its size, or "none".
.pub_read_by_name <- function(world, tag, name) {
  res <- .pub_run(world, sprintf(paste0(
    'rm -rf dl; if gh release download %s -p %s -D dl 2>/dev/null; ',
    'then echo "bytes=$(wc -c < dl/%s)"; else echo "bytes=none"; fi'),
    tag, name, name))
  line <- grep("^bytes=", res$output, value = TRUE)
  gsub("[^0-9a-z]", "", sub("^bytes=", "", line))
}

# The asset name each `gh api -X PATCH ... -f name=X` in the log asked for.
.pub_renames <- function(world) {
  sub(".*-f name=", "", grep("-X PATCH .*-f name=", .pub_log(world), value = TRUE))
}

# What out/.publish-stage holds afterwards: the links a replacement uploads
# through, dangling once out/ is cleared.
.pub_stage_left <- function(world) {
  list.files(file.path(world$work, "out", ".publish-stage"),
             all.files = TRUE, no.. = TRUE)
}

test_that("a republish never takes the live asset away before its replacement is there", {
  world <- .pub_world(list(.pub_0912(), .pub_out_today()))
  before <- .pub_asset_ids(.pub_only(world, "metrics-2026-09-13"))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  r <- .pub_expect_whole(world, "metrics-2026-09-13")

  # Every upload goes under the temporary name.
  uploads <- grep("^gh release upload", .pub_log(world), value = TRUE)
  expect_length(uploads, 4L)
  expect_true(all(grepl("/swap-next-[^/]+ --clobber$", uploads)))

  # The asset a reader asks for by name is a different asset now, and the one
  # it replaced kept its id, set aside, rather than being deleted.
  ids <- .pub_asset_ids(r)
  sizes <- .pub_asset_sizes(r)
  expect_false(ids[[.pub_db]] %in% before)
  expect_equal(ids[[paste0("swap-prev-", .pub_db)]], before[[.pub_db]])
  expect_equal(sizes[[paste0("swap-prev-", .pub_db)]], 4993L)
  expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db), "5000")
  expect_false(any(grepl("-X DELETE", .pub_log(world), fixed = TRUE)))
})

test_that("a republish gives the databases their names before the manifests", {
  # A manifest newer than the database beside it is the pair preflight has to
  # refuse, so the order the renames land in matters as much as the uploads.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  named <- .pub_renames(world)
  expect_equal(named, c(
    paste0("swap-prev-", .pub_db), .pub_db,
    "swap-prev-cran-data-metrics.db", "cran-data-metrics.db",
    "swap-prev-code-manifest.json", "code-manifest.json",
    "swap-prev-data-manifest.json", "data-manifest.json"))
})

test_that("a replacement stopped after the temporary upload leaves the live asset in place", {
  for (fault in c("upload-swap-next-cran-code-metrics.db",
                  "starter-swap-next-cran-code-metrics.db",
                  "rename-to-swap-prev-cran-code-metrics.db")) {
    world <- .pub_world(list(.pub_0912(), .pub_out_today()),
                        faults = stats::setNames(99L, fault))
    expect_false(.pub_run(world, .pub_today)$status == 0L)

    # The published database is untouched, and still what a reader gets.
    sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
    expect_equal(sizes[[.pub_db]], 4993L, info = fault)
    expect_false(paste0("swap-prev-", .pub_db) %in% names(sizes), info = fault)
    expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db), "4993")

    # The next run clears what it left and publishes the day whole.
    .pub_set_faults(world, integer(0L))
    expect_equal(.pub_run(world, .pub_today)$status, 0L)
    .pub_expect_whole(world, "metrics-2026-09-13")
  }
})

test_that("a replacement stopped between the two renames rolls the live asset back", {
  # The first rename landed, so the name a reader asks for is on the asset the
  # run was replacing. Nothing else has been swapped, so the day is exactly
  # where it started: the old database beside the old manifest.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()),
                      faults = c("rename-to-cran-code-metrics.db" = 5L))
  expect_false(.pub_run(world, .pub_today)$status == 0L)
  sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
  expect_equal(sizes[[.pub_db]], 4993L)
  expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db), "4993")

  .pub_set_faults(world, integer(0L))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
})

test_that("a rollback that also fails leaves the bytes set aside for the next run", {
  # This is the one state that hurts: the name is on nothing, so a reader gets
  # "no assets match the file pattern" and preflight sees the asset as absent.
  # The bytes are still there, and the next replacement puts the name back on
  # them before it does anything else.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()),
                      faults = c("rename-to-cran-code-metrics.db" = 99L))
  expect_false(.pub_run(world, .pub_today)$status == 0L)
  sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
  expect_false(.pub_db %in% names(sizes))
  expect_equal(sizes[[paste0("swap-prev-", .pub_db)]], 4993L)
  expect_equal(sizes[[paste0("swap-next-", .pub_db)]], 5000L)
  expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db), "none")

  .pub_set_faults(world, integer(0L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
  # It put the name back on the published bytes rather than taking the staged
  # upload it found: the manifests were not swapped, so the database that
  # upload holds is not the one they describe.
  expect_true(any(grepl(
    sprintf("putting the name back on swap-prev-%s", .pub_db), res$output, fixed = TRUE)))
})

test_that("every leftover an interrupted replacement can leave is put right by the next one", {
  # Each of these was built on a real release, read the way the merger reads
  # it, and repaired. The repair runs before the replacement touches anything.
  old <- .pub_assets[[.pub_db]] - 7L
  new <- .pub_assets[[.pub_db]]
  live <- .pub_asset(250L, .pub_db, old)
  prev <- .pub_asset(251L, paste0("swap-prev-", .pub_db), old)
  half <- .pub_asset(252L, paste0("swap-next-", .pub_db), new, state = "starter")
  whole <- .pub_asset(253L, paste0("swap-next-", .pub_db), new)
  half_live <- .pub_asset(254L, .pub_db, new, state = "starter")

  leftovers <- list(
    # Stopped during the temporary upload: the live asset is correct, and a
    # half-uploaded asset is left that gh itself cannot see.
    "cut-off upload"   = list(extra = list(half), read = old),
    # Stopped after it: a complete staged upload, which would refuse the next.
    "complete staged"  = list(extra = list(whole), read = old),
    # Stopped after the swap: the old copy is still set aside.
    "swapped already"  = list(extra = list(prev), read = old),
    # Stopped between the renames: the name is on nothing.
    "between renames"  = list(extra = list(prev, whole), read = NA,
                              drop_live = TRUE),
    # The name is on nothing and only the new bytes are left.
    "only staged"      = list(extra = list(whole), read = NA, drop_live = TRUE),
    # The live name itself holds an upload that was cut off. Its bytes are not
    # servable, so it is not the copy a reader is on and must not be moved
    # aside as one: with the copy the last replacement left beside it, and
    # without, which leaves this replacement nothing to move aside at all.
    "cut-off live"     = list(extra = list(half_live, prev), read = NA,
                              drop_live = TRUE),
    "cut-off live only" = list(extra = list(half_live), read = NA,
                               drop_live = TRUE, prevs = 0L))

  for (nm in names(leftovers)) {
    case <- leftovers[[nm]]
    assets <- if (isTRUE(case$drop_live)) .pub_assets[-1L] else .pub_assets
    world <- .pub_world(list(.pub_0912(), .pub_release(
      2L, "metrics-2026-09-13", assets = assets - 7L, latest = TRUE,
      extra = case$extra)))
    if (isTRUE(case$drop_live)) {
      expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db),
                   "none", info = nm)
    }
    expect_equal(.pub_run(world, .pub_today)$status, 0L, info = nm)
    r <- .pub_expect_whole(world, "metrics-2026-09-13")
    # Nothing of the interrupted run is left but the copy this one replaced,
    # and nothing half written is left under any name: a replacement that
    # moved one aside would put the rollback on bytes that cannot be served.
    sizes <- .pub_asset_sizes(r)
    expect_false(paste0("swap-next-", .pub_db) %in% names(sizes), info = nm)
    expect_equal(sum(names(sizes) == paste0("swap-prev-", .pub_db)),
                 case$prevs %||% 1L, info = nm)
    expect_false("starter" %in% .pub_asset_states(r), info = nm)
  }
})

test_that("a leftover with nothing left to serve is named, and the run goes on to upload", {
  # Only a half-uploaded staged copy: its bytes are not servable, so there is
  # nothing to put the name back on. The replacement is about to supply the
  # asset anyway, so it says so and carries on.
  world <- .pub_world(list(.pub_0912(), .pub_release(
    2L, "metrics-2026-09-13", assets = .pub_assets[-1L] - 7L, latest = TRUE,
    extra = list(.pub_asset(252L, paste0("swap-next-", .pub_db), 5000L,
                            state = "starter")))))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  expect_true(any(grepl(sprintf("swap-next-%s was cut off", .pub_db),
                        res$output, fixed = TRUE)))
  .pub_expect_whole(world, "metrics-2026-09-13")
})

test_that("a repair asked for twice does nothing the second time", {
  world <- .pub_world(list(.pub_0912(), .pub_out_today(extra = list(
    .pub_asset(251L, paste0("swap-prev-", .pub_db), 4993L)))))
  step <- c('REL=$(release_id metrics-2026-09-13) || exit 1',
            sprintf('repair_asset metrics-2026-09-13 "$REL" %s || exit 1', .pub_db),
            'echo "---"',
            sprintf('repair_asset metrics-2026-09-13 "$REL" %s || exit 1', .pub_db))
  res <- .pub_run(world, step)
  expect_equal(res$status, 0L)
  after <- res$output[seq(which(res$output == "---") + 1L, length(res$output))]
  expect_false(any(grepl("clearing", after, fixed = TRUE)))
  expect_equal(names(.pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))),
               sort(names(.pub_assets)))
})

test_that("a temporary name is not what a reader asking for the database gets", {
  # `gh release download -p NAME` matches the asset name exactly, so the
  # copies a replacement leaves are invisible to the merger and to the
  # download step, which both ask for the databases by name.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db), "5000")
  res <- .pub_run(world,
    'gh release download metrics-2026-09-13 -p cran-code-metrics.db -D dl || exit 1',
    'ls dl')
  expect_equal(res$status, 0L)
  expect_equal(grep("cran-code", res$output, value = TRUE), .pub_db)
})

test_that("a republished manifest is still served as the type its extension names", {
  # An asset is served as the type its upload declared, and gh declares it
  # from the extension of the path it uploads, so a temporary name appended
  # after the extension leaves code-manifest.json served as
  # application/octet-stream from the first same-day republish until the
  # release is gone. A temporary name in front of it keeps the extension where
  # gh looks. .json is in Go's own mime table, so the manifests below hold
  # anywhere; the database's type is what this laptop's tables give, which is
  # what the fake models.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  types <- .pub_asset_types(.pub_expect_whole(world, "metrics-2026-09-13"))
  expect_equal(types[["code-manifest.json"]], "application/json")
  expect_equal(types[["data-manifest.json"]], "application/json")
  expect_equal(types[[.pub_db]], "application/octet-stream")
})

test_that("a reader asking for a name with a trailing wildcard gets only the asset itself", {
  # `-p NAME*` is how a reader asks for an asset whose name carries a version
  # or a date it does not know, and filepath.Match then takes anything the
  # name starts. A temporary name appended to the real one is inside that
  # answer, so such a reader is handed the copy a replacement set aside beside
  # the asset it asked for. A temporary name in front of it is not.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  res <- .pub_run(world,
    "gh release download metrics-2026-09-13 -p 'code-manifest*' -D dl || exit 1",
    'ls dl')
  expect_equal(res$status, 0L)
  expect_equal(grep("code-manifest", res$output, value = TRUE), "code-manifest.json")
})

test_that("the link a replacement uploads through is taken down again", {
  # The upload goes through a link that gives the database the temporary name
  # without copying 1.9 GB of it. Each is named after the asset it stages, so
  # one left behind survives the run that made it, points at a file the next
  # run overwrites, and is what a later run's upload of the same name follows.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()))
  expect_equal(.pub_run(world, .pub_today)$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
  expect_equal(.pub_stage_left(world), character(0L))

  # And a refusal leaves nothing behind either, wherever it stops: before the
  # upload lands, after the check reads bytes it refuses, or between the two
  # renames, with the name on neither asset.
  for (fault in c("upload-swap-next-cran-code-metrics.db",
                  "short-swap-next-cran-code-metrics.db",
                  "rename-to-cran-code-metrics.db")) {
    world <- .pub_world(list(.pub_0912(), .pub_out_today()),
                        faults = stats::setNames(99L, fault))
    expect_false(.pub_run(world, .pub_today)$status == 0L, info = fault)
    expect_equal(.pub_stage_left(world), character(0L), info = fault)
  }
})

test_that("a link that will not come down is named, and answers for nothing else", {
  # The link is this machine's and the asset is on the release whatever
  # becomes of it, so a `rm` that fails must not turn a replacement that landed
  # into a run that failed, which is the day's release thrown away and the
  # analysis repeated.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()))
  writeLines(c("#!/usr/bin/env bash",
               'case "$*" in',
               '  *.publish-stage*) echo "rm: read-only file system" >&2; exit 1 ;;',
               'esac',
               'exec /bin/rm "$@"'), file.path(world$bin, "rm"))
  Sys.chmod(file.path(world$bin, "rm"), mode = "0755")

  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  .pub_expect_whole(world, "metrics-2026-09-13")
  expect_length(grep("^::warning::could not remove out/\\.publish-stage/swap-next-",
                     res$output), 4L)
})

test_that("a replacement that lands short or with other bytes is refused, and the live asset stays", {
  for (fault in c("short-swap-next-cran-code-metrics.db",
                  "digest-swap-next-cran-code-metrics.db")) {
    world <- .pub_world(list(.pub_0912(), .pub_out_today()),
                        faults = stats::setNames(99L, fault))
    res <- .pub_run(world, .pub_today)
    expect_false(res$status == 0L, info = fault)
    expect_true(any(grepl(sprintf("swap-next-%s", .pub_db), res$output, fixed = TRUE)),
                info = fault)
    sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
    expect_equal(sizes[[.pub_db]], 4993L, info = fault)
    expect_length(.pub_renames(world), 0L)
    # The bytes it refused are taken off the release, not left under the
    # temporary name for a later repair to find.
    expect_false(paste0("swap-next-", .pub_db) %in% names(sizes), info = fault)
  }
})

test_that("the bytes a replacement refused are never what a later repair gives the name to", {
  # The check after the upload is the one place that proves an asset wrong,
  # and the repair gives NAME to a whole swap-next-NAME when the release
  # carries no NAME. So a staged upload the check refused must not be there for
  # it to find: this
  # release lost its database to an upload that was cut off, which the repair
  # clears, and the replacement that follows lands wrong.
  for (fault in c("short-swap-next-cran-code-metrics.db",
                  "digest-swap-next-cran-code-metrics.db")) {
    world <- .pub_world(list(.pub_0912(), .pub_release(
      2L, "metrics-2026-09-13", assets = .pub_assets[-1L] - 7L, latest = TRUE,
      extra = list(.pub_asset(252L, paste0("swap-next-", .pub_db), 5000L,
                              state = "starter")))),
      faults = stats::setNames(99L, fault))
    expect_false(.pub_run(world, .pub_today)$status == 0L, info = fault)
    sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
    expect_false(paste0("swap-next-", .pub_db) %in% names(sizes), info = fault)

    # The next run reads the release before it builds on it, and finds
    # nothing to put the name back on rather than the bytes that were refused.
    .pub_set_faults(world, integer(0L))
    res <- .pub_run(world, sprintf(
      "repair_release_assets metrics-2026-09-13 %s || exit 1", .pub_db))
    expect_equal(res$status, 0L, info = fault)
    expect_true(any(grepl(
      sprintf("::warning::metrics-2026-09-13 carries no %s and nothing a replacement left aside",
              .pub_db), res$output, fixed = TRUE)), info = fault)
    expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db),
                 "none", info = fault)
  }
})

test_that("an upload that exits zero on bytes the release cannot serve is refused", {
  # gh exiting 0 is not proof the bytes are servable: one 300 MiB upload did
  # that and a download of the asset still answered BlobNotFound 24.5 s later.
  # A half-written asset carries the full declared size, so the size alone
  # cannot tell the two apart; the release has to say the asset is whole.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()),
                      faults = c("lied-swap-next-cran-code-metrics.db" = 99L))
  res <- .pub_run(world, .pub_today)
  expect_false(res$status == 0L)
  expect_length(grep("^attempt [1-5]: metrics-2026-09-13 does not list swap-next-cran-code-metrics.db",
                     res$output), 5L)
  expect_true(any(grepl(
    "::error::metrics-2026-09-13 does not carry swap-next-cran-code-metrics.db",
    res$output, fixed = TRUE)))

  # The published database keeps its name and its bytes, nothing was renamed,
  # and the asset that was not servable is gone.
  sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
  expect_equal(sizes[[.pub_db]], 4993L)
  expect_length(.pub_renames(world), 0L)
  expect_false(paste0("swap-next-", .pub_db) %in% names(sizes))
  expect_equal(.pub_read_by_name(world, "metrics-2026-09-13", .pub_db), "4993")
})

test_that("a swap the release has not caught up with is read again, and refused if it never shows", {
  # A listing taken inside 40 ms of a rename that answered 200 has been seen
  # to still show the state before it, and refusing there would roll back a
  # swap that worked. Three reads come before the read-back: the repair's, the
  # check after the upload, and the one that finds the two ids. This release
  # carries no database, so the reads that hide the name before the swap
  # change nothing.
  swap <- c('REL=$(release_id metrics-2026-09-13) || exit 1',
            sprintf('swap_asset metrics-2026-09-13 "$REL" out/%s || exit 1', .pub_db))
  missing <- list(.pub_0912(), .pub_release(
    2L, "metrics-2026-09-13", assets = .pub_assets[-1L] - 7L, latest = TRUE))

  world <- .pub_world(missing, faults = c("stale-cran-code-metrics.db" = 4L))
  res <- .pub_run(world, swap)
  expect_equal(res$status, 0L)
  expect_true(any(grepl(
    "attempt 1: metrics-2026-09-13 lists cran-code-metrics.db as [nothing]",
    res$output, fixed = TRUE)))
  expect_equal(.pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))[[.pub_db]],
               5000L)
  # Nothing was moved aside, so the line that says what happened says so.
  expect_true(any(grepl("the release carried no cran-code-metrics.db before this run",
                        res$output, fixed = TRUE)))
  expect_false(any(grepl(paste0("swap-prev-", .pub_db), res$output, fixed = TRUE)))

  world <- .pub_world(missing, faults = c("stale-cran-code-metrics.db" = 99L))
  res <- .pub_run(world, swap)
  expect_false(res$status == 0L)
  expect_true(any(grepl(
    "::error::metrics-2026-09-13 lists cran-code-metrics.db as [nothing] after the swap",
    res$output, fixed = TRUE)))
})

test_that("a live name the release does not list as whole is cleared, whatever state it is in", {
  # The state of an upload that was cut off measures as "starter", which the
  # documented states (uploaded, open) do not carry, so the set of them is not
  # closed. This is the test that decides whether the copy set aside, the
  # only other one there is, gets deleted, so only "uploaded" counts as live.
  for (state in c("starter", "open")) {
    world <- .pub_world(list(.pub_0912(), .pub_release(
      2L, "metrics-2026-09-13", assets = .pub_assets[-1L] - 7L, latest = TRUE,
      extra = list(.pub_asset(254L, .pub_db, 5000L, state = state),
                   .pub_asset(251L, paste0("swap-prev-", .pub_db), 4993L)))))
    res <- .pub_run(world, c(
      "REL=$(release_id metrics-2026-09-13) || exit 1",
      sprintf('repair_asset metrics-2026-09-13 "$REL" %s || exit 1', .pub_db)))
    expect_equal(res$status, 0L, info = state)
    r <- .pub_only(world, "metrics-2026-09-13")
    # The name goes back on the copy a reader can be served, and the asset
    # that was not whole is gone rather than the copy that was.
    expect_equal(.pub_asset_ids(r)[[.pub_db]], 251L, info = state)
    expect_false(254L %in% .pub_asset_ids(r), info = state)
    expect_equal(.pub_asset_sizes(r)[[.pub_db]], 4993L, info = state)
  }
})

test_that("a replacement believes the release rather than the upload's exit status", {
  # An upload that returns 0 is usually servable in the same instant, but once
  # it was not, so state, size and digest are read back from the release. A
  # read that has not caught up with the upload is read again rather than
  # refused: refusing there fails a replacement whose bytes are all present.
  # One of the three reads it hides is the repair's, before the upload.
  world <- .pub_world(list(.pub_0912(), .pub_out_today()),
                      faults = c("stale-swap-next-cran-code-metrics.db" = 3L))
  res <- .pub_run(world, .pub_today)
  expect_equal(res$status, 0L)
  expect_true(any(grepl("attempt 2: metrics-2026-09-13 does not list swap-next-cran-code-metrics.db",
                        res$output, fixed = TRUE)))
  .pub_expect_whole(world, "metrics-2026-09-13")
})

test_that("the copies a replacement leaves are swept off the releases nothing publishes again", {
  # Every asset that gets replaced leaves the copy it replaced behind, and the
  # last replacement of a day is never revisited: without this sweep each kept
  # release carries a second database for good. Today's is left alone, because
  # deleting an asset cuts off a download of it that is already running.
  world <- .pub_world(list(
    # The legacy split series the prune is told to leave alone. Nothing here
    # publishes to it again, and what a replacement of its own left is not
    # this sweep's to put right.
    .pub_release(10L, "code-2026-07-01", assets = .pub_assets[1L] - 2000L,
                 extra = list(.pub_asset(170L, paste0("swap-prev-", .pub_db), 2500L))),
    .pub_release(1L, "metrics-2026-09-12", assets = .pub_assets - 1000L,
                 latest = FALSE, extra = list(
                   .pub_asset(150L, paste0("swap-prev-", .pub_db), 3900L),
                   .pub_asset(151L, "swap-next-code-manifest.json", 40L))),
    .pub_release(3L, "metrics-2026-09-11",
                 assets = .pub_assets[-1L] - 1500L, extra = list(
                   .pub_asset(160L, paste0("swap-prev-", .pub_db), 3400L))),
    .pub_out_today(extra = list(.pub_asset(250L, paste0("swap-prev-", .pub_db), 4993L)))))

  res <- .pub_run(world, "sweep_swap_leftovers metrics metrics-2026-09-13 || exit 1")
  expect_equal(res$status, 0L)

  # The day before yesterday lost the name a swap never finished giving back;
  # the sweep puts it on the bytes that are still there.
  eleven <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-11"))
  expect_equal(eleven[[.pub_db]], 3400L)
  expect_false(any(grepl("\\.(prev|next)$", names(eleven))))
  twelve <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-12"))
  expect_equal(twelve, .pub_assets[order(names(.pub_assets))] - 1000L)
  # Today keeps its copy: tomorrow's sweep takes it.
  thirteen <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
  expect_equal(thirteen[[paste0("swap-prev-", .pub_db)]], 4993L)
  # The other series comes out as it went in, assets and ids alike.
  legacy <- .pub_only(world, "code-2026-07-01")
  expect_equal(.pub_asset_ids(legacy),
               c("cran-code-metrics.db" = 1001L,
                 "swap-prev-cran-code-metrics.db" = 170L))
  expect_equal(.pub_asset_sizes(legacy),
               c("cran-code-metrics.db" = 3000L,
                 "swap-prev-cran-code-metrics.db" = 2500L))
})

# ---------------------------------------------------------------------------
# Drafts no publish comes back for
# ---------------------------------------------------------------------------

.pub_tags <- function(world, drafts) {
  rs <- Filter(function(r) isTRUE(r$isDraft) == drafts, .pub_state(world))
  sort(vapply(rs, function(r) r$tagName, character(1L)))
}

.pub_day <- function(d) {
  sprintf('publish_metrics metrics-%s "CRAN Metrics - %s" || exit 1', d, d)
}

test_that("a draft a failed publish left on an earlier day is deleted once a later day is out", {
  # A publish replaces a draft only under the tag it publishes, and the prune
  # leaves drafts out of its listing. A publish that fails in the last run of a
  # day leaves its draft, databases and all, under a tag nothing publishes
  # again.
  world <- .pub_world(list(.pub_0912(), .pub_stranded_0913()),
                      faults = c(edit = 99L))
  expect_false(.pub_run(world, .pub_day("2026-09-14"))$status == 0L)
  .pub_set_faults(world, integer(0L))
  expect_equal(.pub_run(world, .pub_day("2026-09-15"))$status, 0L)
  expect_equal(.pub_tags(world, drafts = TRUE),
               c("metrics-2026-09-13", "metrics-2026-09-14"))

  res <- .pub_run(world, "delete_stale_drafts metrics metrics-2026-09-15 || exit 1")
  expect_equal(res$status, 0L)
  expect_length(.pub_tags(world, drafts = TRUE), 0L)
  expect_equal(.pub_tags(world, drafts = FALSE),
               c("metrics-2026-09-12", "metrics-2026-09-15"))
  deletes <- grep("-X DELETE", .pub_log(world), value = TRUE, fixed = TRUE)
  expect_equal(sub(".*/", "", deletes), c("3", "2"))
})

test_that("clearing drafts leaves today's draft, other series and every published release alone", {
  # By id, so a draft beside a published release under the same tag goes and
  # the published one stays, which a delete by tag cannot promise.
  world <- .pub_world(list(
    .pub_release(10L, "code-2026-07-01", draft = TRUE),
    .pub_0912(),
    .pub_release(4L, "metrics-2026-09-12", draft = TRUE,
                 assets = .pub_assets[c("code-manifest.json", "data-manifest.json")]),
    .pub_release(5L, "metrics-2026-09-13", assets = .pub_assets, latest = TRUE),
    .pub_release(6L, "metrics-2026-09-14", draft = TRUE)))
  res <- .pub_run(world, "delete_stale_drafts metrics metrics-2026-09-14 || exit 1")
  expect_equal(res$status, 0L)
  expect_equal(vapply(.pub_state(world), function(r) r$id, integer(1L)),
               c(10L, 1L, 5L, 6L))
  expect_false(any(grepl("^gh release delete", .pub_log(world))))
})

test_that("a draft that will not delete waits for the next run, and a listing that fails stops the step", {
  stale <- list(.pub_0912(), .pub_stranded_0913(),
                .pub_release(3L, "metrics-2026-09-14", draft = TRUE),
                .pub_release(4L, "metrics-2026-09-15", assets = .pub_assets))
  step <- c("delete_stale_drafts metrics metrics-2026-09-15 || exit 1",
            'echo "went on past the drafts"')

  world <- .pub_world(stale, faults = c("api-delete" = 1L))
  res <- .pub_run(world, step)
  expect_equal(res$status, 0L)
  expect_true(any(grepl("::warning::could not delete the draft metrics-2026-09-14",
                        res$output, fixed = TRUE)))
  expect_equal(.pub_tags(world, drafts = TRUE), "metrics-2026-09-14")

  # One 500 on the listing is read again: the run's own publish has already
  # gone through by the time this runs, so failing the step over a read that
  # would work on the next attempt turns a clean run red for nothing.
  world <- .pub_world(stale, faults = c(api = 1L))
  res <- .pub_run(world, step)
  expect_equal(res$status, 0L)
  expect_true("went on past the drafts" %in% res$output)
  expect_length(.pub_tags(world, drafts = TRUE), 0L)

  # A listing that cannot be read at all is not "no drafts".
  world <- .pub_world(stale, faults = c(api = 5L))
  res <- .pub_run(world, step)
  expect_false(res$status == 0L)
  expect_false("went on past the drafts" %in% res$output)
  expect_length(.pub_tags(world, drafts = TRUE), 2L)
})

test_that("a listing the sweep could not read once is read again, and one it never reads stops it", {
  # The prune step runs after the run's own publish, so a 500 here fails a run
  # that did everything it was asked. Every other listing in the file is read
  # up to five times; these two were read once.
  leftovers <- list(
    .pub_release(1L, "metrics-2026-09-12", assets = .pub_assets - 1000L,
                 extra = list(.pub_asset(150L, paste0("swap-prev-", .pub_db), 3900L))),
    .pub_out_today())
  step <- c("sweep_swap_leftovers metrics metrics-2026-09-13 || exit 1",
            'echo "went on past the leftovers"')

  world <- .pub_world(leftovers, faults = c(api = 1L))
  res <- .pub_run(world, step)
  expect_equal(res$status, 0L)
  expect_true("went on past the leftovers" %in% res$output)
  expect_equal(.pub_asset_sizes(.pub_only(world, "metrics-2026-09-12")),
               .pub_assets[order(names(.pub_assets))] - 1000L)

  world <- .pub_world(leftovers, faults = c(api = 5L))
  res <- .pub_run(world, step)
  expect_false(res$status == 0L)
  expect_false("went on past the leftovers" %in% res$output)
  expect_true(paste0("swap-prev-", .pub_db) %in%
                names(.pub_asset_sizes(.pub_only(world, "metrics-2026-09-12"))))
})
