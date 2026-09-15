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

# A release as the fake keeps it. `assets` is a named integer vector of sizes.
.pub_release <- function(id, tag, draft = FALSE, assets = integer(0L),
                         latest = FALSE, prerelease = FALSE) {
  list(id = id, tagName = tag, isDraft = draft, isPrerelease = prerelease,
       isLatest = latest,
       hasTag = !draft, name = tag, body = "",
       assets = unname(lapply(names(assets), function(n) {
         list(name = n, size = assets[[n]], state = "uploaded")
       })))
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

.pub_expect_whole <- function(world, tag) {
  r <- .pub_only(world, tag)
  expect_false(isTRUE(r$isDraft))
  expect_equal(.pub_asset_sizes(r), .pub_assets[order(names(.pub_assets))])
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

  # The ids could not be read: still refused, and it says where to find them.
  world <- .pub_world(twins, faults = c(api = 1L))
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

test_that("a failed clobber of today's published release fails the step", {
  # bash ignores `set -e` inside a function called as `f || exit 1`, which is
  # how the shard loop calls publish_metrics. A failed upload followed by a
  # notes edit that worked returned 0, and the run went green with today's
  # release missing its database.
  world <- .pub_world(list(
    .pub_0912(),
    .pub_release(2L, "metrics-2026-09-13", assets = .pub_assets - 7L, latest = TRUE)),
    faults = c("upload-cran-code-metrics.db" = 99L))
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
    faults = c("upload-cran-code-metrics.db" = 1L))
  res <- .pub_run(world, harvest)
  expect_equal(res$status, 0L)
  sizes <- .pub_asset_sizes(.pub_only(world, "metrics-2026-09-13"))
  expect_equal(sizes[["cran-code-metrics.db"]], 5000L)
  expect_equal(sizes[["cran-data-metrics.db"]], 2993L)
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

  # The ids could not be read: still refused, and it says where to find them.
  world <- .pub_world(twins, faults = c(api = 1L))
  res <- .pub_run(world, harvest)
  expect_false(res$status == 0L)
  expect_true(any(grepl("could not list their ids", res$output, fixed = TRUE)))
  expect_false(any(grepl("^gh release upload", .pub_log(world))))
  expect_length(.pub_state(world), 3L)
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

  # A listing that cannot be read is not "no drafts".
  world <- .pub_world(stale, faults = c(api = 1L))
  res <- .pub_run(world, step)
  expect_false(res$status == 0L)
  expect_false("went on past the drafts" %in% res$output)
  expect_length(.pub_tags(world, drafts = TRUE), 2L)
})
