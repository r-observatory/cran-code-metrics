# tests/testthat/test-workflow-dated.R
test_that("update.yml publishes dated code and data releases, not rolling current", {
  # test_dir() sources this file with the working directory set to
  # tests/testthat/, so reach the repo root the same way other fixtures do.
  workflow_path <- file.path("..", "..", ".github", "workflows", "update.yml")
  yml <- paste(readLines(workflow_path), collapse = "\n")
  expect_true(grepl("code-\\$\\(date", yml) || grepl('code-', yml, fixed = TRUE))
  expect_true(grepl("data-", yml, fixed = TRUE))
  expect_true(grepl("cran-data-metrics.db", yml, fixed = TRUE))
  # Prior-day immutability: no unconditional clobber of a non-today tag.
  expect_true(grepl("prune.R", yml, fixed = TRUE))
  expect_true(grepl("render_notes.R", yml, fixed = TRUE))
})

test_that("the run and the tests that vet it install the same analyzer", {
  # A pin that drifts between the two is the quiet version of a broken run: the
  # tests pass against one reader while the nightly writes what a different one
  # produced, and nothing in the output says the two disagree.
  pin_of <- function(name) {
    yml <- readLines(file.path("..", "..", ".github", "workflows", name))
    line <- grep("gh release download .*rpkg-analyzer", yml, value = TRUE)
    expect_equal(length(line), 1L)
    sub(".*gh release download +([^ ]+).*", "\\1", line)
  }
  expect_equal(pin_of("test.yml"), pin_of("update.yml"))
})

# ---------------------------------------------------------------------------
# Resolving and publishing releases
# ---------------------------------------------------------------------------
# On 2026-09-13 a failed `gh release create <tag> <assets>` left a draft holding
# two manifests and no databases. `gh release list` hands drafts to this
# repository's token, and the draft's tag sorted above every real release, so
# every run after it built on the draft and was refused. test-publish.R drives
# the helpers; these hold the workflow to using them.

.update_yml <- function() {
  readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
}

.publish_sh <- function() {
  readLines(file.path("..", "..", "scripts", "publish.sh"))
}

# The body of one shell function in publish.sh, from its opening line to the
# first line that closes it.
.sh_function <- function(lines, name) {
  start <- grep(sprintf("^%s\\(\\) \\{", name), lines)
  expect_length(start, 1L)
  end <- start - 1L + grep("^\\}", lines[start:length(lines)])[1L]
  lines[start:end]
}

test_that("every release listing in update.yml leaves drafts out", {
  yml <- .update_yml()
  lists <- grep("gh release list", yml, value = TRUE, fixed = TRUE)
  expect_gte(length(lists), 1L)   # the prune, whose KEEP a draft must not take
  expect_true(all(grepl("--exclude-drafts", lists, fixed = TRUE)))

  latest <- .sh_function(.publish_sh(), "latest_tag")
  expect_true(any(grepl("gh release list --exclude-drafts", latest, fixed = TRUE)))
})

test_that("the steps that resolve or publish a release source the shared helpers", {
  yml <- .update_yml()
  expect_false(any(grepl("latest_tag() {", yml, fixed = TRUE)))
  expect_false(any(grepl("publish_metrics() {", yml, fixed = TRUE)))

  starts <- grep("^      - ", yml)
  steps <- split(yml, findInterval(seq_along(yml), starts))
  users <- Filter(function(s) {
    any(grepl("latest_tag|publish_metrics|replace_published_asset|delete_stale_drafts", s))
  }, steps)
  expect_length(users, 3L)   # the download, shard and prune steps
  for (s in users) {
    expect_true(any(grepl("source scripts/publish.sh", s, fixed = TRUE)))
  }
  # The shard loop still stops the run when a publish fails.
  expect_true(any(grepl("publish_metrics .*\\|\\| exit 1", yml)))
})

test_that("no release is created with its assets attached", {
  # gh makes that release a draft while the assets upload, and a failure there
  # is what stranded the 09-13 draft.
  expect_false(any(grepl("gh release create", .update_yml(), fixed = TRUE)))
  creates <- grep("gh release create", .publish_sh(), value = TRUE, fixed = TRUE)
  creates <- creates[!grepl("^\\s*#", creates)]
  expect_gte(length(creates), 1L)
  expect_true(all(grepl("--draft", creates, fixed = TRUE)))
  expect_false(any(grepl("\\.db|manifest\\.json|\\$@|\\$\\{assets", creates)))
})

test_that("the harvest upload requires a published release", {
  yml <- .update_yml()
  start <- grep("inputs.harvest_descriptions }}\" = \"true\"", yml, fixed = TRUE)
  expect_length(start, 1L)
  end <- start - 1L + grep("exit 0", yml[start:length(yml)], fixed = TRUE)[1L]
  harvest <- yml[start:end]
  expect_true(any(grepl("replace_published_asset .*\\|\\| exit 1", harvest)))
  expect_false(any(grepl("gh release (upload|view)", harvest)))

  body <- .sh_function(.publish_sh(), "replace_published_asset")
  expect_true(any(grepl("published", body, fixed = TRUE)))
})

test_that("an asset of a release that is already out is replaced by name, never clobbered", {
  # `gh release upload --clobber` deletes the live asset and only then uploads
  # its replacement, so a failure that outlasts the retries left today's
  # release without its database, and every run after it was refused.
  sh <- .publish_sh()
  code <- function(name) {
    body <- .sh_function(sh, name)
    body[!grepl("^\\s*#", body)]
  }
  harvest <- code("replace_published_asset")
  expect_true(any(grepl("swap_asset", harvest, fixed = TRUE)))
  expect_false(any(grepl("upload_asset", harvest, fixed = TRUE)))

  # The new day's draft is the one path that still uploads under the real
  # names: nothing resolves a draft, so there is no reader to protect.
  publish <- code("publish_release")
  expect_true(any(grepl("swap_asset", publish, fixed = TRUE)))
  expect_true(any(grepl("upload_asset", publish, fixed = TRUE)))

  # swap_asset is the link the upload goes through and its removal afterwards;
  # swap_staged_asset is the replacement itself. Neither deletes the copy it
  # replaces, so they are read together.
  swap <- c(code("swap_asset"), code("swap_staged_asset"))
  expect_true(any(grepl('repair_asset "$tag" "$rel" "$name"', swap, fixed = TRUE)))
  expect_true(any(grepl('upload_asset "$tag" "$link"', swap, fixed = TRUE)))
  # The copy it replaces is renamed out of the way, never deleted: a delete
  # cuts off a download of that asset that is already running.
  expect_false(any(grepl("delete_asset", swap, fixed = TRUE)))
  expect_true(any(grepl('rename_asset_retrying "$old_id" "swap-prev-${name}"', swap, fixed = TRUE)))
})

test_that("the download step puts right what an interrupted replacement left on the release it reads", {
  # Nothing publishes under an earlier day's tag again, so a replacement
  # stopped between its two renames leaves that release without the asset,
  # with the bytes under swap-prev-NAME and no publish coming back for them.
  # The run that reads the release is what comes back.
  yml <- .update_yml()
  start <- grep("- name: Download the latest databases", yml, fixed = TRUE)
  expect_length(start, 1L)
  end <- start - 1L +
    grep("- name: Analyze shards", yml[start:length(yml)], fixed = TRUE)[1L]
  download <- yml[start:end]

  repairs <- grep("repair_release_assets", download)
  expect_length(repairs, 2L)
  # Each call, with the line it is continued onto, stops the step when it fails.
  statements <- strsplit(gsub("\\\\\n\\s*", " ", paste(download, collapse = "\n")),
                         "\n")[[1L]]
  statements <- grep("repair_release_assets", statements, value = TRUE)
  expect_length(statements, 2L)
  expect_true(all(grepl("|| exit 1", statements, fixed = TRUE)))
  # Before the run reads what the release carries, not after.
  expect_lt(max(repairs), min(grep('list_assets "\\$(CODE|DATA)_SRC"', download)))
})

test_that("the prune clears the copies a replacement left on releases nothing publishes again", {
  # Every replacement leaves the copy it replaced behind, and the last one of
  # a day is never revisited, so each kept release would otherwise carry a
  # second copy of both databases for good.
  yml <- .update_yml()
  start <- grep("- name: Prune old dated releases", yml, fixed = TRUE)
  expect_length(start, 1L)
  prune <- yml[start:length(yml)]
  expect_true(any(grepl('sweep_swap_leftovers metrics "$METRICS_TAG" || exit 1',
                        prune, fixed = TRUE)))
})

test_that("the cleanup leaves alone the tag the run published, not a day it works out again", {
  # The shard step runs for hours, and the prune comes after it. A run that
  # started late enough works out tomorrow's date there and sweeps the release
  # it published minutes earlier, cutting off the downloads the copies it
  # leaves behind exist to carry, and no longer spares a draft under the tag it
  # was publishing. So the tag is settled once, where the release is made.
  yml <- .update_yml()
  shard <- grep("- name: Analyze shards", yml, fixed = TRUE)
  prune <- grep("- name: Prune old dated releases", yml, fixed = TRUE)
  expect_length(shard, 1L)
  expect_length(prune, 1L)

  publishing <- yml[shard:prune]
  expect_true(any(grepl('echo "METRICS_TAG=${METRICS_TAG}" >> "$GITHUB_ENV"',
                        publishing, fixed = TRUE)))
  # One date for the run, worked out before the first publish.
  expect_length(grep("date -u", publishing, fixed = TRUE), 1L)

  cleanup <- yml[prune:length(yml)]
  expect_false(any(grepl("date -u", cleanup, fixed = TRUE)))
  expect_true(any(grepl('delete_stale_drafts metrics "$METRICS_TAG" || exit 1',
                        cleanup, fixed = TRUE)))
  expect_true(any(grepl('sweep_swap_leftovers metrics "$METRICS_TAG" || exit 1',
                        cleanup, fixed = TRUE)))
})

test_that("the prune clears the drafts a failed publish left on an earlier day", {
  # Publishing replaces a draft only under today's tag, and the prune's listing
  # leaves drafts out, so without this a draft from a failed last run of a day
  # stays for good.
  yml <- .update_yml()
  start <- grep("- name: Prune old dated releases", yml, fixed = TRUE)
  expect_length(start, 1L)
  prune <- yml[start:length(yml)]
  expect_true(any(grepl("source scripts/publish.sh", prune, fixed = TRUE)))
  expect_true(any(grepl('delete_stale_drafts metrics "$METRICS_TAG" || exit 1',
                        prune, fixed = TRUE)))

  body <- .sh_function(.publish_sh(), "delete_stale_drafts")
  expect_true(any(grepl("gh api -X DELETE", body, fixed = TRUE)))
  expect_false(any(grepl("gh release delete", body[!grepl("^\\s*#", body)], fixed = TRUE)))
})
