# shellcheck shell=bash
# scripts/publish.sh: find the release a run builds on, and publish the one it
# produced. Sourced by update.yml's download and shard steps; test-publish.R
# drives it against a fake gh.
#
# On 2026-09-13 `gh release create metrics-2026-09-13 <four assets> --latest`
# got HTTP 500 on both database uploads. gh implements that command as create a
# draft, upload into it, publish it, and deletes the draft when an upload
# fails, but the delete got a 500 too. That left a draft carrying the two
# manifests and no databases. `gh release list` returns drafts to this
# repository's token, and the draft's tag sorted above every real release, so
# each run after it resolved the draft as the prior release, downloaded two
# manifests, and was refused by preflight. Nothing could publish a newer tag
# past it, and the pipeline stayed stuck until someone deleted the draft.
#
# So a draft is never a baseline here, and the publish is spelled out rather
# than left to one gh call: an empty draft, one asset at a time with retries of
# our own, a check of what landed, and only then the publish. On a new day's
# release a failure at any point leaves at most a draft that nothing resolves.
# A later publish the same day deletes it and starts again; once the day has
# passed, the prune step deletes it (delete_stale_drafts).
#
# Every gh call inside a function ends in `|| return 1` or sits in an `if`. The
# shard loop calls `publish_metrics ... || exit 1`, and bash ignores `set -e`
# for the whole body of a function called on the left of `||`. Before this, a
# failed `gh release upload --clobber` followed by a notes edit that worked
# returned 0, and the run went green with today's release missing an asset.

# GitHub refuses a release asset over 2 GiB, and the --clobber in a same-day
# republish deletes the live asset before the upload is attempted, so an
# oversized database is refused before any gh call. ~1.9 GiB, for headroom.
PUBLISH_MAX_BYTES="${PUBLISH_MAX_BYTES:-2040109465}"

# Size in bytes: GNU stat on the runner, BSD stat where the tests run on macOS.
# A missing file is named before either is asked. GNU stat reads the BSD form
# as a usage error, so on the runner a missing file used to print nothing but
# "stat: invalid option -- '%'". The error goes to stderr, since callers read the
# size from stdout.
file_bytes() {
  if [ ! -f "$1" ]; then
    echo "::error::$1 does not exist." >&2
    return 1
  fi
  stat -c%s "$1" 2>/dev/null || stat -f%z "$1"
}

# Wait before attempt $1 + 1. $2 is the seconds per attempt already made;
# PUBLISH_RETRY_SECONDS overrides it, which is how the tests avoid waiting.
publish_backoff() {
  sleep $(( $1 * ${PUBLISH_RETRY_SECONDS:-$2} ))
}

# Newest PUBLISHED tag in a series ("metrics", or the legacy "code"/"data").
# A draft is not a release: it is what an interrupted create leaves, it can
# carry manifests without their databases, and gh lists it unless told not to.
# Pre-releases stay in. This pipeline never makes one, so a pre-release here
# was marked by hand, and skipping it would move the baseline back.
#
# An empty answer means a cold start, which rebuilds from nothing and publishes
# that as latest, so a listing that failed must never read as empty. The list
# is read into a variable first for that reason: in a pipeline its exit status
# would depend on whether the caller set pipefail.
#
# This is the first call to GitHub in every run, so the listing is read up to
# five times, 10 s, then 20 s and so on apart, as release_state reads it. One
# 500 here stopped the download step before the run did anything. The tag is
# this function's stdout, so the attempt messages go to stderr, and a listing
# that fails all five times still fails the call rather than answering empty.
latest_tag() {
  local tags n
  for n in 1 2 3 4 5; do
    if tags=$(gh release list --exclude-drafts --limit 1000 --json tagName -q '.[].tagName'); then
      printf '%s\n' "$tags" | { grep "^$1-" || true; } | sort -r | head -1
      return 0
    fi
    echo "attempt ${n}: could not list the releases to find the newest $1 release" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to list the releases; cannot tell whether a $1 release exists." >&2
  return 1
}

# What exists under a tag: nothing (empty), "published", "draft", or one line
# per release when more than one carries it. Drafts have to be seen here. A
# listing rather than `gh release view TAG`: view fails the same way whether the
# tag is absent or the call failed, and hands back one release when two share
# the tag.
release_state() {
  local tag="$1" n rows
  for n in 1 2 3 4 5; do
    if rows=$(gh release list --limit 1000 --json tagName,isDraft \
                -q ".[] | select(.tagName == \"${tag}\") | if .isDraft then \"draft\" else \"published\" end"); then
      printf '%s' "$rows"
      return 0
    fi
    echo "attempt ${n}: could not list the releases to find ${tag}" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to list the releases; cannot tell whether ${tag} exists." >&2
  return 1
}

# Every release as "<id> <tag> draft|published", newest first. REST rather than
# `gh release list`, which has no id to give. An id is the only safe way to name
# one of two releases under the same tag: `gh release delete TAG` looks the tag
# up as a published release and as a draft at the same time and acts on
# whichever answer comes back first.
release_rows() {
  gh api "repos/{owner}/{repo}/releases?per_page=100" --paginate \
    -q '.[] | "\(.id) \(.tag_name) \(if .draft then "draft" else "published" end)"'
}

# "  id <id>: draft|published" for each release under a tag, for the operator.
release_ids() {  # $1=tag
  local rows id t kind
  rows=$(release_rows) || return 1
  while read -r id t kind; do
    if [ "$t" = "$1" ]; then echo "  id ${id}: ${kind}"; fi
  done <<< "$rows"
}

# gh retries an upload 3 times, 200 ms apart, which does not outlast an outage:
# on 09-13 the database uploads were failing for at least two minutes. --clobber
# on every attempt, because a failed attempt can leave a partial asset under the
# same name.
upload_asset() {  # $1=tag $2=file
  local n
  for n in 1 2 3 4 5; do
    if gh release upload "$1" "$2" --clobber; then
      return 0
    fi
    echo "attempt ${n}: $(basename "$2") did not upload to $1"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 30; fi
  done
  echo "::error::five attempts failed to upload $(basename "$2") to $1."
  return 1
}

# Every file must be attached to the release, fully uploaded, at the size it has
# here. An upload that exits 0 is gh's word; this is the release's.
#
# A read that disagrees is read again, like one that failed. Nothing promises
# that a release lists an asset the moment its upload returns, and refusing on
# the first read that lags leaves a complete draft unpublished, so the next run
# repeats the day's analysis. Only the last of five reads decides, so an asset
# that really landed short takes about a hundred seconds longer to refuse.
verify_assets() {  # $1=tag, then the files
  local tag="$1" n got f size wrong
  shift
  for n in 1 2 3 4 5; do
    wrong=""
    if got=$(gh release view "$tag" --json assets \
               -q '.assets[] | select(.state == "uploaded") | "\(.name) \(.size)"'); then
      for f in "$@"; do
        size=$(file_bytes "$f") || return 1
        if ! printf '%s\n' "$got" | grep -qxF "$(basename "$f") ${size}"; then
          wrong="$(basename "$f") at ${size} bytes"
          break
        fi
      done
      if [ -z "$wrong" ]; then return 0; fi
      echo "attempt ${n}: ${tag} does not list ${wrong} yet"
    else
      got=""
      echo "attempt ${n}: could not read the assets of ${tag}"
    fi
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  if [ -z "$wrong" ]; then
    echo "::error::five attempts failed to read back the assets of ${tag}."
  else
    echo "::error::${tag} does not carry ${wrong} after the upload; it has:"
    printf '%s\n' "$got" | sed 's/^/  /'
  fi
  return 1
}

# Publish the files as the release under a tag, in the order given.
#
# No release yet: create an empty draft, upload, verify, then publish it as
# Latest. A draft already there was left by an earlier attempt that failed
# somewhere in those steps, so it is deleted and the publish starts again
# rather than trusting whatever it holds. Never with --cleanup-tag: a draft
# has no git tag, so that flag deletes the release and then fails. Create is
# never retried within a call either, because a POST that came back 500 may
# still have made the release, and the next call finds and replaces it.
#
# Already published (an earlier shard today): replace its assets in place and
# refresh the notes, as before, but with the retries and the check. That path
# still deletes each old asset just before its replacement uploads, so a
# failure that outlasts the retries leaves today's release short of that
# asset; the step fails instead of going green.
publish_release() {  # $1=tag $2=title $3=notes file, then the files
  local tag="$1" title="$2" notes="$3" state f
  shift 3
  # Every file is here before the release is touched. publish_metrics measures
  # only the databases, and a missing manifest got as far as an empty draft and
  # five failed uploads before anything said which file it was.
  for f in "$@"; do
    file_bytes "$f" >/dev/null || return 1
  done
  state=$(release_state "$tag") || return 1
  case "$state" in
    ""|published) ;;
    draft)
      echo "::warning::${tag} is a draft an earlier publish did not finish; deleting it and publishing again."
      gh release delete "$tag" --yes || return 1
      state="" ;;
    *)
      # Not by tag: the delete could take the published one and keep the draft.
      echo "::error::more than one release is named ${tag} ($(printf '%s' "$state" | tr '\n' ' ')). Delete the draft ones by id, with gh api -X DELETE repos/{owner}/{repo}/releases/<id>, and re-run. A delete by tag can take the published one."
      release_ids "$tag" ||
        echo "  could not list their ids; gh api 'repos/{owner}/{repo}/releases?per_page=100' --paginate shows them."
      return 1 ;;
  esac

  if [ -z "$state" ]; then
    gh release create "$tag" --draft --title "$title" --notes-file "$notes" || return 1
  fi
  for f in "$@"; do
    upload_asset "$tag" "$f" || return 1
  done
  verify_assets "$tag" "$@" || return 1
  if [ -z "$state" ]; then
    gh release edit "$tag" --draft=false --latest --notes-file "$notes" || return 1
  else
    gh release edit "$tag" --notes-file "$notes" || return 1
  fi
}

# The day's metrics release: both databases, then both manifests, from out/.
#
# Databases first, because a manifest that landed without its database is the
# pair preflight has to refuse, and a database ahead of its manifest is the one
# it can build on.
publish_metrics() {  # $1=tag $2=title
  local tag="$1" title="$2" f bytes
  for f in cran-code-metrics.db cran-data-metrics.db; do
    bytes=$(file_bytes "out/$f") || return 1
    if [ "$bytes" -gt "$PUBLISH_MAX_BYTES" ]; then
      echo "::error::out/$f is ${bytes} bytes (> ${PUBLISH_MAX_BYTES}); refusing to publish ${tag}."
      return 1
    fi
  done
  publish_release "$tag" "$title" out/release-notes-code.md \
    out/cran-code-metrics.db out/cran-data-metrics.db \
    out/code-manifest.json out/data-manifest.json || return 1
}

# Replace one asset of a release that is already out, for the harvest run.
# Only a published release: uploading into a draft publishes nothing, and a
# missing release means the normal update has not run today.
replace_published_asset() {  # $1=tag $2=file
  local tag="$1" file="$2" state
  state=$(release_state "$tag") || return 1
  case "$state" in
    published) ;;
    "")
      echo "::error::no ${tag} release to update; run the normal update first."
      return 1 ;;
    draft)
      echo "::error::${tag} is a draft that was never published; run the normal update first, which replaces it."
      return 1 ;;
    *)
      echo "::error::more than one release is named ${tag}; refusing to guess which one to update."
      return 1 ;;
  esac
  upload_asset "$tag" "$file" || return 1
  verify_assets "$tag" "$file" || return 1
}

# Delete the drafts in a series that no publish will come back for.
#
# publish_release replaces a draft only under the tag it is publishing, and the
# prune lists without drafts. So a publish that fails in the last run of a day,
# or in a dispatch, leaves its draft under a tag nothing uses again, holding up
# to both databases, for good. The prune step runs this once the run's own
# publish has gone through, under the workflow's concurrency group, so no
# publish is part way through a draft. Today's tag is skipped all the same: the
# next publish today replaces that draft itself.
#
# By id, for the reason release_rows gives: a draft can share its tag with a
# published release. A delete that fails is left for the next scheduled run. A
# listing that fails stops the step, as the prune's own listing does, rather
# than reading as no drafts.
delete_stale_drafts() {  # $1=series $2=tag to leave alone
  local rows id t kind
  rows=$(release_rows) || return 1
  while read -r id t kind; do
    case "$t" in "$1"-*) ;; *) continue ;; esac
    if [ "$kind" != draft ] || [ "$t" = "$2" ]; then continue; fi
    echo "deleting the draft ${t} (release ${id}), left by a publish that did not finish"
    if ! gh api -X DELETE "repos/{owner}/{repo}/releases/${id}"; then
      echo "::warning::could not delete the draft ${t} (release ${id}); the next scheduled run tries again."
    fi
  done <<< "$rows"
}
