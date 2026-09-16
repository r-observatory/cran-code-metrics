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
# failed upload followed by a notes edit that worked returned 0, and the run
# went green with today's release missing an asset.
#
# An asset of a release that is already out is never replaced with `gh release
# upload --clobber`, which deletes the live asset and only then uploads its
# replacement: when every attempt failed, today's release was left without its
# database, and since that release is still `latest_tag metrics` tomorrow, the
# download step came back with a manifest and no database and preflight refused
# every run after it. swap_asset uploads under a temporary name and swaps the
# names once the bytes are on the release, so the gap in which a reader finds
# no asset of that name is one rename wide instead of one upload wide, and the
# copy it replaces is kept rather than deleted.

# GitHub refuses a release asset over 2 GiB, so an oversized database is
# refused before any gh call rather than after a 1.9 GiB upload that cannot
# land. ~1.9 GiB, for headroom.
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

# sha256 of a file, bare hex, to hold an uploaded asset's digest against. GNU
# coreutils on the runner, the BSD spelling where the tests run on macOS.
file_sha256() {
  local out
  if out=$(sha256sum "$1" 2>/dev/null) || out=$(shasum -a 256 "$1" 2>/dev/null); then
    printf '%s' "${out%% *}"
    return 0
  fi
  echo "::error::could not compute the sha256 of $1." >&2
  return 1
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

# Refuse a tag that more than one release carries, and say how to clear it,
# naming each release under it by id. Not by tag: the delete could take the
# published one and keep the draft. $2 says what is refused. Always returns 1.
refuse_doubled_tag() {  # $1=tag $2=what is refused
  echo "::error::more than one release is named $1; $2 Delete the draft ones by id, with gh api -X DELETE repos/{owner}/{repo}/releases/<id>, and re-run. A delete by tag can take the published one."
  release_ids "$1" ||
    echo "  could not list their ids; gh api 'repos/{owner}/{repo}/releases?per_page=100' --paginate shows them."
  return 1
}

# The id of the PUBLISHED release under a tag. The tag endpoint answers with
# the published release only, which is what the callers here want: an asset is
# never replaced on a draft. Read up to five times, as every listing is.
release_id() {  # $1=tag
  local n id
  for n in 1 2 3 4 5; do
    if id=$(gh api "repos/{owner}/{repo}/releases/tags/$1" -q .id); then
      printf '%s' "$id"
      return 0
    fi
    echo "attempt ${n}: could not read the release ${1}" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to read the release ${1}." >&2
  return 1
}

# "<id> <state> <size> <digest> <name>" for every asset of a release, the
# half-uploaded ones included.
#
# GET /releases/{id}/assets is the only listing that shows an asset in state
# "starter", which is what an upload that was cut off leaves: it sits there at
# the full declared size with no digest and does not clear itself, while `gh
# release view`, `gh release download` and `gh release upload --clobber` all
# read the tag listing, which leaves it out. The name goes last because nothing
# before it can hold a space.
release_assets() {  # $1=release id
  local n rows
  for n in 1 2 3 4 5; do
    if rows=$(gh api "repos/{owner}/{repo}/releases/$1/assets" --paginate \
                -q '.[] | "\(.id) \(.state) \(.size) \(.digest) \(.name)"'); then
      printf '%s' "$rows"
      return 0
    fi
    echo "attempt ${n}: could not list the assets of release ${1}" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to list the assets of release ${1}." >&2
  return 1
}

# One asset of such a listing, by name, as "<id> <state> <size> <digest>", or
# nothing when the release does not carry that name. The four fields are read
# back out with asset_id and the three beside it.
asset_row() {  # $1=listing $2=name
  local id state size digest name
  printf '%s\n' "$1" | while read -r id state size digest name; do
    if [ "$name" = "$2" ]; then echo "${id} ${state} ${size} ${digest}"; fi
  done
}

asset_id()     { printf '%s' "${1%% *}"; }
asset_state()  { local r="${1#* }"; printf '%s' "${r%% *}"; }
asset_size()   { local r="${1#* }"; r="${r#* }"; printf '%s' "${r%% *}"; }
asset_digest() { case "$1" in *" "*) printf '%s' "${1##* }" ;; esac; }

# Rename an asset by id: the id, the bytes, the size, the digest and the state
# all stay, only the name and a download by that name move. A name another
# asset of the release already holds is refused with 422 already_exists,
# compared case-insensitively, and an id that is gone with 404; gh prints the
# body on stdout, its own line on stderr, and exits 1 for both.
rename_asset() {  # $1=asset id $2=new name
  gh api -X PATCH "repos/{owner}/{repo}/releases/assets/$1" -f "name=$2" >/dev/null
}

# Both halves of a swap are retried, seconds apart: the gap between them is the
# only moment in which a reader finds no asset of that name.
rename_asset_retrying() {  # $1=asset id $2=new name
  local n
  for n in 1 2 3 4 5; do
    if rename_asset "$1" "$2"; then return 0; fi
    echo "attempt ${n}: could not name asset ${1} ${2}"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 2; fi
  done
  return 1
}

# Deleting an asset cuts off a download of it that is already running, so this
# is only ever aimed at a temporary name, or at a live name holding bytes the
# release does not serve, which no download can be on. The copy a replacement
# moves aside stays until the next publish under that tag, or the prune's
# sweep.
delete_asset() {  # $1=asset id
  gh api -X DELETE "repos/{owner}/{repo}/releases/assets/$1" >/dev/null
}

# Take an asset a caller has proved wrong off the release. Only ever aimed at a
# temporary name, and best effort: the caller is already failing.
#
# The bytes of a refused upload cannot be left there. Nothing downloads
# NAME.next, so keeping it serves no reader, and repair_asset gives that name
# to a whole NAME.next when the release has lost NAME itself: the one copy this
# file has measured against the local file and refused would otherwise be the
# one a later run promotes, without measuring it again.
discard_asset() {  # $1=tag $2=release id $3=asset name
  local rows row id
  if ! rows=$(release_assets "$2"); then
    echo "::warning::could not read the assets of $1 to clear $3; delete it by id if it is there."
    return 0
  fi
  row=$(asset_row "$rows" "$3")
  if [ -z "$row" ]; then return 0; fi
  id=$(asset_id "$row")
  echo "clearing $3 on $1, which holds bytes this run refused"
  delete_asset "$id" ||
    echo "::warning::could not clear $3 (asset ${id}) on $1; delete it by id, or a run that finds the name missing gives it to bytes this one refused."
}

# Put right whatever an interrupted replacement of one asset left on a release,
# before anything else touches it. By id, always. Running it twice is a no-op
# the second time.
#
# Every state below was built on a real release and read back the way the
# merger reads it:
#
#   NAME not whole              its bytes are not servable: a download of an
#                               upload that was cut off answers BlobNotFound.
#                               It is deleted, and the rules below then decide
#                               what takes the name. Whole means the release
#                               says "uploaded", and nothing else does: the
#                               state measured on a cut-off upload is
#                               "starter", which the documented uploaded|open
#                               does not carry, so the set is not closed, and
#                               this is the test that decides whether the copy
#                               under .prev is deleted.
#   NAME there, .next as well   an upload that was cut off, or one that landed
#                               and was never swapped in. The live asset is
#                               correct; the leftover goes, because a complete
#                               one would refuse the next upload of that name.
#   NAME there, .prev as well   the swap finished and the copy it replaced was
#                               left for readers. It goes now, a publish later,
#                               rather than straight after the swap.
#   NAME gone, .prev there      the run stopped between the two renames. The
#                               name goes back on the published bytes: the
#                               manifests were not swapped either, so that pair
#                               is exactly where the day started.
#   NAME gone, only a whole .next   nothing published is left and the new bytes
#                               are complete, so they take the name.
#   NAME gone, only a cut-off .next  there is nothing to serve. It is deleted
#                               and said out loud; a caller that is about to
#                               upload the asset carries on.
#   none of the three there     the release has lost the asset and nothing is
#                               left to put the name back on, which is said
#                               out loud for the same reason: the caller may
#                               be about to supply it, and the run that reads
#                               the release has its own answer for an asset
#                               that is not there.
repair_asset() {  # $1=tag $2=release id $3=asset name
  local tag="$1" rel="$2" name="$3" rows here prev next
  rows=$(release_assets "$rel") || return 1
  here=$(asset_row "$rows" "$name")
  prev=$(asset_row "$rows" "${name}.prev")
  next=$(asset_row "$rows" "${name}.next")

  if [ -n "$here" ] && [ "$(asset_state "$here")" != uploaded ]; then
    echo "::warning::${name} on ${tag} is an upload that did not finish (state $(asset_state "$here")); clearing it."
    delete_asset "$(asset_id "$here")" || return 1
    here=""
  fi

  if [ -n "$here" ]; then
    if [ -n "$prev" ]; then
      echo "clearing ${name}.prev on ${tag}, the copy the last replacement moved aside"
      delete_asset "$(asset_id "$prev")" || return 1
    fi
    if [ -n "$next" ]; then
      echo "clearing ${name}.next on ${tag}, left by a replacement that did not finish"
      delete_asset "$(asset_id "$next")" || return 1
    fi
    return 0
  fi

  if [ -n "$prev" ]; then
    echo "::warning::${tag} carries no ${name}; putting the name back on ${name}.prev, which a replacement moved aside and never finished."
    rename_asset "$(asset_id "$prev")" "$name" || return 1
    if [ -n "$next" ]; then
      delete_asset "$(asset_id "$next")" || return 1
    fi
    return 0
  fi

  if [ -n "$next" ]; then
    if [ "$(asset_state "$next")" = uploaded ]; then
      echo "::warning::${tag} carries no ${name}; giving the name to ${name}.next, which uploaded whole."
      rename_asset "$(asset_id "$next")" "$name" || return 1
    else
      echo "::warning::${tag} carries no ${name} and ${name}.next was cut off, so there is nothing to put the name back on; clearing it."
      delete_asset "$(asset_id "$next")" || return 1
    fi
  else
    echo "::warning::${tag} carries no ${name} and nothing a replacement left aside, so there is nothing to put the name back on."
  fi
  return 0
}

# Put right what an interrupted replacement left on a release, for each asset
# name given.
#
# The download step calls this on the release it is building on, before it
# reads it. Nothing publishes under an earlier day's tag again, so a run that
# died between the two renames of its last replacement would otherwise leave
# that release without the asset for good, with the bytes sitting under .prev
# and nothing left to put the name back.
repair_release_assets() {  # $1=tag, then the asset names
  local tag="$1" rel n
  shift
  rel=$(release_id "$tag") || return 1
  for n in "$@"; do
    repair_asset "$tag" "$rel" "$n" || return 1
  done
}

# Replace one asset of a published release with a local file, without the
# release ever carrying a half-written one under that name:
#
#   0. put right whatever an earlier attempt left,
#   1. upload the file under NAME.next,
#   2. make the release say that asset is whole,
#   3. rename NAME to NAME.prev, then NAME.next to NAME,
#   4. read the release back,
#   5. leave NAME.prev for the next publish under this tag, or for the sweep.
#
# Measured against a scratch repository: a by-name download finds no asset for
# 472 to 1013 ms (median 520) between the two renames, against the length of
# the whole upload with --clobber, and what a reader meets in that gap is "no
# assets match the file pattern" from a listing taken inside it, never a 404 on
# an id that has been deleted, because the old bytes are still there under
# .prev. Sending both renames down one connection with curl measured 235 to
# 355 ms, which buys a quarter of a second for a second HTTP client and a
# second way of holding the token in the publish path.
swap_asset() {  # $1=tag $2=release id $3=file
  local tag="$1" rel="$2" file="$3"
  local name size sha stage link n rows row old_id new_id got ok

  name=$(basename "$file")
  size=$(file_bytes "$file") || return 1
  sha=$(file_sha256 "$file") || return 1

  repair_asset "$tag" "$rel" "$name" || return 1

  # 1. A symlink gives the file the temporary name without copying 1.9 GB of
  #    it: the upload names the asset after the path it is handed and sends the
  #    bytes the link points at.
  stage="$(dirname "$file")/.publish-stage"
  mkdir -p "$stage" || return 1
  link="${stage}/${name}.next"
  ln -sfn "$(cd "$(dirname "$file")" && pwd)/${name}" "$link" || return 1
  upload_asset "$tag" "$link" || return 1

  # 2. An upload that exits 0 is usually servable in the same instant, but once
  #    it was not: 24.5 s later a download of it still answered BlobNotFound.
  #    So the release is asked what it holds, and a read that disagrees is read
  #    again rather than refused, as verify_assets reads its own.
  ok=""
  for n in 1 2 3 4 5; do
    rows=$(release_assets "$rel") || return 1
    row=$(asset_row "$rows" "${name}.next")
    if [ -n "$row" ] && [ "$(asset_state "$row")" = uploaded ] &&
       [ "$(asset_size "$row")" = "$size" ]; then
      got=$(asset_digest "$row")
      # An asset carries "sha256:<64 hex>" once it is written. It is checked
      # when the release gives one: a half-written asset has none, and neither
      # does one uploaded before the API reported them.
      if [ "$got" = "sha256:${sha}" ] || [ "$got" = null ] || [ -z "$got" ]; then
        ok=yes
        break
      fi
    fi
    echo "attempt ${n}: ${tag} does not list ${name}.next as ${size} bytes of sha256:${sha} yet; it has [${row:-nothing}]"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  if [ -z "$ok" ]; then
    echo "::error::${tag} does not carry ${name}.next at ${size} bytes of sha256:${sha} after the upload; it has [${row:-nothing}]. ${name} itself is untouched."
    discard_asset "$tag" "$rel" "${name}.next"
    return 1
  fi

  # 3. Swap the names, the published copy out of the way first, so the name is
  #    never on two assets and the bytes behind it are never deleted.
  rows=$(release_assets "$rel") || return 1
  old_id=$(asset_id "$(asset_row "$rows" "$name")")
  new_id=$(asset_id "$(asset_row "$rows" "${name}.next")")
  if [ -z "$new_id" ]; then
    echo "::error::${name}.next is gone from ${tag} between the check and the swap."
    return 1
  fi
  if [ -n "$old_id" ] && ! rename_asset_retrying "$old_id" "${name}.prev"; then
    echo "::error::five attempts failed to move ${name} aside on ${tag}; it is still the published asset and nothing was taken away."
    return 1
  fi
  if ! rename_asset_retrying "$new_id" "$name"; then
    echo "::error::${name}.next uploaded whole but five attempts failed to give it the name ${name} on ${tag}."
    if [ -n "$old_id" ]; then
      if rename_asset "$old_id" "$name"; then
        echo "::error::${name} is back on the copy that was published, so ${tag} carries what it did before this run."
      else
        echo "::error::${tag} now carries no ${name}: its bytes are under ${name}.prev, nothing was deleted, and the next publish or download under this tag puts the name back."
      fi
    fi
    return 1
  fi

  # 4. Read the release back. A listing taken inside 40 ms of a rename that
  #    answered 200 has been seen to still show the state before it, so a read
  #    that disagrees is read again too.
  ok=""
  for n in 1 2 3 4 5; do
    rows=$(release_assets "$rel") || return 1
    row=$(asset_row "$rows" "$name")
    if [ "$(asset_id "$row")" = "$new_id" ] && [ "$(asset_size "$row")" = "$size" ]; then
      ok=yes
      break
    fi
    echo "attempt ${n}: ${tag} lists ${name} as [${row:-nothing}], not asset ${new_id} at ${size} bytes"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  if [ -z "$ok" ]; then
    echo "::error::${tag} lists ${name} as [${row:-nothing}] after the swap, not asset ${new_id} at ${size} bytes."
    return 1
  fi

  # 5. NAME.prev stays. A delete cuts off a download of that asset already in
  #    flight, and the merger's is tens of seconds long, so a reader whose
  #    listing was taken before the swap finishes on the old bytes instead of
  #    failing and pulling the whole file again.
  #
  #    What the reader makes of a whole file it did not expect is its own
  #    affair, and the merger makes the worst of it: it reads the declared
  #    size of whatever holds the name once its download is done, so a
  #    download that finished on the copy now under .prev is a size it was not
  #    promised, which it calls a torn download and fails the merge over.
  #    That costs one merge run per day it happens on, and the next hourly one
  #    reads the new bytes; the cut-off download it replaces had to be made
  #    again anyway, and could be cut off again.
  if [ -n "$old_id" ]; then
    echo "${tag}: ${name} is asset ${new_id}, ${size} bytes, sha256:${sha}; the copy it replaced is ${name}.prev"
  else
    echo "${tag}: ${name} is asset ${new_id}, ${size} bytes, sha256:${sha}; the release carried no ${name} before this run, so nothing was moved aside"
  fi
  return 0
}

# gh retries an upload 3 times, 200 ms apart, which does not outlast an outage:
# on 09-13 the database uploads were failing for at least two minutes.
#
# --clobber on every attempt, because a failed attempt can leave a partial
# asset under the same name. It only reaches an asset gh can see: an upload
# that was cut off leaves one in state "starter", which the tag listing does
# not show, and the next upload displaces it anyway.
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

# Edit a release with the gh release edit flags that follow, up to five times,
# 10 s, then 20 s and so on apart.
#
# Both edits a publish makes come after every asset has landed and been
# checked, so one 500 on either would throw that upload away, leave the day's
# release unpublished or its notes stale, and fail the run, which repeats the
# day's analysis. Each is one PATCH that sets the same fields however many
# times it lands, so repeating one that returned 500 and applied anyway changes
# nothing: gh finds the release by its tag again and sets the fields again.
edit_release() {  # $1=tag, then the flags
  local tag="$1" n
  shift
  for n in 1 2 3 4 5; do
    if gh release edit "$tag" "$@"; then
      return 0
    fi
    echo "attempt ${n}: could not edit ${tag}"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to edit ${tag}."
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
# still have made the release, and the next call finds and replaces it. Nor is
# the delete of the draft, which goes by tag: gh resolves a tag to a published
# release as readily as to a draft (release_rows), so when the listing has not
# caught up with a release published under the same tag, the delete can take
# that release instead, and each repeat is another chance to. A draft left by
# the one attempt is deleted by the next publish under the tag, or by
# delete_stale_drafts once the day has passed. The listing, the uploads, the
# read-back and the edits land the same however often they run, and each is
# retried.
#
# Already published (an earlier shard today): each asset is replaced by name
# with swap_asset, in the order the files are given, and the notes refreshed.
# A failure that outlasts the retries leaves the release carrying what it did
# before the run, and fails the step instead of going green.
publish_release() {  # $1=tag $2=title $3=notes file, then the files
  local tag="$1" title="$2" notes="$3" state rel f
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
      refuse_doubled_tag "$tag" "refusing to publish into it."
      return 1 ;;
  esac

  if [ -z "$state" ]; then
    # Nothing resolves a draft, so its assets go up under their own names.
    gh release create "$tag" --draft --title "$title" --notes-file "$notes" || return 1
    for f in "$@"; do
      upload_asset "$tag" "$f" || return 1
    done
  else
    # A release that is out is read by the merger and by the next run's
    # download step while this runs, so each asset is replaced by name.
    rel=$(release_id "$tag") || return 1
    for f in "$@"; do
      swap_asset "$tag" "$rel" "$f" || return 1
    done
  fi
  verify_assets "$tag" "$@" || return 1
  if [ -z "$state" ]; then
    edit_release "$tag" --draft=false --latest --notes-file "$notes" || return 1
  else
    edit_release "$tag" --notes-file "$notes" || return 1
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
#
# The file is checked before the release is read, as publish_release checks
# its own. gh refuses a missing file without calling GitHub, so the upload
# retries could not change the answer: it took five attempts and about 300 s
# of backoff to fail, and the error named the upload rather than the file.
replace_published_asset() {  # $1=tag $2=file
  local tag="$1" file="$2" state rel
  file_bytes "$file" >/dev/null || return 1
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
      refuse_doubled_tag "$tag" "refusing to guess which one to update."
      return 1 ;;
  esac
  rel=$(release_id "$tag") || return 1
  swap_asset "$tag" "$rel" "$file" || return 1
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

# Clear the copies a replacement left on the releases of a series that no
# publish comes back for, and put back any name a swap did not finish giving.
#
# Every replacement leaves the copy it replaced under NAME.prev, and the next
# publish under the same tag clears it. The last publish of a day is never
# revisited, because tomorrow's publish uses tomorrow's tag, so without this
# every kept release would carry a second copy of both databases for good.
#
# Today's tag is left alone for the reason the .prev is left in the first
# place: a delete cuts off a download of that asset already in flight, and the
# merger's is tens of seconds long. Tomorrow's sweep takes it. This runs in the
# prune step, after the run's own publish and under the workflow's concurrency
# group, so no replacement is part way through.
#
# The release listing carries each release's assets, which is one call rather
# than one per release; a half-uploaded asset may not appear in it, and one on
# a release nothing publishes to again would then be swept by neither this nor
# a later publish.
sweep_swap_leftovers() {  # $1=series $2=tag to leave alone
  local rows pairs id t base
  # shellcheck disable=SC2016  # $r is jq's own variable, not the shell's
  rows=$(gh api "repos/{owner}/{repo}/releases?per_page=100" --paginate \
           -q '.[] | . as $r | .assets[]?
               | select(.name | endswith(".prev") or endswith(".next"))
               | "\($r.id) \($r.tag_name) \(.name)"') || return 1
  # Both temporary names of one asset are one asset to repair.
  pairs=$(printf '%s\n' "$rows" | sed -e 's/\.prev$//' -e 's/\.next$//' | sort -u)
  while read -r id t base; do
    case "$t" in "$1"-*) ;; *) continue ;; esac
    if [ "$t" = "$2" ] || [ -z "$base" ]; then continue; fi
    echo "clearing what a replacement left under ${base} on ${t} (release ${id})"
    if ! repair_asset "$t" "$id" "$base"; then
      echo "::warning::could not clear what a replacement left under ${base} on ${t}; the next scheduled run tries again."
    fi
  done <<< "$pairs"
}
