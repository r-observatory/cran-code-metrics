#!/usr/bin/env bash
# tests/testthat/fixtures/fake-gh.sh: a stand-in for the `gh release` calls
# scripts/publish.sh makes. test-publish.R copies it onto PATH as `gh`.
#
# It models the parts of gh 2.100.0 that decide whether a publish can strand a
# draft, not the whole CLI:
#   - `release list` returns drafts unless told --exclude-drafts, and
#     pre-releases unless told --exclude-pre-releases.
#   - `release view/upload/edit/delete TAG` look the tag up as a published
#     release and as a draft at the same time and act on whichever answers
#     first, the way shared.FetchRelease does: gh 2.100.0
#     pkg/cmd/release/shared/fetch.go starts both lookups in goroutines and
#     returns the first result that carries no error. resolve() below settles
#     that race published-first every time, so nothing a test asserts may rest
#     on the order.
#   - `release create TAG files...` without --draft makes a draft, uploads the
#     files (the small manifests land first) and publishes it, deleting the
#     draft again if an upload fails, and that delete can fail too.
#   - `upload --clobber` deletes the old copy before uploading the new one, so
#     a failed upload leaves the release without that asset.
#   - a draft has no git tag, so `delete --cleanup-tag` on one deletes the
#     release and then exits non-zero.
#
# State is $GH_STATE, a JSON array of releases, oldest first:
#   {id, tagName, isDraft, isPrerelease, isLatest, hasTag, name, body,
#    assets: [{name, size, state}]}
# Every call is appended to $GH_LOG. A fault is a file in $GH_FAULTS holding how
# many more times it fires:
#   upload-<asset>  that asset's upload returns HTTP 500
#   short-<asset>   that asset's upload succeeds but lands one byte short
#   create          create returns HTTP 500 and creates nothing
#   create-after    create makes the release, then returns HTTP 500
#   cleanup         create's own delete of its draft returns HTTP 500
#   list, view, edit, delete   that call returns HTTP 500
set -u

echo "gh $*" >> "$GH_LOG"

state() { cat "$GH_STATE"; }
save() { local t; t=$(mktemp "${GH_STATE}.XXXXXX") && cat > "$t" && mv "$t" "$GH_STATE"; }
fsize() { stat -c%s "$1" 2>/dev/null || stat -f%z "$1"; }

fault() {
  local f="$GH_FAULTS/$1" n
  [ -f "$f" ] || return 1
  n=$(cat "$f")
  [ "$n" -gt 0 ] || return 1
  echo $((n - 1)) > "$f"
  return 0
}

http500() { echo "HTTP 500: Internal Server Error ($1)" >&2; exit 1; }

# shared.FetchRelease: whichever of the published and the draft lookup answers
# first. gh runs them at once; here the published release under TAG wins every
# time, which is one of the two orders gh can come back in, so nothing a test
# asserts may rest on this one.
resolve() {
  state | jq -e --arg t "$1" '
    ([.[] | select(.tagName == $t and (.isDraft | not))] +
     [.[] | select(.tagName == $t and .isDraft)]) | .[0].id' 2>/dev/null ||
    { echo "release not found" >&2; return 1; }
}

add_asset() {  # id file
  local name size
  name=$(basename "$2")
  size=$(fsize "$2")
  if fault "short-$name"; then size=$((size - 1)); fi
  state | jq --argjson id "$1" --arg n "$name" --argjson s "$size" '
    map(if .id == $id
        then .assets = ([.assets[] | select(.name != $n)] +
                        [{name: $n, size: $s, state: "uploaded"}])
        else . end)' | save
}

drop_asset() {  # id name
  state | jq --argjson id "$1" --arg n "$2" '
    map(if .id == $id then .assets |= map(select(.name != $n)) else . end)' | save
}

has_asset() {  # id name
  state | jq -e --argjson id "$1" --arg n "$2" '
    any(.[] | select(.id == $id) | .assets[]; .name == $n)' >/dev/null
}

drop_release() { state | jq --argjson id "$1" 'map(select(.id != $id))' | save; }

make_latest() {
  state | jq --argjson id "$1" 'map(.isLatest = (.id == $id))' | save
}

[ "${1:-}" = release ] || { echo "fake gh: unsupported command: $*" >&2; exit 2; }
sub="${2:-}"
shift 2

case "$sub" in
  list)
    exclude=false; exclude_pre=false; query=""
    while [ $# -gt 0 ]; do
      case "$1" in
        --exclude-drafts) exclude=true ;;
        --exclude-pre-releases) exclude_pre=true ;;
        -q|--jq) query="$2"; shift ;;
        -L|--limit|--json) shift ;;
      esac
      shift
    done
    if fault list; then http500 "list"; fi
    listed=$(state | jq --argjson e "$exclude" --argjson p "$exclude_pre" '
      [reverse[] | select(($e and .isDraft) | not)
                 | select(($p and (.isPrerelease // false)) | not)]')
    if [ -n "$query" ]; then printf '%s\n' "$listed" | jq -r "$query"; else printf '%s\n' "$listed"; fi
    ;;

  view)
    tag="$1"; shift; query=""
    while [ $# -gt 0 ]; do
      case "$1" in
        -q|--jq) query="$2"; shift ;;
        --json) shift ;;
      esac
      shift
    done
    if fault view; then http500 "view"; fi
    id=$(resolve "$tag") || exit 1
    one=$(state | jq --argjson id "$id" '.[] | select(.id == $id)')
    if [ -n "$query" ]; then printf '%s\n' "$one" | jq -r "$query"; else printf '%s\n' "$one"; fi
    ;;

  create)
    tag="$1"; shift; draft=false; latest=false; title=""; notes=""; files=()
    while [ $# -gt 0 ]; do
      case "$1" in
        --draft) draft=true ;;
        --latest) latest=true ;;
        --title|-t) title="$2"; shift ;;
        --notes-file|-F) notes=$(cat "$2"); shift ;;
        --*) ;;
        *) files+=("$1") ;;
      esac
      shift
    done
    if fault create; then http500 "create"; fi
    if [ "$draft" = false ] &&
       state | jq -e --arg t "$tag" 'any(.[]; .tagName == $t and (.isDraft | not))' >/dev/null; then
      echo "HTTP 422: Validation Failed: tag_name already_exists ($tag)" >&2
      exit 1
    fi
    id=$(( $(state | jq 'map(.id) | max // 0') + 1 ))
    state | jq --argjson id "$id" --arg t "$tag" --arg ti "$title" --arg b "$notes" '
      . + [{id: $id, tagName: $t, isDraft: true, isPrerelease: false,
            isLatest: false, hasTag: false,
            name: $ti, body: $b, assets: []}]' | save
    if [ "$draft" = false ]; then
      # gh keeps the release a draft while it uploads, and publishes it last.
      if [ ${#files[@]} -gt 0 ]; then
        for f in $(for g in "${files[@]}"; do echo "$(fsize "$g") $g"; done | sort -n | cut -d' ' -f2); do
          if fault "upload-$(basename "$f")"; then
            echo "HTTP 500: Internal Server Error (assets?name=$(basename "$f"))" >&2
            if fault cleanup; then
              echo "cleaning up draft failed: HTTP 500" >&2
            else
              drop_release "$id"
            fi
            exit 1
          fi
          add_asset "$id" "$f"
        done
      fi
      state | jq --argjson id "$id" 'map(if .id == $id then .isDraft = false | .hasTag = true else . end)' | save
      if [ "$latest" = true ]; then make_latest "$id"; fi
    fi
    if fault create-after; then http500 "create"; fi
    ;;

  upload)
    tag="$1"; shift; clobber=false; files=()
    while [ $# -gt 0 ]; do
      case "$1" in
        --clobber) clobber=true ;;
        --*) ;;
        *) files+=("$1") ;;
      esac
      shift
    done
    id=$(resolve "$tag") || exit 1
    rc=0
    for f in "${files[@]}"; do
      name=$(basename "$f")
      if has_asset "$id" "$name"; then
        if [ "$clobber" = false ]; then
          echo "asset under the same name already exists: [$name]" >&2
          exit 1
        fi
        drop_asset "$id" "$name"
      fi
      if fault "upload-$name"; then
        echo "HTTP 500: Internal Server Error (assets?name=$name)" >&2
        rc=1
        continue
      fi
      add_asset "$id" "$f"
    done
    exit "$rc"
    ;;

  edit)
    tag="$1"; shift; draft=""; latest=false; notes=""; have_notes=false
    while [ $# -gt 0 ]; do
      case "$1" in
        --draft=false) draft=false ;;
        --draft=true|--draft) draft=true ;;
        --latest|--latest=true) latest=true ;;
        --notes-file|-F) notes=$(cat "$2"); have_notes=true; shift ;;
        --title|-t) shift ;;
      esac
      shift
    done
    id=$(resolve "$tag") || exit 1
    if fault edit; then http500 "edit"; fi
    if [ -n "$draft" ]; then
      state | jq --argjson id "$id" --argjson d "$draft" '
        map(if .id == $id then .isDraft = $d | .hasTag = (.hasTag or ($d | not)) else . end)' | save
    fi
    if [ "$have_notes" = true ]; then
      state | jq --argjson id "$id" --arg b "$notes" 'map(if .id == $id then .body = $b else . end)' | save
    fi
    if [ "$latest" = true ]; then make_latest "$id"; fi
    ;;

  delete)
    tag="$1"; shift; cleanup=false
    while [ $# -gt 0 ]; do
      case "$1" in --cleanup-tag) cleanup=true ;; esac
      shift
    done
    id=$(resolve "$tag") || exit 1
    if fault delete; then http500 "delete"; fi
    had_tag=$(state | jq --argjson id "$id" '.[] | select(.id == $id) | .hasTag')
    drop_release "$id"
    if [ "$cleanup" = true ] && [ "$had_tag" != true ]; then
      echo "HTTP 422: Reference does not exist (git/refs/tags/$tag)" >&2
      exit 1
    fi
    ;;

  *)
    echo "fake gh: unsupported release subcommand: $sub" >&2
    exit 2
    ;;
esac
