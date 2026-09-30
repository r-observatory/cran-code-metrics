# cran-code-metrics

This pipeline computes per-version code and quality metrics for CRAN packages
by cloning each package's `github.com/cran` repository (one commit per CRAN
release) and analyzing every version. It publishes the results as a SQLite
database to the `r-observatory/cran-code-metrics` GitHub repository for
downstream consumers.

For each package version it records structural metrics (file counts, lines of
code by language, compiled-code share), function counts (exported vs internal),
documentation and testing signals, security and code-health scanners,
portability and licensing fields, and per-file churn (added and deleted lines
per release, from `git log --numstat`). Cross-version metrics (release cadence,
API stability, dependency drift, cold-removal rate, and more) are derived from
the ordered version series.

## Output

`cran-code-metrics.db` (published as a dated `metrics-YYYY-MM-DD` release; a
release is immutable once a later day's release exists, and old releases are
pruned on a retention schedule):

- `cran_code_summary` - one row per package version, with the metric columns and
  a per-version release date.
- `cran_code_churn` - added and deleted lines per file per version.
- `cran_api_history` - exported-symbol additions and removals per version.
- `cran_description_fields` - the latest analysed version's RdMacros,
  RoxygenNote, SystemRequirements, Language, LazyData, Date and `Config/*`
  DESCRIPTION fields, each value capped at 16,384 bytes.
- `cran_release_notes` - the NEWS section for the latest analysed version, when
  the analyzer found one, capped at 16,384 bytes.

`cran-data-metrics.db` is published in the same release and holds the
dataset-focused tables.

`cran-release-text.db` is published in the same release and keeps the text
history: every DESCRIPTION field (`cran_description_history`) and NEWS section
(`cran_release_notes_history`) of every analysed version, with
`cran_release_text_versions` recording which versions were read. A run takes it
from the newest release that carries it and starts it empty only when no
release ever has.

Each dated release carries `code-manifest.json`, `data-manifest.json` and
`text-manifest.json`. A separate `run-status.json`, written alongside but not
published, carries the `changed` and `bootstrap_complete` flags that drive the
shard loop.

The databases are published zstd-compressed, as `cran-code-metrics.db.zst`, `cran-data-metrics.db.zst` and `cran-release-text.db.zst`, and `zstd -d cran-code-metrics.db.zst` gives back the SQLite file. A release carries one form of each database, and releases from before the switch carry the plain `.db` files. A manifest describes the SQLite file (`db_filename`, `db_bytes`, `db_sha256`) and the asset that carries it (`asset_filename`, `asset_bytes`, `asset_sha256`). Setting `PUBLISH_FORM=plain` for the workflow publishes the plain files again.

## Retired columns

These columns are no longer published in `cran_code_summary`. Each leaves the
database on the first shard written by the analyzer version named.

- `has_website`, `copyright_holder_declared` (analyzer 0.5.0)

## Running

```sh
Rscript tests/testthat.R          # unit tests
Rscript scripts/update.R out/     # analyze the next shard of packages, carry-forward
Rscript scripts/update.R out/ --bootstrap   # re-analyze everything from scratch
```

The update reads the prior databases from `out/`, analyzes a shard of packages
that are new or have a new release, and writes the updated code and dataset
databases plus their manifests. The bootstrap fills the full catalog over
several runs: each shard is published so progress survives a restart, and the
workflow keeps starting shards until the catalog is complete or a time budget
is reached. Set `GITHUB_TOKEN` so git fetches are authenticated.

The tests run from the repository root and need R with RSQLite, DBI, jsonlite, digest, testthat and withr, plus bash, jq, zstd, git and sqlite3 on PATH. The release tests in `test-publish.R` skip without bash or jq, and those that read or write a `.zst` skip without zstd. Point `RPKG_ANALYZER_BIN` at an rpkg-analyzer binary (the workflows pin v0.4.0) to run the tests that need a real analyzer; without one they skip. A few tests skip when `rpkg-analyzer` is on PATH, so name the binary through the variable rather than PATH.

`test-record-memo.R` also runs the builds that `RPA_TEST_BIN_040` and `RPA_TEST_BIN_050` name. `test-record-parse-corpus.R` holds the record parse and the dataset memo to the per-line parser over whole corpora of analyzer output when `RPA_PARSE_CORPUS_050` and `RPA_PARSE_CORPUS_040` name them as absolute paths: every `*.ndjson` or `*.ndjson.gz` file below, where the files under `mv/<package>/` are one package's versions in name order. With `RPA_PARSE_CORPUS_REPORT` set it appends one line per corpus, the label, the files read and the files found identical, separated by tabs.

## Notes

Each package is cloned, analyzed across all its versions, and deleted before the
next one, so peak disk stays small. Repositories that no longer exist are skipped
and recorded in the manifest. All metrics are computed from git and the package
source; there is no external `cloc` dependency.

Moving the rpkg-analyzer pin re-queues every package unless `ANALYZER_SAME_OUTPUT` in `scripts/config.R` also names the build the stored rows came from. Add a build there only when the analyzer gate has shown that it reproduces the pinned build record for record, and quote the gate report in the pin PR; to rescan on purpose, set the list to the new pin alone. `test-output-class.R` fails while the pinned build is missing from the list, so the choice is made in the PR that moves the pin. Each run prints the output class and how many latest rows it covers, and the manifests carry `analyzer_version`, `output_class` and `n_latest_on_build` under `bootstrap`.

Each worker gives rpkg-analyzer a directory of its own for the package it analyses, `work/.rpa/<package>`, and removes it when the package is done. `RPKG_ANALYZER_CACHE_DIR` names a cache there, so a compiled file that did not change between versions is parsed once, and `RPKG_ANALYZER_STATS` names a statistics file. Builds before 0.5.1 read neither variable. Set `RPA_CACHE` to `off` (or `false`, `no`, `0`) in the workflow's environment to leave the cache out; the output is the same either way.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
