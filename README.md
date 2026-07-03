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

`cran-code-metrics.db` (published on the rolling `current` release):

- `cran_code_summary` - one row per package version, with the metric columns and
  a per-version release date.
- `cran_code_churn` - added and deleted lines per file per version.
- `cran_api_history` - exported-symbol additions and removals per version.

A `manifest.json` accompanies the database with a `changed` flag and a
`bootstrap_complete` flag.

## Running

```sh
Rscript tests/testthat.R          # unit tests
Rscript scripts/update.R out/     # analyze the next shard of packages, carry-forward
Rscript scripts/update.R out/ --bootstrap   # re-analyze everything from scratch
```

The update reads the prior database from `out/`, analyzes a shard of packages
that are new or have a new release, and writes an updated database plus manifest.
The bootstrap fills the full catalog over several runs: each shard is published
so progress survives a restart, and the workflow keeps starting shards until the
catalog is complete or a time budget is reached. Set `GITHUB_TOKEN` so git
fetches are authenticated.

## Notes

Each package is cloned, analyzed across all its versions, and deleted before the
next one, so peak disk stays small. Repositories that no longer exist are skipped
and recorded in the manifest. All metrics are computed from git and the package
source; there is no external `cloc` dependency.
