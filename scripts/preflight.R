# scripts/preflight.R: settle what this run's baseline is, and check the
# downloaded prior databases against it, before any shard writes to them.
#
# Runs once per run, from the workflow's download step. It cannot live inside
# the shard loop: the comparison it makes holds only on the first shard,
# because every later shard has legitimately added rows to the same file while
# prev-*-manifest.json still describes yesterday's release.
#
# Two things happen here, in this order. A release that published a database
# and no manifest gets a baseline measured from that database, because the
# publish is four assets in one non-atomic --clobber and can be interrupted
# between them; refusing on the resulting pair made a transient upload failure
# permanent, since the same release stays latest tomorrow. Then each database
# is compared against the manifest that shipped with it, and only a database
# holding LESS than its manifest recorded stops the run.

if (identical(sys.nframe(), 0L)) {
  # R prints at most warning.length bytes of an error and drops the rest, and
  # the default 1000 cuts the repair instructions off the end of this one.
  options(warning.length = 8170L)

  .script_dir <- {
    fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(fa) >= 1L) dirname(sub("^--file=", "", fa[1L])) else "scripts"
  }
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "retention.R"))

  args    <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[1L] else "out"

  derived <- ensure_prior_baseline(out_dir)
  checked <- preflight_prior_dbs(out_dir)
  for (n in c(derived, checked$notes)) {
    cat(sprintf("::warning::%s\n", n), file = stderr())
  }
  if (length(checked$violations) > 0L) {
    for (p in checked$violations) cat(sprintf("::error::%s\n", p), file = stderr())
    stop("the prior database holds less than the manifest published with it ",
         "recorded; refusing to build a release on top of it.",
         retention_repair_advice(), call. = FALSE)
  }
  cat("prior databases hold everything the manifests published with them recorded\n")
}
