# scripts/preflight.R: check the downloaded prior databases against the
# manifests published alongside them, before any shard writes to them.
#
# Runs once per run, from the workflow's download step. It cannot live inside
# the shard loop: the equality it asserts holds only on the first shard,
# because every later shard has legitimately added rows to the same file while
# prev-*-manifest.json still describes yesterday's release.

if (identical(sys.nframe(), 0L)) {
  .script_dir <- {
    fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(fa) >= 1L) dirname(sub("^--file=", "", fa[1L])) else "scripts"
  }
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "retention.R"))

  args    <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[1L] else "out"

  problems <- preflight_prior_dbs(out_dir)
  if (length(problems) > 0L) {
    for (p in problems) cat(sprintf("::error::%s\n", p), file = stderr())
    stop("the prior database does not match the manifest published with it; ",
         "refusing to build a release on top of it.", call. = FALSE)
  }
  cat("prior databases agree with the manifests published with them\n")
}
