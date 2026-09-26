# scripts/preflight.R: settle what this run's baseline is, and check the
# downloaded prior databases against it, before any shard writes to them.
#
# Runs once per run, from the workflow's download step. It cannot live inside
# the shard loop: the comparison it makes holds only on the first shard,
# because every later shard has legitimately added rows to the same file while
# prev-*-manifest.json still describes yesterday's release.
#
# Two things happen here, in this order. A release that published a database
# and no manifest gets a baseline measured from that database, because a
# same-day republish replaces its assets one at a time and can be interrupted
# between them; refusing on the resulting pair made a transient upload failure
# permanent, since the same release stays latest tomorrow. The download step
# repairs a replacement that was cut off mid-swap before this runs, so what
# reaches here is a release whose assets are each whole and possibly of
# different ages. Then each database is compared against the manifest that shipped
# with it, and only a database holding LESS than its manifest recorded, or a
# manifest whose database never arrived, stops the run.

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
  flag    <- function(name) sub(sprintf("^--%s=", name), "",
                                grep(sprintf("^--%s=", name), args, value = TRUE)[1L])
  positional <- args[!startsWith(args, "--")]
  out_dir <- if (length(positional) >= 1L) positional[1L] else "out"
  code_tag <- flag("code-src") %||% ""
  text_tag <- flag("text-src") %||% ""

  derived <- ensure_prior_baseline(out_dir)
  checked <- preflight_prior_dbs(out_dir)
  pairing <- text_code_pairing(out_dir, code_tag, text_tag)
  for (n in c(derived, checked$notes, pairing$notes)) {
    cat(sprintf("::warning::%s\n", n), file = stderr())
  }
  # Each shard's run-status.json repeats this, so a mismatch outlives this step.
  jsonlite::write_json(list(text_code_mismatch = pairing$text_code_mismatch,
                            code_tag = code_tag, text_tag = text_tag),
                       file.path(out_dir, "text-code-check.json"), auto_unbox = TRUE)
  if (length(checked$violations) > 0L) {
    for (p in checked$violations) cat(sprintf("::error::%s\n", p), file = stderr())
    stop(preflight_refusal(checked$violations), call. = FALSE)
  }
  cat("prior databases hold everything the manifests published with them recorded\n")
}
