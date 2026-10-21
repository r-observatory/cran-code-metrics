# tests/testthat/helper-version-skip.R: runs with the version skip on or off.

# The suite runs in record mode whatever the shell exports; a skip test turns it on.
.local_global("VERSION_SKIP", FALSE, frame = testthat::teardown_env())

# One .vst_run() with the version skip on, or off when skip is FALSE.
.vs_run <- function(out_dir, root, latest, skip = TRUE, ...) {
  .local_global("VERSION_SKIP", skip)
  .vst_run(out_dir, root, latest, ...)
}

# .package_rows() without the columns a run stamps with its own time or
# numbering, each frame ordered by the columns it keeps.
.vs_rows <- function(out_dir, pkg) {
  rows <- .package_rows(out_dir, pkg)
  lapply(rows, function(df) {
    df <- df[, setdiff(names(df), c("read_at", "assessed_at", "last_attempt", "content_id",
                                    "current_content_id")), drop = FALSE]
    df <- df[do.call(order, unname(as.list(df))), , drop = FALSE]
    rownames(df) <- NULL
    df
  })
}
