test_that("releases_to_prune keeps newest N and every first-of-month", {
  days <- sprintf("code-2026-06-%02d", 1:30)          # 30 dailies in June
  extra <- c("code-2026-05-01", "code-2026-05-15", "code-2026-04-01")
  tags <- c(days, extra)
  del <- releases_to_prune(tags, keep = 30L)
  # Newest 30 (all of June) are kept.
  expect_false(any(grepl("2026-06", del)))
  # First-of-month always kept.
  expect_false("code-2026-05-01" %in% del)
  expect_false("code-2026-04-01" %in% del)
  # A non-first-of-month older daily is pruned.
  expect_true("code-2026-05-15" %in% del)
})

test_that("nothing is pruned when under the keep threshold", {
  expect_identical(releases_to_prune(sprintf("code-2026-06-%02d", 1:10), keep = 30L),
                   character(0L))
})

test_that("keep = Inf selects nothing among 400 tags", {
  tags <- format(as.Date("2025-01-01") + 0:399)
  expect_identical(releases_to_prune(paste0("metrics-", tags), keep = Inf),
                   character(0L))
})

test_that("parse_keep maps all to Inf and leaves numbers alone", {
  expect_identical(parse_keep("all"), Inf)
  expect_identical(parse_keep(" ALL "), Inf)
  expect_identical(parse_keep("30"), 30L)
  expect_identical(parse_keep("5"), 5L)
  expect_error(parse_keep("lots"), "KEEP")
})

test_that("the script prints nothing for KEEP=all and still prunes for a number", {
  tags <- paste0("metrics-", format(as.Date("2025-01-01") + 0:399))
  script <- file.path("..", "..", "scripts", "prune.R")
  run <- function(keep) {
    f <- tempfile(); on.exit(unlink(f)); writeLines(tags, f)
    system2("Rscript", script, stdin = f, stdout = TRUE,
            env = paste0("KEEP=", keep))
  }
  out_all <- run("all")
  expect_null(attr(out_all, "status"))
  expect_length(out_all, 0L)
  out_30 <- run("30")
  expect_null(attr(out_30, "status"))
  expect_gt(length(out_30), 0L)
})

test_that("the workflow prune step sets KEEP to all", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  start <- grep("- name: Prune old dated releases", yml, fixed = TRUE)
  expect_length(start, 1L)
  step <- yml[start:length(yml)]
  expect_true(any(grepl('^\\s*KEEP: "all"\\s*$', step)))
  expect_false(any(grepl('KEEP: "30"', step, fixed = TRUE)))
})
