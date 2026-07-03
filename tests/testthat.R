library(testthat)

# Source all scripts in dependency order.
source("scripts/config.R")
source("scripts/git.R")
source("scripts/context.R")
source("scripts/metrics/structure.R")
source("scripts/analyze.R")

test_dir("tests/testthat", stop_on_failure = TRUE)
