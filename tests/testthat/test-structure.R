test_that("metrics_structure computes correct LOC counts from a fixture ctx", {
  file_map <- list(
    "DESCRIPTION"           = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"             = "export(foo)\n",
    "R/foo.R"               = "foo <- function(x) {\n  x + 1\n}\n",
    "R/bar.R"               = "bar <- function() NULL\n",
    "src/mypkg.c"           = "#include <R.h>\nvoid hello(void) {}\n",
    "tests/testthat/test-foo.R" = "test_that('ok', expect_equal(1,1))\n",
    "man/foo.Rd"            = "\\name{foo}\n\\alias{foo}\n\\title{Foo}\n",
    "vignettes/intro.Rmd"   = "# Introduction\n\nSome text here.\n",
    "MD5"                   = "abc123  DESCRIPTION\n"  # non-code
  )
  read_fn <- function(p) file_map[[p]] %||% ""
  files   <- names(file_map)

  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01", files, read_fn)
  m   <- metrics_structure(ctx)

  expect_equal(m$n_files, length(files))

  # R/ LOC: foo.R has 3 lines, bar.R has 1 line
  expect_equal(m$loc_r, 3L + 1L)

  # src/ LOC: mypkg.c has 2 lines
  expect_equal(m$loc_src, 2L)

  # tests/ LOC
  expect_equal(m$loc_tests, 1L)

  # man/ LOC
  expect_equal(m$loc_docs, 3L)

  # vignettes/ LOC
  expect_equal(m$loc_vignettes, 3L)

  # loc_total = sum of all classified categories
  expect_equal(m$loc_total, m$loc_r + m$loc_src + m$loc_tests +
                              m$loc_docs + m$loc_vignettes)

  # compiled_share = loc_src / loc_total
  expect_equal(m$compiled_share, m$loc_src / m$loc_total, tolerance = 1e-9)

  # has_src: TRUE because src/mypkg.c exists
  expect_true(m$has_src)

  # lang_breakdown is parseable JSON
  bd <- jsonlite::fromJSON(m$lang_breakdown)
  expect_true(is.list(bd) || is.numeric(bd))
  # R files have extension "R"
  expect_true("R" %in% names(bd))
})

test_that("metrics_structure: no src/ gives has_src=FALSE and compiled_share=0", {
  file_map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "R/a.R"       = "a <- 1\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")
  m   <- metrics_structure(ctx)

  expect_false(m$has_src)
  expect_equal(m$compiled_share, 0)
  expect_equal(m$loc_src, 0L)
})

test_that("metrics_structure: MD5 and binary files excluded from LOC, counted in n_files", {
  file_map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "R/a.R"       = "a <- 1\nb <- 2\n",
    "MD5"         = "deadbeef  DESCRIPTION\n",
    "data/x.rda"  = "(binary)"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")
  m   <- metrics_structure(ctx)

  # All 4 files counted
  expect_equal(m$n_files, 4L)
  # Only R/a.R (2 lines) counted in loc_total
  expect_equal(m$loc_r,    2L)
  expect_equal(m$loc_total, 2L)
})

test_that("metrics_structure returns parseable lang_breakdown JSON", {
  file_map <- list(
    "R/a.R"           = "a <- 1\n",
    "src/b.cpp"       = "int main() {}\n",
    "tests/test-a.R"  = "test_that('x', TRUE)\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")
  m   <- metrics_structure(ctx)

  bd <- jsonlite::fromJSON(m$lang_breakdown)
  expect_true("R"   %in% names(bd))
  expect_true("cpp" %in% names(bd))
})

test_that("metrics_structure handles empty file list (NA-safe)", {
  ctx <- build_context("p", "0.1", "0.1", "2024-01-01",
                       character(0L), function(p) "")
  m   <- metrics_structure(ctx)

  expect_equal(m$n_files,   0L)
  expect_equal(m$loc_total, 0L)
  expect_equal(m$loc_r,     0L)
  expect_false(m$has_src)
  expect_equal(m$compiled_share, 0)
})

test_that("metrics_structure: inst/CITATION is detected as a shipped citation", {
  # utils::citation() reads inst/CITATION from the installed package, so what
  # matters is whether the tarball shipped one. That is what this file list is:
  # the contents of a released version, not a repository at HEAD.
  mk <- function(files) {
    fm <- setNames(as.list(rep("x\n", length(files))), files)
    build_context("mypkg", "1.0", "1.0", "2024-01-01", files,
                  function(p) fm[[p]] %||% "")
  }

  with_it <- metrics_structure(mk(c("DESCRIPTION", "R/foo.R", "inst/CITATION")))
  expect_true(with_it$has_citation)

  without <- metrics_structure(mk(c("DESCRIPTION", "R/foo.R")))
  expect_false(without$has_citation)

  # A CITATION.cff at the root is the CFF standard, read by repository hosts.
  # It is not the file citation() reads and must not be mistaken for it.
  cff <- metrics_structure(mk(c("DESCRIPTION", "CITATION.cff")))
  expect_false(cff$has_citation)

  # Nor is a CITATION anywhere else in the tree the installed one.
  expect_false(metrics_structure(mk(c("CITATION")))$has_citation)
  expect_false(metrics_structure(mk(c("man/CITATION")))$has_citation)
  expect_false(metrics_structure(mk(c("inst/extdata/CITATION")))$has_citation)

  # Shipping both is normal and each is reported on its own terms.
  both <- metrics_structure(mk(c("inst/CITATION", "CITATION.cff")))
  expect_true(both$has_citation)
})
