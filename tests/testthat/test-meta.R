# tests/testthat/test-meta.R

# Helper: build a context from a DESCRIPTION string (and optional extras).
make_meta_ctx <- function(desc_text, extra_files = list()) {
  map     <- c(list(DESCRIPTION = desc_text), extra_files)
  read_fn <- function(p) map[[p]] %||% ""
  build_context("testpkg", "1.0.0", "1.0.0", "2024-01-01",
                names(map), read_fn)
}

# ===========================================================================
# n_deps_direct + dep_list
# ===========================================================================

test_that("metrics_meta: n_deps_direct counts Imports + Depends, excluding R", {
  ctx <- make_meta_ctx(paste0(
    "Package: p\nVersion: 1.0\n",
    "Imports: dplyr (>= 1.0), rlang\n",
    "Depends: R (>= 4.0), methods\n"
  ))
  m <- metrics_meta(ctx)

  # dplyr, rlang, methods = 3; R is excluded
  expect_equal(m$n_deps_direct, 3L)
  deps <- jsonlite::fromJSON(m$dep_list)
  expect_equal(sort(deps), c("dplyr", "methods", "rlang"))
})

test_that("metrics_meta: n_deps_direct and dep_list are NA when DESCRIPTION absent", {
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       character(0L), function(p) "")
  m <- metrics_meta(ctx)

  expect_true(is.na(m$n_deps_direct))
  expect_true(is.na(m$dep_list))
})

test_that("metrics_meta: n_deps_direct is 0 and dep_list is [] when no deps fields", {
  ctx <- make_meta_ctx("Package: p\nVersion: 1.0\n")
  m   <- metrics_meta(ctx)

  expect_equal(m$n_deps_direct, 0L)
  expect_equal(m$dep_list, "[]")
})

test_that("metrics_meta: dep_list excludes R when only R is in Depends", {
  ctx <- make_meta_ctx("Package: p\nVersion: 1.0\nDepends: R (>= 3.5)\n")
  m   <- metrics_meta(ctx)

  expect_equal(m$n_deps_direct, 0L)
  expect_equal(m$dep_list, "[]")
})

# ===========================================================================
# maintainer + maintainer_email
# ===========================================================================

test_that("metrics_meta: maintainer name and email extracted from 'Name <email>' format", {
  ctx <- make_meta_ctx(
    "Package: p\nVersion: 1.0\nMaintainer: Jane Doe <jane@example.com>\n"
  )
  m <- metrics_meta(ctx)

  expect_equal(m$maintainer, "Jane Doe")
  expect_equal(m$maintainer_email, "jane@example.com")
})

test_that("metrics_meta: maintainer and maintainer_email are NA when Maintainer field absent", {
  ctx <- make_meta_ctx("Package: p\nVersion: 1.0\n")
  m   <- metrics_meta(ctx)

  expect_true(is.na(m$maintainer))
  expect_true(is.na(m$maintainer_email))
})

test_that("metrics_meta: maintainer_email is NA when Maintainer has no angle brackets", {
  ctx <- make_meta_ctx(
    "Package: p\nVersion: 1.0\nMaintainer: Jane Doe\n"
  )
  m <- metrics_meta(ctx)

  expect_equal(m$maintainer, "Jane Doe")
  expect_true(is.na(m$maintainer_email))
})

# ===========================================================================
# n_authors + authors (Authors@R path)
# ===========================================================================

test_that("metrics_meta: n_authors counts person() calls in Authors@R", {
  ctx <- make_meta_ctx(paste0(
    "Package: p\nVersion: 1.0\n",
    'Authors@R: c(person("Jane", "Doe", role = "aut"),',
    ' person("John", "Smith", role = c("ctb", "cre")))\n'
  ))
  m <- metrics_meta(ctx)

  expect_equal(m$n_authors, 2L)
})

test_that("metrics_meta: authors JSON contains correct given/family/roles from Authors@R", {
  ctx <- make_meta_ctx(paste0(
    "Package: p\nVersion: 1.0\n",
    'Authors@R: person("Hadley", "Wickham", role = c("aut", "cre"))\n'
  ))
  m  <- metrics_meta(ctx)
  au <- jsonlite::fromJSON(m$authors, simplifyVector = FALSE)

  expect_equal(length(au), 1L)
  expect_equal(au[[1L]]$given,  "Hadley")
  expect_equal(au[[1L]]$family, "Wickham")
  expect_true("aut" %in% unlist(au[[1L]]$roles))
  expect_true("cre" %in% unlist(au[[1L]]$roles))
})

test_that("metrics_meta: n_authors and authors are NA when no author fields present", {
  ctx <- make_meta_ctx("Package: p\nVersion: 1.0\n")
  m   <- metrics_meta(ctx)

  expect_true(is.na(m$n_authors))
  expect_true(is.na(m$authors))
})

test_that("metrics_meta: Authors@R handles single-quoted string arguments", {
  ctx <- make_meta_ctx(paste0(
    "Package: p\nVersion: 1.0\n",
    "Authors@R: person('Jane', 'Doe', role = 'aut')\n"
  ))
  m  <- metrics_meta(ctx)
  au <- jsonlite::fromJSON(m$authors, simplifyVector = FALSE)

  expect_equal(m$n_authors, 1L)
  expect_equal(au[[1L]]$given,  "Jane")
  expect_equal(au[[1L]]$family, "Doe")
  expect_equal(unlist(au[[1L]]$roles), "aut")
})

# ===========================================================================
# n_authors + authors (Author free-text fallback)
# ===========================================================================

test_that("metrics_meta: n_authors falls back to Author field comma-separated count", {
  ctx <- make_meta_ctx(
    "Package: p\nVersion: 1.0\nAuthor: Jane Doe [aut], John Smith [ctb]\n"
  )
  m  <- metrics_meta(ctx)
  au <- jsonlite::fromJSON(m$authors, simplifyVector = FALSE)

  expect_equal(m$n_authors, 2L)
  expect_equal(length(au), 2L)
  expect_equal(au[[1L]]$family, "Doe")
  expect_equal(au[[2L]]$family, "Smith")
})

test_that("metrics_meta: Author field splits on 'and' separator", {
  ctx <- make_meta_ctx(
    "Package: p\nVersion: 1.0\nAuthor: Jane Doe and John Smith\n"
  )
  m <- metrics_meta(ctx)

  expect_equal(m$n_authors, 2L)
})

test_that("metrics_meta: Author field roles extracted from [bracket] notation", {
  ctx <- make_meta_ctx(
    "Package: p\nVersion: 1.0\nAuthor: Jane Doe [aut, cre]\n"
  )
  m  <- metrics_meta(ctx)
  au <- jsonlite::fromJSON(m$authors, simplifyVector = FALSE)

  expect_equal(m$n_authors, 1L)
  roles <- trimws(unlist(au[[1L]]$roles))
  expect_true("aut" %in% roles)
  expect_true("cre" %in% roles)
})

# ===========================================================================
# NA-safety: never errors on absent / empty / malformed input
# ===========================================================================

test_that("metrics_meta: all metrics NA-safe on empty DESCRIPTION content", {
  # DESCRIPTION present as a file but with no parseable content
  ctx <- make_meta_ctx("")
  m   <- metrics_meta(ctx)

  # Should return 0 deps (DESCRIPTION file exists, just empty/no Imports)
  expect_true(is.numeric(m$n_deps_direct) || is.na(m$n_deps_direct))
  expect_true(is.na(m$maintainer))
  expect_true(is.na(m$maintainer_email))
  # n_authors and authors: both fields absent in empty DESCRIPTION
  expect_true(is.na(m$n_authors))
  expect_true(is.na(m$authors))
})

test_that("metrics_meta: returns named list with all expected metrics", {
  ctx <- make_meta_ctx("Package: p\nVersion: 1.0\n")
  m   <- metrics_meta(ctx)

  expect_true(all(c("n_deps_direct", "dep_list", "maintainer",
                    "maintainer_email", "n_authors", "authors",
                    "title", "description") %in% names(m)))
})

# ===========================================================================
# title + description
# ===========================================================================

test_that("metrics_meta: title and description are taken from the DESCRIPTION", {
  ctx <- make_meta_ctx(paste0(
    "Package: p\nVersion: 1.0\n",
    "Title: A Concise Title\n",
    "Description: A longer description of what\n",
    "  the package does, across two lines.\n"
  ))
  m <- metrics_meta(ctx)

  expect_equal(m$title, "A Concise Title")
  # Continuation lines are folded (parse_dcf joins them with a newline).
  expect_true(grepl("across two lines", m$description, fixed = TRUE))
})

test_that("metrics_meta: title and description are NA when the fields are absent", {
  ctx <- make_meta_ctx("Package: p\nVersion: 1.0\n")
  m   <- metrics_meta(ctx)

  expect_true(is.na(m$title))
  expect_true(is.na(m$description))
})

test_that("metrics_meta: title and description are NA when DESCRIPTION is absent", {
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       character(0L), function(p) "")
  m <- metrics_meta(ctx)

  expect_true(is.na(m$title))
  expect_true(is.na(m$description))
})

test_that("metrics_meta: an empty Title field becomes NA (not empty string)", {
  ctx <- make_meta_ctx("Package: p\nVersion: 1.0\nTitle:\nDescription:\n")
  m   <- metrics_meta(ctx)

  expect_true(is.na(m$title))
  expect_true(is.na(m$description))
})

# ===========================================================================
# meta_typed_deps: raw typed dependency fields for the per-version summary
# ===========================================================================

test_that("meta_typed_deps returns the five fields verbatim, LinkingTo -> linkingto", {
  desc <- parse_dcf(paste0(
    "Package: p\nVersion: 1.0\n",
    "Depends: R (>= 4.0), methods\n",
    "Imports: dplyr (>= 1.0), rlang\n",
    "Suggests: testthat\n",
    "LinkingTo: Rcpp\n",
    "Enhances: sp\n"
  ))
  td <- meta_typed_deps(desc)

  expect_identical(names(td),
                   c("depends", "imports", "suggests", "linkingto", "enhances"))
  # Raw field text is preserved verbatim (version constraints included).
  expect_equal(td$depends,   "R (>= 4.0), methods")
  expect_equal(td$imports,   "dplyr (>= 1.0), rlang")
  expect_equal(td$suggests,  "testthat")
  expect_equal(td$linkingto, "Rcpp")
  expect_equal(td$enhances,  "sp")
})

test_that("meta_typed_deps: absent field is NA (honest-NA, not empty string)", {
  desc <- parse_dcf("Package: p\nVersion: 1.0\nImports: stats\n")
  td   <- meta_typed_deps(desc)

  expect_equal(td$imports, "stats")
  expect_true(is.na(td$depends))
  expect_true(is.na(td$suggests))
  expect_true(is.na(td$linkingto))
  expect_true(is.na(td$enhances))
})

test_that("meta_typed_deps: an empty field value is NA (not empty string)", {
  desc <- parse_dcf("Package: p\nVersion: 1.0\nImports:\nSuggests: testthat\n")
  td   <- meta_typed_deps(desc)

  expect_true(is.na(td$imports))
  expect_equal(td$suggests, "testthat")
})

test_that("meta_typed_deps: an empty DESCRIPTION list yields NA for every field", {
  td <- meta_typed_deps(list())

  expect_true(all(vapply(td, is.na, logical(1L))))
})
