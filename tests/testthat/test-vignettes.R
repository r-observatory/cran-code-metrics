# One row per vignette, from the tarball.

mk <- function(map) build_context("p", "1.0", "1.0", "2024-01-01",
                                  names(map), function(x) map[[x]] %||% "")

test_that("each vignette is named, and named the way a reader sees it", {
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nVignetteBuilder: knitr\n",
    "vignettes/getting-started.qmd" = "---\ntitle: \"Getting started\"\n---\n")))
  expect_equal(nrow(v), 1L)
  expect_equal(v$file, "vignettes/getting-started.qmd")
  expect_equal(v$name, "getting-started")
  expect_equal(v$title, "Getting started")
  expect_equal(v$builder, "knitr")
})

test_that("the index entry wins over front matter, because R lists that one", {
  # Most sources carry both. \VignetteIndexEntry is what R itself puts in the
  # vignette index, so it is the string a user browses.
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/a.Rmd" = paste0("---\ntitle: front matter title\n---\n",
                               "<!-- %\\VignetteIndexEntry{Index entry title} -->\n"))))
  expect_equal(v$title, "Index entry title")
})

test_that("a Sweave vignette carrying only the classic marker is titled", {
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/theory.Rnw" = "%\\VignetteIndexEntry{The theory behind p}\n")))
  expect_equal(v$title, "The theory behind p")
  expect_equal(v$engine, "sweave")
})

test_that("a vignette with no declared title says so rather than guessing one", {
  # The file stem is not a title. Deriving one would invent a fact.
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/untitled.Rmd" = "Just some prose with no header at all.\n")))
  expect_true(is.na(v$title))
  expect_equal(v$name, "untitled")
})

test_that("the engine names the source, and every registered one is covered", {
  for (p in list(c("a.Rmd","rmarkdown"), c("b.qmd","quarto"), c("c.Rnw","sweave"),
                 c("d.Rtex","sweave"), c("e.Rhtml","html"), c("f.md","markdown"))) {
    v <- metrics_vignettes(mk(setNames(
      list("Package: p\nVersion: 1.0\n", "x"),
      c("DESCRIPTION", paste0("vignettes/", p[1])))))
    expect_equal(v$engine, p[2], info = p[1])
  }
})

test_that("prose and code are distinguished, in both chunk grammars", {
  knitr_chunk <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/a.Rmd" = "text\n\n```{r}\n1+1\n```\n")))
  sweave_chunk <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/b.Rnw" = "text\n<<setup>>=\nx <- 1\n@\n")))
  prose_only <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/c.Rmd" = "---\ntitle: t\n---\n\nOnly words here.\n")))
  expect_equal(knitr_chunk$has_code, 1L)
  expect_equal(sweave_chunk$has_code, 1L)
  expect_equal(prose_only$has_code, 0L)
})

test_that("a package shipping no vignettes yields a measured absence", {
  # Zero rows, not an error and not a blank row: the tarball was read and held
  # no vignette source.
  v <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
                                 "R/f.R" = "f <- function() 1\n")))
  expect_equal(nrow(v), 0L)
  expect_true(all(c("file","name","engine","title","builder","lines","has_code") %in% names(v)))
})

test_that("a README at the root is never a vignette", {
  v <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
                                 "README.Rmd" = "---\ntitle: readme\n---\n")))
  expect_equal(nrow(v), 0L)
})

test_that("rows are ordered, so a diff between versions is readable", {
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/zebra.Rmd" = "x", "vignettes/apple.Rmd" = "y")))
  expect_equal(v$name, c("apple", "zebra"))
})
