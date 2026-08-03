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
  expect_equal(v$format, "sweave")
})

test_that("a vignette with no declared title says so rather than guessing one", {
  # The file stem is not a title. Deriving one would invent a fact.
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/untitled.Rmd" = "Just some prose with no header at all.\n")))
  expect_true(is.na(v$title))
  expect_equal(v$name, "untitled")
})

test_that("the source format is read from the extension, for every engine", {
  for (p in list(c("a.Rmd","rmarkdown"), c("b.qmd","quarto"), c("c.Rnw","sweave"),
                 c("d.Rtex","sweave"), c("e.Rhtml","html"), c("f.md","markdown"))) {
    v <- metrics_vignettes(mk(setNames(
      list("Package: p\nVersion: 1.0\n", "x"),
      c("DESCRIPTION", paste0("vignettes/", p[1])))))
    expect_equal(v$format, p[2], info = p[1])
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

# ---------------------------------------------------------------------------
# What it renders to, whether it was built ahead of time, and who wrote it
# ---------------------------------------------------------------------------

test_that("the declared engine names the output the extension cannot", {
  # livelink's real header. Saying only "the extension does not say" understated
  # this: the source usually does say, in the engine declaration.
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/a.qmd" = paste0("---\ntitle: A\nvignette: >\n",
                               "  %\\VignetteEngine{quarto::html}\n---\n"))))
  expect_equal(v$engine, "quarto::html")
  expect_equal(v$output, "html")
})

test_that("a pre-built document is marked as shipped rather than built", {
  # jsonlite's real json-mapping.pdf.asis. R.rsp::asis installs a finished PDF
  # as the vignette: nothing in it runs, and its source is a few marker lines.
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nVignetteBuilder: R.rsp\n",
    "vignettes/json-mapping.pdf.asis" = paste0(
      "%\\VignetteIndexEntry{A mapping between JSON data and R objects}\n",
      "%\\VignetteEngine{R.rsp::asis}\n"))))
  expect_equal(nrow(v), 1L)
  expect_equal(v$prebuilt, 1L)
  expect_equal(v$output, "pdf")          # from the .pdf.asis filename
  expect_equal(v$format, "asis")
  expect_equal(v$title, "A mapping between JSON data and R objects")
})

test_that("the precompute pattern is recognised by its .orig neighbour", {
  # An .Rmd generated from an .Rmd.orig ran its expensive code before
  # submission rather than on CRAN's machines. Different artifact, same
  # extension, so only the neighbour distinguishes them.
  v <- metrics_vignettes(mk(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/slow.Rmd" = "---\ntitle: Slow\n---\n",
    "vignettes/slow.Rmd.orig" = "the real source\n",
    "vignettes/fast.Rmd" = "---\ntitle: Fast\n---\n")))
  expect_equal(v$precomputed[v$name == "slow"], 1L)
  expect_equal(v$precomputed[v$name == "fast"], 0L)
})

test_that("Sweave renders to PDF by construction even with nothing declared", {
  v <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
                                 "vignettes/old.Rnw" = "\\documentclass{article}\n")))
  expect_equal(v$output, "pdf")
})

test_that("an output nothing states is NA rather than a guess", {
  # An .Rmd with no engine, no output: and no format: could render to either.
  v <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
                                 "vignettes/a.Rmd" = "just prose, no header\n")))
  expect_true(is.na(v$output))
})

test_that("the author is read where stated and NA where not", {
  # Stated on roughly half the .Rmd vignettes on GitHub, so absence is the
  # common case and means the vignette does not say, not that the maintainer
  # wrote it.
  yaml <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/a.Rmd" = "---\ntitle: A\nauthor: Jane Roe\n---\n")))
  sweave <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/b.Rnw" = "\\author{John Doe}\n")))
  silent <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/c.Rmd" = "---\ntitle: C\n---\n")))
  expect_equal(yaml$author, "Jane Roe")
  expect_equal(sweave$author, "John Doe")
  expect_true(is.na(silent$author))
})

test_that("every shape a vignette states its authors in is read", {
  # Four shapes occur and all four are common enough to matter. A scalar-only
  # reader put the block forms into the same bucket as the vignettes that name
  # nobody: 2,956 of the 14,672 authored .Rmd vignettes on GitHub use the
  # name-mapping block, and 2,840 .Rnw vignettes use \\and.
  shapes <- list(
    scalar   = "---\ntitle: A\nauthor: Jane Roe\n---\n",
    flow     = "---\ntitle: A\nauthor: [Ada Lovelace, Bob Stone]\n---\n",
    block    = "---\ntitle: A\nauthor:\n  - Ada Lovelace\n  - Bob Stone\n---\n",
    mappings = "---\ntitle: A\nauthor:\n  - name: Ada Lovelace\n    affiliation: X\n  - name: Bob Stone\n---\n")
  for (nm in names(shapes)) {
    v <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
                                   "vignettes/a.Rmd" = shapes[[nm]])))
    expect_equal(v$author_stated, 1L, info = nm)
    if (nm == "scalar") {
      expect_equal(v$author, "Jane Roe"); expect_equal(v$n_authors, 1L)
    } else {
      expect_equal(v$author, "Ada Lovelace, Bob Stone", info = nm)
      expect_equal(v$n_authors, 2L, info = nm)
    }
  }
})

test_that("a Sweave vignette splits its authors on and, and on commas", {
  a <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/a.Rnw" = "\\author{Ada Lovelace \\and Bob Stone}\n")))
  expect_equal(a$n_authors, 2L)
  expect_equal(a$author, "Ada Lovelace, Bob Stone")
})

test_that("naming nobody is not the same row as naming people unreadably", {
  # The conflation this replaced. Both used to be author = NA, so a shape the
  # parser did not read vanished into the population that states no author, and
  # nothing counted how large that was.
  silent <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/a.Rmd" = "---\ntitle: A\n---\n")))
  expect_equal(silent$author_stated, 0L)
  expect_true(is.na(silent$author))
  expect_true(is.na(silent$n_authors))

  # An author block carrying neither plain items nor name: keys is stated but
  # unread, and says so.
  odd <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/b.Rmd" = "---\ntitle: B\nauthor:\n  given: Ada\n  family: Lovelace\n---\n")))
  expect_equal(odd$author_stated, 1L)
  expect_true(is.na(odd$author))
})

test_that("a repeated name is listed once", {
  v <- metrics_vignettes(mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "vignettes/a.Rmd" = "---\ntitle: A\nauthor:\n  - name: Ada\n  - name: Ada\n---\n")))
  expect_equal(v$n_authors, 1L)
})

test_that("the count and the boolean come from the same rows", {
  # Computing them from two patterns is how the .qmd gap survived: one place was
  # fixed and the other was not.
  ctx <- mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n",
                 "vignettes/a.qmd" = "---\ntitle: A\n---\n",
                 "vignettes/b.Rnw" = "\\documentclass{article}\n"))
  m <- metrics_portability(ctx)
  expect_equal(m$n_vignettes, 2L)
  expect_true(m$has_vignettes)
  expect_equal(m$n_vignettes, nrow(metrics_vignettes(ctx)))

  none <- mk(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n"))
  expect_equal(metrics_portability(none)$n_vignettes, 0L)
  expect_false(metrics_portability(none)$has_vignettes)
})
