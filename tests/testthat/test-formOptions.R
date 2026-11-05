context("Options of a form: only the options a test sets are given to the form")

formDefaults <- function() {
  options <- jaspTools:::formDefaultOptions(list(a = 1, b = TRUE, c = "x", d = list(e = 1, f = 2)))
  attr(options, "analysisName") <- "SomeAnalysis"
  options
}

test_that("assignments mark the options that are set", {
  options <- formDefaults()
  options$a <- 2
  options[["b"]] <- TRUE # set to its default value: still set
  options$d$e <- 5
  expect_setequal(attr(options, "setOptions"), c("a", "b", "d"))
  expect_identical(attr(options, "analysisName"), "SomeAnalysis")

  options <- utils::modifyList(options, list(c = "y"))
  expect_true(inherits(options, "jaspFormOptions"))
  expect_setequal(attr(options, "setOptions"), c("a", "b", "c", "d"))

  options <- formDefaults()
  options["c"] <- list("z")
  expect_identical(attr(options, "setOptions"), "c")
})

test_that("only the options that are set are given to the form", {
  options <- formDefaults()
  expect_identical(jaspTools:::formOptionsAsList(options), setNames(list(), character(0)))
  expect_identical(as.character(jsonlite::toJSON(jaspTools:::formOptionsAsList(options), auto_unbox = TRUE)), "{}")

  options$a <- 2
  options$d$e <- 5
  expect_identical(jaspTools:::formOptionsAsList(options), list(a = 2, d = list(e = 5, f = 2)))

  options$a <- NULL # removed: the form gives it its default
  expect_identical(jaspTools:::formOptionsAsList(options), list(d = list(e = 5, f = 2)))

  expect_identical(jaspTools:::formOptionsAsList(options, onlySet = FALSE), list(b = TRUE, c = "x", d = list(e = 5, f = 2)))
})

test_that("a plain list (a .jasp file, a json string, built by hand) is given as it is", {
  options <- list(a = 1, b = TRUE)
  expect_identical(jaspTools:::formOptionsAsList(options), options)
})
