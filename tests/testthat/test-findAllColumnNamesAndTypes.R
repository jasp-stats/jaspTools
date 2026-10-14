context("findAllColumnNamesAndTypes")

test_that("types are found when the variable option is not the first option", {

  options <- list(
    plotWidth       = 480,
    variables       = c("a", "b"),
    variables.types = c("scale", "ordinal")
  )

  result <- jaspTools:::findAllColumnNamesAndTypes(options, c("a", "b", "c"))

  expect_equal(result[["variables"]], c("a", "b"))
  expect_equal(result[["types"]],     c("scale", "ordinal"))
})

test_that("types stay aligned with variables when several options hold variables", {

  options <- list(
    bfType            = "fractional",
    covariates        = "c",
    covariates.types  = "nominal",
    plotWidth         = 480,
    variables         = c("a", "b"),
    variables.types   = c("scale", "ordinal")
  )

  result <- jaspTools:::findAllColumnNamesAndTypes(options, c("a", "b", "c"))

  expect_equal(result[["variables"]], c("c", "a", "b"))
  expect_equal(result[["types"]],     c("nominal", "scale", "ordinal"))
})

test_that("variables without a types entry get one NA per variable", {

  options <- list(
    seed      = 1,
    variables = c("a", "b", "c"),
    other     = "c",
    other.types = "nominal"
  )

  result <- jaspTools:::findAllColumnNamesAndTypes(options, c("a", "b", "c"))

  expect_equal(result[["variables"]], c("a", "b", "c", "c"))
  expect_equal(result[["types"]],     c(NA_character_, NA_character_, NA_character_, "nominal"))
})

test_that("types are found in nested options", {

  options <- list(
    seed  = 1,
    terms = list(
      list(label = "x", component = "a", component.types = "scale"),
      list(label = "y", component = "b", component.types = "nominal")
    )
  )

  result <- jaspTools:::findAllColumnNamesAndTypes(options, c("a", "b"))

  expect_equal(result[["variables"]], c("a", "b"))
  expect_equal(result[["types"]],     c("scale", "nominal"))
})
