context("analysisOptions")

test_that("analysisOptions flattens simple types/value structures", {
  # Test case: split option with empty types and simple value
  # Raw structure: $split$types = list(), $split$value = ""
  # Expected: $split = "", $split.types = list()

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]


  # The split option should be flattened to just the value

  expect_true(is.character(opts$split) || is.null(opts$split$value))

  # If flattened correctly, split should be a character, not a list with types/value


  if (is.list(opts$split) && !is.null(opts$split$value)) {
    fail("split option was not flattened - still has $value structure")
  }

  # The .types should be stored separately
  expect_true("split.types" %in% names(opts) || identical(opts$split, ""))
})

test_that("analysisOptions flattens complex types/value structures with optionKey",
{
  # Test case: effectSizeModelTerms with optionKey and nested lists
  # Raw structure:
  #   $effectSizeModelTerms$optionKey = "components"

  #   $effectSizeModelTerms$types = c("scale", "nominal")
  #   $effectSizeModelTerms$value = list(list(components="ablat"), list(components="alloc"))
  # Expected:
  #   $effectSizeModelTerms = list(list(components="ablat"), list(components="alloc"))
  #   $effectSizeModelTerms.types = list(list(components="scale"), list(components="nominal"))

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]

  # The effectSizeModelTerms should be flattened to just the value (a list)
  expect_true(is.list(opts$effectSizeModelTerms))

  # It should NOT have the optionKey, types, value structure anymore
  expect_false("optionKey" %in% names(opts$effectSizeModelTerms))
  expect_false("types" %in% names(opts$effectSizeModelTerms))
  expect_false("value" %in% names(opts$effectSizeModelTerms))

  # The first element should have "components" with value "ablat"
  expect_equal(opts$effectSizeModelTerms[[1]]$components, "ablat")
  expect_equal(opts$effectSizeModelTerms[[2]]$components, "alloc")

  # The .types should be stored separately with parallel structure
  expect_true("effectSizeModelTerms.types" %in% names(opts))
  expect_equal(opts$`effectSizeModelTerms.types`[[1]]$components, "scale")
  expect_equal(opts$`effectSizeModelTerms.types`[[2]]$components, "nominal")
})

test_that("analysisOptions flattens deeply nested types/value structures", {
  # Test case: variables[[1]]$coefficientAlpha with nested types/value
  # Raw structure:
  #   $variables[[1]]$coefficientAlpha$types = list()
  #   $variables[[1]]$coefficientAlpha$value = ""
  # Expected:
  #   $variables[[1]]$coefficientAlpha = ""
  #   $variables.types[[1]]$coefficientAlpha = list()

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]

  # The coefficientAlpha inside variables[[1]] should be flattened to just the value
  expect_true(is.character(opts$variables[[1]]$coefficientAlpha))
  expect_equal(opts$variables[[1]]$coefficientAlpha, "")

  # It should NOT have the types/value structure anymore
  expect_false(is.list(opts$variables[[1]]$coefficientAlpha))

  # The .types should be stored separately with parallel structure
  expect_true("variables.types" %in% names(opts))
  expect_true(is.list(opts$`variables.types`[[1]]$coefficientAlpha))
  expect_equal(length(opts$`variables.types`[[1]]$coefficientAlpha), 0)
})
