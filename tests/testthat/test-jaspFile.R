context("runAnalysisFromJaspFile and expect_equal_jasp_file")

jaspFixture <- function(name) {
  file.path(testthat::test_path(), "..", "JASPFiles", name)
}

table <- function(rows) {
  list(schema = list(fields = list()), data = rows)
}

test_that("selectJaspFileAnalysis takes a position or a name", {
  records <- list(list(name = "First"), list(name = "Second"), list(name = "Second"))

  expect_identical(jaspTools:::selectJaspFileAnalysis(records, 2), records[[2]])
  expect_identical(jaspTools:::selectJaspFileAnalysis(records, 1L), records[[1]])
  expect_identical(jaspTools:::selectJaspFileAnalysis(records, "Second"), records[[2]])

  expect_error(jaspTools:::selectJaspFileAnalysis(records, "Third"), "no analysis `Third`")
  expect_error(jaspTools:::selectJaspFileAnalysis(records, 4), "position from 1 to 3")
  expect_error(jaspTools:::selectJaspFileAnalysis(records, 1.5), "position from 1 to 3")
})

test_that("resultsTables finds the tables of a results tree with their place", {
  results <- list(
    title = "Analysis",
    stats = table(list(list(a = 1))),
    container = list(collection = list(
      inner = table(list()),
      plot  = list(data = "resources/0/plot.png", width = 320, height = 320)
    ))
  )

  tables <- jaspTools:::resultsTables(results)

  expect_named(tables, c("stats", "container/collection/inner"))
  expect_identical(tables$stats$data, list(list(a = 1)))
})

test_that("jaspFileMismatches compares the tables saved in the file", {
  stored <- list(status = "complete", results = list(
    stats   = table(list(list(Variable = "x", mean = 0.123456), list(Variable = "y", mean = 2))),
    empty   = table(list()),
    removed = table(list(list(value = 1)))
  ))
  results <- list(status = "complete", results = list(
    stats   = table(list(list(Variable = "x", mean = 0.1234561), list(Variable = "y", mean = 2))),
    empty   = table(list()),
    extra   = table(list(list(value = 1)))
  ))

  mismatches <- jaspTools:::jaspFileMismatches(results, stored)
  expect_length(mismatches, 1L)
  expect_match(mismatches, "Table `removed` is missing", fixed = TRUE)

  results$results$stats$data[[2]]$mean <- 3
  results$results$removed <- table(list(list(value = 1)))
  mismatches <- jaspTools:::jaspFileMismatches(results, stored)
  expect_length(mismatches, 1L)
  expect_match(mismatches, "Table `stats` is not equal to old table", fixed = TRUE)

  results$results$stats$data[[2]] <- NULL
  mismatches <- jaspTools:::jaspFileMismatches(results, stored)
  expect_match(mismatches, "are not of equal length", fixed = TRUE)
})

test_that("jaspFileMismatches reports an analysis that no longer runs", {
  stored  <- list(status = "complete", results = list())
  results <- list(status = "fatalError", results = list(errorMessage = "Something broke"))

  expect_identical(
    jaspTools:::jaspFileMismatches(results, stored),
    "The analysis ran with status `fatalError` instead of `complete`: Something broke"
  )

  stored$status <- "validationError"
  expect_length(jaspTools:::jaspFileMismatches(results, stored), 0L)
})

test_that("expect_equal_tables still reports what differs", {
  expect_success(expect_equal_tables(list(list(a = 1, b = "x")), list(1, "x")))
  expect_failure(expect_equal_tables(list(list(a = 1, b = "x")), list(2, "x")), "is not equal to old table")
  expect_failure(expect_equal_tables(list(list(a = 1, b = "x")), list(1)), "are not of equal length")
})

test_that("runAnalysisFromJaspFile gives the results JASP saved in the file", {
  skip_on_cran()
  skip_if_not(jaspTools:::.isSetupComplete(), "jaspTools is not configured")
  skip_if_not_installed("jaspDescriptives")

  jaspFile <- jaspFixture("debug-descriptives.jasp")
  set.seed(1)
  results <- runAnalysisFromJaspFile(jaspFile, "Descriptives")

  expect_identical(results$status, "complete")
  expect_true("stats" %in% names(results$results))
  expect_equal_jasp_file(results, jaspFile, 1)

  results$results$stats$data[[1]][[2]] <- -1
  expect_failure(expect_equal_jasp_file(results, jaspFile, 1), "Table `stats` is not equal to old table")
})
