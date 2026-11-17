context("run helpers, .jasp file selection and test lookup")

test_that("a quiet run keeps the warnings of the option parse and hides the others", {
  # The option parse of jaspSyntax, as jaspBase::runWrappedAnalysis calls it
  loadQmlAndParseOptions <- function() warning("unknown option: `foo`")
  analysis <- function() {
    loadQmlAndParseOptions()
    warning("a warning of the analysis")
    cat("output of the analysis\n")
    "done"
  }

  seen <- character(0)
  result <- withCallingHandlers(
    runQuietly(analysis, list(), quiet = TRUE),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(result, "done")
  expect_identical(seen, "unknown option: `foo`")
})

test_that("whileParsingOptions recognises the option parse of jaspSyntax, also called with ::", {
  expect_true(whileParsingOptions(list(quote(f()), quote(jaspSyntax::loadQmlAndParseOptions(a, b)))))
  expect_true(whileParsingOptions(list(quote(loadQmlAndParseOptionsStatus(a)))))
  expect_false(whileParsingOptions(list(quote(f()), quote(jaspBase::runWrappedAnalysis(a)))))
  expect_false(whileParsingOptions(list(quote((function() 1)()))))
})

test_that("the QML files of a source module count for its reinstall", {
  modulePath <- tempfile("jaspToolsModule")
  dir.create(file.path(modulePath, "inst", "qml", "common"), recursive = TRUE)
  dir.create(file.path(modulePath, "R"))
  on.exit(unlink(modulePath, recursive = TRUE))
  for (file in c("DESCRIPTION", "NAMESPACE", "R/analysis.R", "inst/Description.qml", "inst/qml/Analysis.qml",
                 "inst/qml/common/Shared.qml", "inst/qml/notes.txt"))
    writeLines("x", file.path(modulePath, file))

  files <- sub(paste0(modulePath, "/"), "", moduleSourceFiles(modulePath), fixed = TRUE)
  expect_setequal(files, c("DESCRIPTION", "NAMESPACE", "R/analysis.R", "inst/Description.qml", "inst/qml/Analysis.qml",
                           "inst/qml/common/Shared.qml"))
})

test_that("an analysis of a .jasp file is selected by position or name, and NA is refused", {
  records <- list(list(name = "Anova"), list(name = NULL), list(name = "Ancova"))

  expect_identical(selectJaspFileAnalysis(records, 3)$name, "Ancova")
  expect_identical(selectJaspFileAnalysis(records, "Ancova")$name, "Ancova")
  expect_error(selectJaspFileAnalysis(records, NA_real_), "must be the name of an analysis")
  expect_error(selectJaspFileAnalysis(records, NA_character_), "must be the name of an analysis")
  expect_error(selectJaspFileAnalysis(records, c(1, 2)), "must be the name of an analysis")
  expect_error(selectJaspFileAnalysis(records, "Ttest"), "has no analysis `Ttest`")
  expect_error(selectJaspFileAnalysis(records, 4), "position from 1 to 3")
})

test_that("expect_equal_jasp_file fails when the file saved no table for the analysis", {
  local_mocked_bindings(
    readAnalysisResultsFromJaspFile = function(jaspFilePath) list(Anova = list(name = "Anova", status = "running", results = NULL)),
    .package = "jaspSyntax"
  )

  expect_failure(
    expect_equal_jasp_file(list(status = "fatalError", results = list(errorMessage = "boom")), "saved.jasp"),
    "nothing to compare"
  )
})

test_that("the mismatch warning of the generator shows braces instead of reading them as cli code", {
  expect_warning(
    warnJaspFileMismatches(2, "Anova", "file.jasp", c("Table `a` is missing", "error in function(x) { stop(x) }")),
    "function\\(x\\) \\{ stop\\(x\\) \\}"
  )
})

test_that("the generated example tests are found by the name of their analysis", {
  modulePath <- tempfile("jaspToolsModule")
  testsDir   <- file.path(modulePath, "tests", "testthat")
  dir.create(testsDir, recursive = TRUE)
  on.exit(unlink(modulePath, recursive = TRUE))

  writeLines(c('test_that("Descriptives results match", {',
               '  results <- jaspTools::runAnalysisFromJaspFile(jaspFile, 1)', '})'),
             file.path(testsDir, "test-library-Exam Scores.R"))
  writeLines(c('test_that("Descriptives (analysis 2) runs without error", {',
               '  results <- jaspTools::runAnalysisFromJaspFile(jaspFile, 2)', '})'),
             file.path(testsDir, "test-other-Big Five.R"))
  writeLines(c('test_that("DescriptivesExtra results match", {', '})'),
             file.path(testsDir, "test-other-Extra.R"))
  writeLines(c('results <- jaspTools::runAnalysis("Descriptives", "debug.csv", options)'),
             file.path(testsDir, "test-verified-Old.R"))

  expect_setequal(getTestFilesMatchingName("Descriptives", modulePath),
                  c("test-library-Exam Scores.R", "test-other-Big Five.R", "test-verified-Old.R"))
})
