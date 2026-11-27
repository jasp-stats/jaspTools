#' Run tests on Travis.
#'
#' Runs all tests that are in the test directory of the module. Should not be used in RStudio.
#'
#' @param modulePath The path to the module on Travis. Should be obtained from testthat.R.
#'
#' @export runTestsTravis
runTestsTravis <- function(modulePath) {
  if (Sys.getenv("CI") == "" && Sys.getenv("R_COVR") == "") {
    testAll()
  } else {
    if (!.isSetupComplete())
      stop("The setup should be completed before the tests are ran")

    setPkgOption("module.dirs", modulePath)

    # this check is identical to covr::in_covr()
    codeCoverage <- identical(Sys.getenv("R_COVR"), "true")

    # only install the pkg if we're not running on covr, because covr also installs it
    #if (!codeCoverage)
    #  remotes::install_local(modulePath, upgrade = "never", force = FALSE, INSTALL_opts = "--no-multiarch")

    testingStatus <- testAll()

    if (!interactive())
      quit(save = "no", status = testingStatus)
  }
}

#' Test a specific JASP analysis.
#'
#' Tests a specific R analysis found under module/tests/testthat. Useful to perform before
#' making a pull request, to prevent failing builds.
#'
#' In addition to the standard test file (e.g., \code{test-AnalysisName.R}), this function
#' also discovers auto-generated source-based test files (\code{test-library-*.R},
#' \code{test-verified-*.R}, \code{test-other-*.R}) that contain
#' \code{runAnalysis("AnalysisName", ...)} calls. This ensures that tests
#' created by \code{\link{makeTestsFromExamples}} are included when testing an analysis.
#'
#' @param name String name of the analysis to test (case sensitive).
#' @param onlyPlots Run only test blocks containing \code{expect_equal_plots()}.
#'   In mixed blocks, run the setup and plot checks and skip JASP table and HTML comparisons.
#' @param includeAll Logical (default \code{TRUE}). When \code{TRUE}, also
#'   includes auto-generated example test files (\code{test-library-*.R},
#'   \code{test-verified-*.R}, \code{test-other-*.R}) that contain
#'   \code{runAnalysis("AnalysisName", ...)} calls. Set \code{includeAll = FALSE} to
#'   restrict testing to the standard \code{test-AnalysisName.R} file only.
#' @examples
#'
#' testAnalysis("AnovaBayesian")
#'
#' @export testAnalysis
testAnalysis <- function(name, onlyPlots = FALSE, includeAll = TRUE) {
  preserveSnapshotReferences(environment())
  modulePath <- getModulePathFromRFunction(name)
  filesToTest <- getTestFilesMatchingName(name, modulePath, includeAll = includeAll)

  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true") # this is to prevent vdiffr from skipping plots
  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
  }, add = TRUE)

  if (onlyPlots)
    localPlotTests()

  testthat:::test_files(test_paths = filesToTest, test_dir = file.path(modulePath, "tests", "testthat"), reporter = testthat::default_compact_reporter(), test_package = NULL)

  return(invisible())
}

#' Test all analyses in the currently monitored modules.
#'
#' Tests all R analyses found under modules/tests/testthat. Useful to perform before making
#' a pull request, to prevent failing builds.
#'
#' @inheritParams testAnalysis
#'
#' @export testAll
testAll <- function(onlyPlots = FALSE) {
  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true")

  optsValue <- getOption("testthat.progress.max_fails")
  options("testthat.progress.max_fails" = 1000)

  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
    options("testthat.progress.max_fails" = optsValue)
  }, add = TRUE)

  if (onlyPlots)
    localPlotTests()

  modulePaths <- getModulePathsForTesting()

  testResults <- list(failedModules = c(), passedModules = c())
  for (modulePath in modulePaths) {
    if (length(modulePaths) > 1)
      message("\nRunning tests from ", modulePath, "\n")

    testDir <- file.path(modulePath, "tests", "testthat")
    results <- try(as.data.frame(runModuleTestDir(testDir)))

    if (inherits(results, "try-error") || (sum(results$failed) > 0 || sum(results$error) > 0))
      testResults[["failedModules"]] <- c(testResults[["failedModules"]], basename(modulePath))
    else
      testResults[["passedModules"]] <- c(testResults[["passedModules"]], basename(modulePath))

  }

  if (length(modulePaths) > 1)
    printSuccessFailureModules(testResults)

  status <- 0
  if (length(testResults[["failedModules"]]) > 0)
    status <- 1

  return(invisible(status))
}

#' Review pending JASP figure snapshots
#'
#' Uses the [reviewSnapshots()] viewer for new and changed figures produced by
#' [expect_equal_plots()]. Export and apply decisions with [applySnapshotReview()]
#' to accept an SVG together with its reviewed ggplot structure.
#'
#' @param name Optional analysis name. By default, review all configured modules.
#' @param open Open the reports in the default browser.
#' @return Invisibly, a named list of report paths for modules with pending figures.
#' @examples
#'
#' # manageTestPlots("Anova")
#'
#' @export manageTestPlots
manageTestPlots <- function(name = NULL, open = interactive()) {
  files <- NULL
  if (is.null(name)) {
    modulePaths <- getModulePathsForTesting()
  } else {
    modulePaths <- getModulePathFromRFunction(name)
    files <- getTestFilesMatchingName(name, modulePaths)
  }
  reports <- list()
  for (modulePath in modulePaths) {
    ids <- snapshotReviewIds(modulePath, files = files)
    figures <- ids[endsWith(ids, ".svg")]
    if (length(figures))
      reports[[basename(modulePath)]] <- writeSnapshotReview(modulePath, figures, output = NULL, open = open)
  }
  if (!length(reports))
    message("No pending JASP figures to review.")
  invisible(reports)
}

#' Aids in the creation of tests for tables.
#'
#' This function is designed to make it easier to create unit tests for tables.
#' It strips off attributes and flattens the structure until a list remains
#' with dimension 1. Output is then produced which can be immediately placed in
#' the test file.
#'
#'
#' @param rows A list with lists of rows (i.e., a JASP table).
#' @param print Should the result be printed.
#' @return Copy-paste ready output which may serve as the reference to test
#' tables against.
#' @examples
#'
#' options <- analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#' results <- runAnalysis("BinomialTest", "debug", options, view=FALSE)
#' makeTestTable(results[["results"]][["binomial"]][["data"]])
#'
#' @export makeTestTable
makeTestTable <- function(rows, print=TRUE) {
  x <- collapseTestTable(rows)
  result <- ""
  nChars <- 0
  for (i in 1:length(x)) {
    element <- x[[i]]
    if (! is.numeric(element)) {
      element <- paste0("\"", element, "\"")
    }

    nOldChars <- nchar(result)
    if (nchar(result) == 0) {
      result <- element
    } else {
      result <- paste(result, element, sep=", ")
    }

    nChars <- nChars + nchar(result) - nOldChars
    if (nChars >= 60) {
      result <- paste0(result, "\n\t")
      nChars <- 0
    }
  }

  result <- gsub("\n\t,", ",\n\t", result, fixed=TRUE)
  if (endsWith(result, "\n")) {
    result <- substr(result, 1, nchar(result)-1)
  }
  result <- paste0("list(", result, ")")

  if (print)
    cat(result)

  invisible(result)
}

getTestFilesMatchingName <- function(name, modulePath, includeAll = TRUE) {
  testsDir <- file.path(modulePath, "tests", "testthat")
  if (!dir.exists(testsDir))
    stop("Could not locate ", testsDir)

  testFiles <- list.files(testsDir)
  if (length(testFiles) == 0)
    stop("No files found to test.")

  # Pass 1: match by file name (e.g., test-AnalysisName.R)
  analysisNames <- gsub("^test-(verified-|library-|other-)?", "", testFiles)
  analysisNames <- gsub("\\.[rR]$", "", analysisNames)
  matches <- which(tolower(basename(analysisNames)) == tolower(name))
  matchedFiles <- testFiles[matches]

  # Pass 2: scan test-{library,verified,other}-* files for runAnalysis("name", ...) calls, or for the test_that()
  # titles of the generated tests, which run the R syntax of the analysis ("name results match",
  # "name (analysis 2) runs without error")
  if (includeAll) {
    sourceFiles <- testFiles[grepl("^test-(library|verified|other)-.*\\.[rR]$", testFiles)]
    sourceFiles <- setdiff(sourceFiles, matchedFiles)
    escapedName <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", name)
    pattern <- paste0('runAnalysis\\(\\s*["\']', escapedName, '["\']|',
                      'test_that\\(\\s*["\']', escapedName, '( \\(analysis [0-9]+\\))? (results match|runs without error)["\']')
    for (ef in sourceFiles) {
      content <- readLines(file.path(testsDir, ef), warn = FALSE)
      if (any(grepl(pattern, content)))
        matchedFiles <- c(matchedFiles, ef)
    }
  }

  if (length(matchedFiles) == 0) {
    hint <- if (!includeAll) " (example test files were excluded because includeAll = FALSE)" else ""
    stop("Could not locate test file for ", name, hint, ". Found the following test files: ", paste(basename(testFiles), collapse = ", "))
  }

  return(matchedFiles)
}

printSuccessFailureModules <- function(testResults) {
  if (length(testResults[["passedModules"]]) > 0)
    message("\nThe tests passed for ", paste(testResults[["passedModules"]], collapse = ", "), ".")

  if (length(testResults[["failedModules"]]) > 0)
    message("\nThe tests failed for ", paste(testResults[["failedModules"]], collapse = ", "), ". Scroll up for more details.")
}

charVec2MixedList <- function(x) {
  x <- stringi::stri_escape_unicode(x)
  x <- gsub("\\\\u.{4}", "<unicode>", x)
  x <- stringi::stri_unescape_unicode(x)
  lapply(x, function(element) {
    res <- element
    if (is.character(element)) {
      num <- suppressWarnings(as.numeric(element))
      if (! is.na(num)) {
        res <- num
      }
    }
    return(res)
  })
}

collapseTestTable <- function(rows) {
  if (! is.list(rows) || length(rows) == 0)
    stop("expecting input to be a list (with a list for each JASP table row)")

  x <- unname(unlist(rows))
  x <- charVec2MixedList(x)

  return(x)
}

runModuleTestDir <- function(testDir, ...) {
  preserveSnapshotReferences(environment())
  # testthat writes inline Markdown in its workers. Keep those comparisons
  # in the protected process when a module has accepted Markdown references.
  markdown <- list.files(file.path(testDir, "_snaps"), pattern = "[.]md$", recursive = TRUE)
  if (any(!endsWith(markdown, ".new.md")))
    withr::local_envvar(c(TESTTHAT_PARALLEL = "false"))
  testthat::test_dir(testDir, ...)
}

localPlotTests <- function(envir = parent.frame()) {
  # test_dir attaches testthat. Attach before rebinding so both qualified and
  # unqualified calls see the same functions and are restored after the run.
  for (package in c("testthat", "jaspTools"))
    if (!paste0("package:", package) %in% search())
      withr::local_package(package, .local_envir = envir)
  withr::local_envvar(c(JASP_PLOT_TEST = "true", TESTTHAT_PARALLEL = "false"), .local_envir = envir)

  testThat <- testthat::test_that
  plotTest <- function(desc, code) {
    if (!"expect_equal_plots" %in% all.names(substitute(code)))
      return(invisible(NULL))
    call <- match.call()
    call[[1L]] <- testThat
    eval(call, parent.frame())
  }

  omit <- function(...) invisible(NULL)
  # Plot files are written during the expectation. A partial run must neither
  # rewrite inline Markdown snapshots nor resolve pending Markdown candidates.
  snapshotReporter <- utils::getFromNamespace("SnapshotReporter", "testthat")
  plotReporter <- R6::R6Class(inherit = snapshotReporter,
                              public = list(end_file = omit))
  testthat::local_mocked_bindings(test_that = plotTest, SnapshotReporter = plotReporter,
                                  .package = "testthat", .env = envir)
  testthat::local_mocked_bindings(expect_snapshot_table = omit, expect_snapshot_html = omit,
                                  expect_equal_tables = omit, .package = "jaspTools", .env = envir)
}
