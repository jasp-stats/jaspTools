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
    if (!codeCoverage)
      remotes::install_local(modulePath, upgrade = "never", force = FALSE, INSTALL_opts = "--no-multiarch")

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
#'
#' @param name String name of the analysis to test (case sensitive).
#' @param onlyPlots Would you like to only run the tests for plots? This can speed up the generating of reference images in case you are not interested in the other unit tests.
#' @examples
#'
#' testAnalysis("AnovaBayesian")
#'
#' @export testAnalysis
testAnalysis <- function(name, onlyPlots = FALSE) {
  modulePath <- getModulePathFromRFunction(name)
  filesToTest <- getTestFilesMatchingName(name, modulePath)

  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true") # this is to prevent vdiffr from skipping plots
  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
  })

  if (onlyPlots) {
    originalFn <- testthat::test_that
    on.exit(undoPlotTestingChanges(originalFn), add=TRUE)
    adjustTestthatForPlotTesting()
  }

  testthat:::test_files(test_paths = filesToTest, test_dir = file.path(modulePath, "tests", "testthat"), reporter = testthat::default_compact_reporter(), test_package = NULL)

  return(invisible())
}

 fixRNGForTesting <- function() {
  lifecycle::deprecate_stop(
    when = "1.5.2",
    what = "jaspTools::fixRNGForTesting()",
    details = "Set options(jaspLegacyRngKind = TRUE) or options(jaspLegacyRngKind = FALSE) to use the correct method."
  )
 }

#' Test all analyses in the currently monitored modules.
#'
#' Tests all R analyses found under modules/tests/testthat. Useful to perform before making
#' a pull request, to prevent failing builds.
#'
#' @param onlyPlots Would you like to only run the tests for plots? This can speed up the generating of reference images in case you are not interested in the other unit tests.
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
  })

  if (onlyPlots) {
    originalFn <- testthat::test_that
    on.exit(undoPlotTestingChanges(originalFn), add=TRUE)
    adjustTestthatForPlotTesting()
  }

  modulePaths <- getModulePathsForTesting()

  testResults <- list(failedModules = c(), passedModules = c())
  for (modulePath in modulePaths) {
    if (length(modulePaths) > 1)
      message("\nRunning tests from ", modulePath, "\n")

    testDir <- file.path(modulePath, "tests", "testthat")
    results <- try(as.data.frame(testthat::test_dir(testDir)))

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

#' Visually inspect failed test plots.
#'
#' This function is a wrapper around \code{testthat::snapshot_review()}. It allows
#' visual inspection of the plots in the unit tests that produced an error.
#' If no analysis is specified it will iterate over all test
#' cases.
#'
#'
#' @param name Optional string name of the analysis whose plots should be
#' tested.
#' @return A Shiny app that shows all failed cases. The app allows
#' test plots to be validated, at which point they are placed in the _snaps
#' directory and used as a reference for future tests.
#' @examples
#'
#' # manageTestPlots("Anova")
#'
#' @export manageTestPlots
manageTestPlots <- function(name = NULL) {
  if (is.null(name))
    manageAllTestPlots()
  else
    manageTestPlotsFile(name)
}

manageAllTestPlots <- function() {
  modulePaths <- getModulePathsForTesting()
  for (modulePath in modulePaths) {
    if (length(modulePaths) > 1)
      message("\nTesting plots from ", modulePath, "\n")

    .manageTestPlots(modulePath)
  }
}

manageTestPlotsFile <- function(name) {
  modulePath <- getModulePathFromRFunction(name)
  testFiles <- getTestFilesMatchingName(name, modulePath)
  .manageTestPlots(modulePath, testFiles)
}

.manageTestPlots <- function(modulePath, testFiles = NULL) {
  testthat::snapshot_review(files = testFiles, path = file.path(modulePath, "tests", "testthat"))
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

approxMatch <- function(new, old, tol = 1e-5) {

  idxNumNew <- sapply(new, is.numeric)
  idxNumOld <- sapply(old, is.numeric)
  idxCharNew <- sapply(new, is.character)
  idxCharOld <- sapply(old, is.character)

  numNew <- unlist(new[idxNumNew])
  numOld <- unlist(old[idxNumOld])
  charNew <- unlist(new[idxCharNew])
  charOld <- unlist(old[idxCharOld])

  idxCharMis <- !charOld %in% charNew
  idxMissingChars <- which(idxCharOld)[idxCharMis]

  idxFoundNums <- NULL
  idxMissNums <- NULL
  for (i in seq_along(numOld)) {
    v <- abs(numOld[i] - numNew)
    idx <- which.min(v)
    if (isTRUE(v[idx] < tol)) {
      idxFoundNums <- rbind(idxFoundNums, c(i, idx))
    } else {
      idxMissNums <- rbind(idxMissNums, c(i, idx))
    }
  }
  idxNumNewMiss <- which(idxNumNew)[idxMissNums[, 2]]
  idxNumOldMiss <- which(idxNumOld)[idxMissNums[, 1]]

  if (length(idxMissingChars) > 0) {
    cat("Missing character(s) in old\n")
    print(old[idxMissingChars])
  }
  if (length(idxMissNums) > 0) {
    cat("Could not match the following value(s) from old in new.\n")
    for (i in 1:nrow(idxMissNums)) {
      cat(sprintf("value: %.6f at index %d\n",
                  old[[idxNumOldMiss[i]]], idxNumOldMiss[i]))
      cat("Closest match:\n")
      cat(sprintf("value: %.6f at index %d\n",
                  new[[idxNumNewMiss[i]]], idxNumNewMiss[i]))
    }
  }
  if (length(idxMissingChars) > 0 || length(idxMissNums) > 0) {
    cat("Some elements of old were not found in.\nPlease compare the results carefully!")
  } else {
    cat("All elements of old appear in new.")
  }

  return(invisible(list(
    idxMissingChars = idxMissingChars,
    idxNumNewMiss = idxNumNewMiss,
    idxNumOldMiss = idxNumOldMiss
  )))

}

getTestFilesMatchingName <- function(name, modulePath) {
  testsDir <- file.path(modulePath, "tests", "testthat")
  if (!dir.exists(testsDir))
    stop("Could not locate ", testsDir)

  testFiles <- list.files(testsDir)
  if (length(testFiles) == 0)
    stop("No files found to test.")

  analysisNames <- gsub("^test-(verified-)?", "", testFiles)
  analysisNames <- gsub("\\.[rR]$", "", analysisNames)

  matches <- which(tolower(basename(analysisNames)) == tolower(name))
  if (length(matches) == 0)
    stop("Could not locate test-", name, ".R, found the following testfiles: ", paste(basename(testFiles), collapse =  ", "))

  return(testFiles[matches])
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

adjustTestthatForPlotTesting <- function() {

  # taken from testthat::test_that
  test_thatStandIn <- function (desc, code) {
    if (!is.character(desc) || length(desc) != 1) {
      rlang::abort("`desc` must be a string")
    }
    reporter <- testthat::get_reporter()
    if (is.null(reporter)) {
      reporter <- testthat:::local_interactive_reporter()
    }
    testthat::local_test_context()
    code <- substitute(code)
    if (testthat::edition_get() >= 3) {
      if (!rlang::is_call(code, "{")) {
        rlang::warn("The `code` argument to `test_that()` must be a braced expression to get accurate file-line information for failures.")
      }
    }

    # and added these lines
    envirPlotTest <- Sys.getenv("JASP_PLOT_TEST")
    if (envirPlotTest == "true" && !any(grepl("expect_equal_plots", code, fixed=TRUE)))
      return()
    # until here

    testthat:::test_code(desc, code, env = parent.frame(), reporter = reporter)
  }

  Sys.setenv("JASP_PLOT_TEST" = "true")
  replaceFn("test_that", test_thatStandIn, "testthat")
}

undoPlotTestingChanges <- function(originalFn) {
  Sys.unsetenv("JASP_PLOT_TEST")
  replaceFn("test_that", originalFn, "testthat")
}
