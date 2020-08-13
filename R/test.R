#' Run tests on Travis.
#'
#' Runs all tests that are in the test directory of the module. Should not be used in RStudio.
#'
#' @param modulePath The path to the module on Travis. Should be obtained from testthat.R.
#'
#' @export runTestsTravis
runTestsTravis <- function(modulePath) {
  if (Sys.getenv("CI") == "")
    stop("This function is meant to be run on Travis. If you want to run tests use `testAnalysis()` or `testAll()`")

  if (Sys.getenv("REQUIRED_PKGS") == "")
    stop("Could not find environment variable `REQUIRED_PKGS`")

  initJaspTools(TRUE, normalizePath(Sys.getenv("REQUIRED_PKGS")))

  setPkgOption("module.dirs", modulePath)

  options("testthat.progress.max_fails" = 1E3L)

  result <- testthat::test_dir("tests/testthat")
  result <- as.data.frame(result)

  if (sum(result$failed) > 0 || sum(result$error) > 0)
    quit(save = "no", status = 1)
}

#' Test a specific JASP analysis.
#'
#' Tests a specific R analysis found under module/tests/testthat. Useful to perform before
#' making a pull request, to prevent failing builds.
#'
#'
#' @param name String name of the analysis to test.
#' @examples
#'
#' testAnalysis("AnovaBayesian")
#'
#' @export testAnalysis
testAnalysis <- function(name) {
  modulePath <- getModulePathsForTesting(name)

  testDir    <- file.path(modulePath, "tests", "testthat")
  name       <- matchCaseTestFileAnalysisName(name, testDir)
  fileToTest <- file.path(testDir, paste0("test-", name, ".R"))

  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true") # this is to prevent vdiffr from skipping plots
  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
  })

  fixRNGForTesting()
  results <- testthat::test_file(fileToTest)

  printTipsNewPlots(hasNewPlots(results), name)
}


adjustTestthatForPlotTesting <- function() {

  test_thatStandIn <- function(desc, code) {
      code <- substitute(code)

      envirPlotTest <- Sys.getenv("JASP_PLOT_TEST")
      if (envirPlotTest == "true" && !any(grepl("expect_equal_plots", code, fixed=TRUE)))
          return()

      testthat:::test_code(desc, code, env = parent.frame())
  }

  Sys.setenv("JASP_PLOT_TEST" = "true")
  replaceFn("test_that", test_thatStandIn, "testthat")
}


undoPlotTestingChanges <- function(originalFn) {
  Sys.unsetenv("JASP_PLOT_TEST")
  replaceFn("test_that", originalFn, "testthat")
}


 fixRNGForTesting <- function() {
   suppressWarnings(RNGkind(sample.kind = "Rounding"))
 }

 getModulePathsForTesting <- function(name = NULL) {
   if (!is.null(name))
     modulePaths <- getModulePathFromRFunction(name)
   else
     modulePaths <- getModulePaths()

   modulesWithTests <- NULL
   for (modulePath in modulePaths) {
     testDir <- file.path(modulePath, "tests", "testthat")
     if (dir.exists(testDir) && length(list.files(testDir)) > 0)
       modulesWithTests <- c(modulesWithTests, modulePath)
   }

   if (length(modulesWithTests) == 0)
     stop("None of the module folders specified in `module.dirs` (see `viewPkgOptions()`) have any tests. Note that the tests should be in `moduleDir/tests/testthat` and named `test-analysisName.R`.")

   return(modulesWithTests)
 }


#' Test all analyses in the currently monitored modules.
#'
#' Tests all R analyses found under modules/tests/testthat. Useful to perform before making
#' a pull request, to prevent failing builds.
#'
#'
#' @export testAll
testAll <- function() {
  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true")

  optsValue <- getOption("testthat.progress.max_fails")
  options("testthat.progress.max_fails" = 1000)

  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
    options("testthat.progress.max_fails" = optsValue)
  })

  modulePaths <- getModulePathsForTesting()
  fixRNGForTesting()

  hasNewPlots <- FALSE
  testResults <- list(failedModules = c(), passedModules = c())
  for (modulePath in modulePaths) {
    if (length(modulePaths) > 1)
      message("\nRunning tests from ", modulePath, "\n")

    testDir <- file.path(modulePath, "tests", "testthat")
    results <- as.data.frame(testthat::test_dir(testDir))

    if (length(modulePaths) > 1) {
      if (sum(results$failed) > 0 || sum(results$error) > 0)
        testResults[["failedModules"]] <- c(testResults[["failedModules"]], basename(modulePath))
      else
        testResults[["passedModules"]] <- c(testResults[["passedModules"]], basename(modulePath))
    }

    if (hasNewPlots(results))
      hasNewPlots <- TRUE
  }

  printSuccessFailureModules(testResults)
  printTipsNewPlots(hasNewPlots)
}


#' Visually inspect new/failed test plots.
#'
#' This function is a wrapper around \code{vdiffr::manage_cases()}. It allows
#' visual inspection of the plots in the unit tests that were newly added or
#' produced an error. If no analysis is specified it will iterate over all test
#' cases.
#'
#'
#' @param name Optional string name of the analysis whose plots should be
#' tested.
#' @return A Shiny app that shows all new/failed/orphaned cases. The app allows
#' test plots to be validated, at which point they are placed in the figs
#' folder and used as a reference for future tests.
#' @examples
#'
#' # manageTestPlots("Anova")
#'
#' @export manageTestPlots
manageTestPlots <- function(name = NULL) {
  modulePaths <- getModulePathsForTesting(name)

  if (!is.null(name)) {
    name <- matchCaseTestFileAnalysisName(name, file.path(modulePaths, "tests", "testthat"))
    name <- paste0("^", name, "$")
  }

  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true")

  optsValue <- getOption("testthat.progress.max_fails")
  options("testthat.progress.max_fails" = 1000)

  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
    options("testthat.progress.max_fails" = optsValue)
  })

  originalFn <- testthat::test_that
  on.exit(undoPlotTestingChanges(originalFn), add=TRUE)
  adjustTestthatForPlotTesting()

  fixRNGForTesting()
  for (modulePath in modulePaths) {
    if (length(modulePaths) > 1)
      message("\nTesting plots from ", modulePath, "\n")

    versionMismatches <- checkDepVersionMismatches(modulePath)
    if (length(versionMismatches[["newer"]]) > 0 || length(versionMismatches[["older"]]) > 0)
      handleVersionMismatches(versionMismatches, name)

    vdiffr::manage_cases(modulePath, filter = name)
  }
}

matchCaseTestFileAnalysisName <- function(name, testsDir) {
  if (!is.null(name) && length(testsDir) ==  1 && is.character(testsDir)) {
    testFiles <- list.files(testsDir)
    if (length(testFiles) == 0)
      stop("No files found to test.")

    analysesToTest <- sub("^test-?", "", testFiles)
    analysesToTest <- sub("\\.[rR]$", "", analysesToTest)
    fileIndex <- which(tolower(basename(analysesToTest)) == tolower(name))
    if (length(fileIndex) == 0)
      stop("Could not locate test-", name, ".R, found the following testfiles: ", paste(basename(testFiles), collapse =  ", "))

    name <- analysesToTest[fileIndex]
  }

  return(name)
}


#' Allows users to add package dependencies to unit testing, specifically to plot testing
#'
#' Testing might fail if dependencies are not the same across platforms (e.g., different versions of jaspGraphs).
#' If this is the case then those dependencies should be monitored and errors given when they do not match.
#' This function allows you to define "unit test breaking" dependencies for plots
#'
#'
#' @param dep A single character value of a package name currently installed on your system
#' @return This function only has a side effect: updating figs/jasp-deps.txt
#' @examples
#'
#' addTestDependency("jaspGraphs")
#'
#' @export addTestDependency
addTestDependency <- function(dep) {
  if (!is.character(dep) || length(dep) > 1)
    stop("Expecting single name of a package")

  depsInFile <- getDepsFromFile()
  if (dep %in% names(depsInFile))
    stop("Package already exists in dependency file")

  if (!dep %in% installed.packages())
    stop("The package is not installed on your system, cannot retrieve a version")

  depToWrite <- list()
  depToWrite[[dep]] <- packageVersion(dep)
  writeDepsToFile(depToWrite)
  print(paste0("Dependency `", dep, "` added to jasp-deps.txt"))
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


checkDepVersionMismatches <- function(modulePath) {
  depsInFile <- getDepsFromFile(modulePath)
  userDeps <- getDepsFromUser(modulePath)

  depMismatches <- list(older=list(), newer=list())
  for (i in seq_along(userDeps)) {
    pkgName <- names(userDeps)[i]
    pkgVersion <- userDeps[[i]]

    if (pkgVersion == depsInFile[[pkgName]])
      next

    type <- "older"
    if (pkgVersion > depsInFile[[pkgName]])
      type <- "newer"

    misMatch <- list(pkg=pkgName, userVersion=pkgVersion, fileVersion=depsInFile[[pkgName]])
    depMismatches[[type]] <- c(depMismatches[[type]], list(misMatch))
  }

  return(depMismatches)
}


handleVersionMismatches <- function(versionMismatches, name) {
  onlyNewer <- length(versionMismatches[["newer"]]) > 0 && length(versionMismatches[["older"]]) == 0
  if (onlyNewer) {
    if (is.null(name)) {
      print("The installed packages on your system are newer than the ones used to create the library of test plots. Automatically updated the dependencies file; please make sure there are no plot mismatches in the Shiny app")
      writeUpdatedDeps(versionMismatches[["newer"]])
    } else {
      stop("The library of test plots was created using older packages; to avoid version mismatches between plots from different analyses please validate ALL plots by running `manageTestPlots()`")
    }
  } else {
    stop("Some of your installed packages are outdated (the library of test plots was created with a newer version). Please update these packages:\n", makeOutdatedDepsMsg(versionMismatches[["older"]]))
  }
}


makeOutdatedDepsMsg <- function(oldDeps) {
  msg <- NULL
  for (oldDep in oldDeps)
    msg <- c(msg, paste0("- ", oldDep[["pkg"]], " (version ", oldDep[["userVersion"]], ") is older than the version used to create other plots (", oldDep[["fileVersion"]], ").\n",
                         "* The ", oldDep[["pkg"]], " package is located at ", getInstallLocationDep(oldDep[["pkg"]])))
  return(paste(msg, collapse="\n\n"))
}


getDepsFromFile <- function(modulePath) {
  pathToDeps <- getDepsFileLocation(modulePath)
  if (!file.exists(pathToDeps)) {
    message("File with JASP dependencies does not exist yet. Creating ", pathToDeps)
    writeDepsToFile(list(jaspGraphs = packageVersion("jaspGraphs")), modulePath, add = FALSE)
  }

  depLines <- readLines(pathToDeps, warn=FALSE)
  pattern <- "^- ([a-zA-Z0-9.]{2,}(?<![.])): ((\\d+\\.?)+)$"
  deps <- list()
  for (depLine in depLines) {
    if (!grepl(pattern, depLine, perl=TRUE))
      stop("jasp-deps.txt is corrupted; each line should have the form `- valid.package.name: 0.1.5`")
    matches <- stringr::str_match(depLine, pattern)
    name <- matches[, 2]
    version <- matches[, 3]
    deps[[name]] <- version
  }
  return(deps)
}


getDepsFromUser <- function(modulePath) {
  depPkgs <- names(getDepsFromFile(modulePath))
  userDeps <- list()
  for (dep in depPkgs) {
    if (! dep %in% installed.packages())
      stop("You must install the dependency ", dep, " before you can test plots")
    userDeps[[dep]] <- packageVersion(dep)
  }
  return(userDeps)
}


writeUpdatedDeps <- function(newDeps) {
  deps <- vector("list", length(newDeps))
  for (newDep in newDeps)
    deps[[newDep[["pkg"]]]] <- newDep[["userVersion"]]
  writeDepsToFile(deps)
}

getDepsFileLocation <- function(modulePath) {
  return(file.path(modulePath, "tests", "figs", "jasp-deps.txt"))
}


writeDepsToFile <- function(deps, modulePath, add = TRUE) {
  if (add) {
    depsInFile <- getDepsFromFile(modulePath)
    deps <- modifyList(depsInFile, deps)
  }

  txt <- character(0)
  for (i in seq_along(deps)) {
    txt <- c(txt, paste0("- ", names(deps)[i], ": ", deps[[i]]))
  }
  txt <- paste(txt, collapse="\n")

  pathToDeps <- getDepsFileLocation(modulePath)
  fileConn <- file(pathToDeps, open = "w")
  writeLines(txt, fileConn)
  close(fileConn)
}


hasNewPlots <- function(testthatResults) {
  any(grepl("`vdiffr::manage_cases()`", unlist(testthatResults), fixed=TRUE))
}


printSuccessFailureModules <- function(testResults) {
  if (length(testResults[["passedModules"]]) > 0)
    message("\nThe tests passed for ", paste(testResults[["passedModules"]], collapse = ", "), ".")

  if (length(testResults[["failedModules"]]) > 0)
    message("\nThe tests failed for ", paste(testResults[["failedModules"]], collapse = ", "), ". Scroll up for more details.")
}



printTipsNewPlots <- function(hasNewPlots, name = NULL) {
  if (hasNewPlots) {
    analysisName <- ""
    if (!is.null(name))
      analysisName <- paste0('"', name,'"')
    message("To more easily validate new plots use manageTestPlots(", analysisName, ")")
  }
}


makeUnitTestsFromResults <- function(results, name, dataset, options) {
  if (!is.list(results) || is.null(names(results)) || results$status == "error")
    stop("Can't make unit test from results: not a results list")

  tests <- getTests(results$results)
  if (length(tests) == 0)
    stop("Could not identify any tables or plots to test")

  output <- makeExpectations(tests, name, options, dataset)
  cat(output)
}


getTests <- function(results) {
  tests <- list()

  markResultsLocationExtractTests <- function(x) {
    if (! "list" %in% class(x))
      return(x)

    unitTestType <- NULL
    if (all(c("data", "schema") %in% names(x)))
      unitTestType <- "table"
    else if (all(c("data", "width", "height") %in% names(x)))
      unitTestType <- "plot"

    if (!is.null(unitTestType)) {
      testid <- length(tests)
      tests[[paste0("itemToUnitTest-", testid)]] <<- list(
        title = unlist(x[["title"]]),
        id = testid,
        type = unitTestType,
        data =  ifelse(unitTestType == "table", makeTestTable(x[["data"]], print=FALSE), ""))
      x[["itemToUnitTest"]] <- testid
    }

    return(lapply(x, markResultsLocationExtractTests))
  }

  markedResults <- markResultsLocationExtractTests(results)
  if (length(tests) > 0) {
    tests <- addPathIndexToTests(tests, markedResults)
    tests <- getOneTestPerCollection(tests)
  }

  return(tests)
}


addPathIndexToTests <- function(tests, markedResults) {
  results <- unlist(markedResults)
  for (testName in names(tests)) {
    id <- tests[[testName]][["id"]]
    dotSeparatedPathName <- getTestLocationInResultsById(results, id)
    tests[[testName]][["index"]] <- normalizeTestPath(dotSeparatedPathName)
  }
  return(tests)
}


getTestLocationInResultsById <- function(results, id) {
  testIndices <- which(grepl("itemToUnitTest", names(results)))
  index <- testIndices[results[testIndices] == id]
  if (length(index) != 1)
    stop("Failed to uniquely identify test case in results")

  location <- names(results)[index]
  return(gsub("itemToUnitTest", "data", location))
}


normalizeTestPath <- function(index) {
  indexNames <- unlist(strsplit(index, ".", fixed=TRUE))
  path <- paste0('[["', paste0(indexNames, collapse='"]][["'), '"]]')

  return(path)
}


getOneTestPerCollection <- function(tests) {
  pathNames <- NULL
  for (test in tests) {
    pathNames <- c(pathNames, test$index)
  }

  firstSiblingOrUnique <- rep(TRUE, length(tests))

  purgedTests <- tests[firstSiblingOrUnique]

  return(purgedTests)
}


makeExpectations <- function(tests, name, options, dataset) {
  centralizePreamble <- FALSE
  if (length(tests) > 1)
    centralizePreamble <- TRUE

  expectations <- ""

  if (centralizePreamble) {
    preamble <- addPreambleLines(name, options, dataset)
    expectations <- paste0(preamble, "\n")
  }

  for (test in tests) {
    if (!test$type %in% c("table", "plot"))
      stop("Unknown test type extracted from results, cannot continue: ", test$type)

    expectation <- makeSingleExpectation(test, name, options, dataset, centralizePreamble)
    expectations <- paste(expectations, expectation, sep="\n\n")
  }

  return(expectations)
}


makeSingleExpectation <- function(test, name, options, dataset, centralizePreamble) {
  if (!is.character(test$title) || test$title == "") {
    test$title <- paste("titleless", test$type, test$id, sep="-")
    warning(test$type, " does not have a title, using a generic one: ", test$title, immediate.=TRUE)
  }

  openingLine <- addOpeningLine(test)

  preambleLines <- NA
  if (!centralizePreamble) {
    preambleLines <- addPreambleLines(name, options, dataset)
    preambleLines <- gsub("\n", "\n\t", paste0("\t", preambleLines))
  }

  testSpecificLines <- NA
  if (test$type == "table")
    testSpecificLines <- addTableSpecificLines(test)
  else if (test$type == "plot")
    testSpecificLines <- addPlotSpecificLines(test, name)

  closingLine <- "})"

  expectation <- paste(openingLine, preambleLines, testSpecificLines, closingLine, sep="\n")
  expectation <- gsub("NA\n", "", expectation)

  return(expectation)
}


addOpeningLine <- function(test) {
  opening <- paste0('test_that("', test$title)

  titleContainsTestType <- grepl(test$type, test$title, ignore.case=TRUE)
  if (test$type == "table")
    opening <- paste0(opening, ifelse(titleContainsTestType, '',  ' table'), ' results match", {')
  else if (test$type == "plot")
    opening <- paste0(opening, ifelse(titleContainsTestType, '', ' plot'), ' matches", {')

  return(opening)
}


addPreambleLines <- function(name, options, dataset) {
  settingOfOptions <- addOptionSpecificationLines(name, options)
  settingOfSeed <- "set.seed(1)"
  runningOfAnalysis <- addRunAnalysisLines(name, dataset)

  return(paste(settingOfOptions, settingOfSeed, runningOfAnalysis, sep="\n"))
}


addRunAnalysisLines <- function(name, dataset) {
  if (is.character(dataset))
    dataArg <- paste0('"', dataset ,'"')
  else
    dataArg <- paste0('dataset')

  readingData <- paste0('dataset <- ', paste(capture.output(dput(dataset)), collapse="\n"))

  running <- paste0('results <- runAnalysis("', name, '", ', dataArg, ', options)')

  if (is.character(dataset))
    return(running)
  else
    return(paste(readingData, running, sep="\n"))
}


addTableSpecificLines <- function(test) {
  gettingTable <- paste0('\ttable <- results[["results"]]', test$index)

  comparingTables <- paste0("\texpect_equal_tables(table,\n\t\t", gsub("\n", "\n\t\t", test$data), ")")

  return(paste(gettingTable, comparingTables, sep="\n"))
}


addPlotSpecificLines <- function(test, name) {
  gettingPlotName <- paste0('\tplotName <- results[["results"]]', test$index)

  gettingPlot <- paste0('\ttestPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]')

  title <- gsub("-+", "-", gsub("\\W", "-", tolower(test$title)))
  comparingPlots <- paste0('\texpect_equal_plots(testPlot, "', title, '", dir="', name, '")')

  return(paste(gettingPlotName, gettingPlot, comparingPlots, sep="\n"))
}


addOptionSpecificationLines <- function(name, options) {
  settingOfOptions <- paste0('options <- analysisOptions("', name, '")')

  nonDefaultOpts <- getNonDefaultOptions(name, options)
  if (length(nonDefaultOpts) > 0) {
    nonDefaults <- paste0("options$", names(nonDefaultOpts), " <- ", nonDefaultOpts, collapse="\n")
    settingOfOptions <- paste0(settingOfOptions, "\n", nonDefaults)
  }

  return(settingOfOptions)
}


getNonDefaultOptions <- function(name, options) {
  defaultOpts <- analysisOptions(name)
  if (!is.list(defaultOpts) || is.null(names(defaultOpts)))
    stop("Couldn't find the default analysis options for this analysis")

  nonDefaultOpts <- NULL
  for (optName in names(options)) {
    optValue <- options[[optName]]
    if (!isTRUE(all.equal(defaultOpts[[optName]], optValue))) {
      options[[optName]] <- prepOptionValueForPrinting(optValue)
      nonDefaultOpts <- c(nonDefaultOpts, options[optName])
    }
  }

  return(nonDefaultOpts)
}


prepOptionValueForPrinting <- function(value) {
  if (is.list(value))
    result <- paste(capture.output(dput(value)), collapse="\n")
  else if (is.character(value) && length(value) == 1 && !startsWith(value, "\""))
    result <- paste0("\"", value, "\"")
  else
    result <- value

  return(result)
}
