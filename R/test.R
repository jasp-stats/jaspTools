#' Run tests on Travis.
#'
#' Runs all tests that are in the test directory of the module. Should not be used in RStudio.
#'
#' @param modulePath The path to the module on Travis. Should be obtained from testthat.R.
#'
#' @export runTestsTravis
runTestsTravis <- function(modulePath) {
  if (Sys.getenv("CI") == "") {
    testAll()
  } else {
    if (Sys.getenv("REQUIRED_PKGS") == "")
      stop("Could not find environment variable `REQUIRED_PKGS`")

    .libPaths(c(.libPaths(), Sys.getenv("REQUIRED_PKGS")))

    setupJaspTools(pathJaspDesktop = NULL, pathJaspRequiredPkgs = normalizePath(Sys.getenv("REQUIRED_PKGS")), installJaspModules = FALSE, force = FALSE)

    remotes::install_local(modulePath, upgrade = "never", force = FALSE)

    setPkgOption("module.dirs", modulePath)

    options("testthat.progress.max_fails" = 1E3L)

    result <- testthat::test_dir("tests/testthat")
    result <- as.data.frame(result)

    if (sum(result$failed) > 0 || sum(result$error) > 0)
      quit(save = "no", status = 1)
  }
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
  nameMatch  <- matchCaseTestFileAnalysisName(name, testDir)
  fileToTest <- file.path(testDir, paste0("test-", nameMatch, ".R"))

  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true") # this is to prevent vdiffr from skipping plots
  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
  })

  fixRNGForTesting()
  results <- testthat::test_file(fileToTest)

  printTipsNewPlots(hasNewPlots(results), name)
}

 fixRNGForTesting <- function() {
   suppressWarnings(RNGkind(sample.kind = "Rounding"))
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
      handleVersionMismatches(versionMismatches, name, modulePath)

    vdiffr::manage_cases(modulePath, filter = name)
  }
}

#' Allows users to add package dependencies to unit testing, specifically to plot testing
#'
#' Testing might fail if dependencies are not the same across platforms (e.g., different versions of jaspGraphs).
#' If this is the case then those dependencies should be monitored and errors given when they do not match.
#' This function allows you to define "unit test breaking" dependencies for plots
#'
#'
#' @param dep A single character value of a package name currently installed on your system
#' @param modulePath Specify the path to the root folder of the module, or specify it through `setPkgOption("/.../")`
#' @return This function only has a side effect: updating figs/jasp-deps.txt
#' @examples
#'
#' addTestDependency("jaspGraphs")
#'
#' @export addTestDependency
addTestDependency <- function(dep, modulePath = getPkgOption("module.dirs")) {
  if (!is.character(dep) || length(dep) > 1)
    stop("Expecting single name of a package")

  if (length(modulePath) != 1 || modulePath == "")
    stop("Not sure where to write the dependency to. Please specify one module.")

  depsInFile <- getDepsFromFile(modulePath)
  if (dep %in% names(depsInFile))
    stop("Package already exists in dependency file")

  if (!dep %in% installed.packages())
    stop("The package is not installed on your system, cannot retrieve a version")

  depToWrite <- list()
  depToWrite[[dep]] <- packageVersion(dep)
  writeDepsToFile(depToWrite, modulePath)
  message(paste0("Dependency `", dep, "` added to jasp-deps.txt"))
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

handleVersionMismatches <- function(versionMismatches, name, modulePath) {
  onlyNewer <- length(versionMismatches[["newer"]]) > 0 && length(versionMismatches[["older"]]) == 0
  if (onlyNewer) {
    if (is.null(name)) {
      message("The installed packages on your system are newer than the ones used to create the library of test plots. Automatically updated the dependencies file; please make sure there are no plot mismatches in the Shiny app")
      writeUpdatedDeps(versionMismatches[["newer"]], modulePath)
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

writeUpdatedDeps <- function(newDeps, modulePath) {
  deps <- vector("list", length(newDeps))
  for (newDep in newDeps)
    deps[[newDep[["pkg"]]]] <- newDep[["userVersion"]]
  writeDepsToFile(deps, modulePath)
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

getInstallLocationDep <- function(dep) {
  pkgs <- installed.packages()
  index <- min(which(row.names(pkgs) == dep))
  return(pkgs[index, "LibPath"])
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
