#' Run a JASP analysis in R.
#'
#' \code{runAnalysis} makes it possible to execute a JASP analysis in R. Usually this
#' process is a bit cumbersome as there are a number of objects unique to the
#' JASP environment. Think .ppi, data-reading, etc. These (rcpp) objects are
#' replaced in the jaspTools so you do not have to deal with them. Note that
#' \code{runAnalysis} sources JASP analyses every time it runs, so any change in
#' analysis code between calls is incorporated. The output of the analysis is
#' shown automatically through a call to \code{view} and returned
#' invisibly.
#'
#'
#' @param name String indicating the name of the analysis to run. This name is
#' identical to that of the main function in a JASP analysis.
#' @param dataset Data.frame, matrix, string name or string path; if it's a string then jaspTools
#' first checks if it's valid path and if it isn't if the string matches one of the JASP datasets (e.g., "debug.csv").
#' By default the folder in Resources is checked first, unless called within a testthat environment, in which case tests/datasets is checked first.
#' @param options List of options to supply to the analysis (see also
#' \code{analysisOptions}).
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the
#' analysis.
#' @param makeTests Boolean indicating whether to create testthat unit tests and print them to the terminal.
#' @examples
#'
#' options <- analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#' runAnalysis("BinomialTest", "debug", options)
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' options <- analysisOptions('{
#'    "id" : 6,
#'    "name" : "BinomialTest",
#'    "options" : {
#'       "VovkSellkeMPR" : false,
#'       "confidenceInterval" : false,
#'       "confidenceIntervalInterval" : 0.950,
#'       "descriptivesPlots" : false,
#'       "descriptivesPlotsConfidenceInterval" : 0.950,
#'       "hypothesis" : "notEqualToTestValue",
#'       "plotHeight" : 300,
#'       "plotWidth" : 160,
#'       "testValue" : 0.50,
#'       "variables" : [ "contBinom" ]
#'    },
#'    "perform" : "run",
#'    "revision" : 1,
#'    "settings" : {
#'       "ppi" : 192
#'    }
#' }')
#' runAnalysis("BinomialTest", "debug.csv", options)
#'
#'
#' @export runAnalysis
runAnalysis <- function(name, dataset, options, view = TRUE, quiet = FALSE, makeTests = FALSE) {

  if (missing(name)) {
    name <- attr(options, "analysisName")
    if (is.null(name))
      stop("Please supply an analysis name")
  }

  if (insideTestEnvironment()) {
    view  <- FALSE
    quiet <- TRUE
  }

  oldLibPaths <- .libPaths()
  oldWd       <- getwd()
  oldLocale   <- Sys.getlocale()
  on.exit({
    .resetRunTimeInternals()
    .libPaths(oldLibPaths)
    setwd(oldWd)
    localeRes <- suppressWarnings(Sys.setlocale(category = "LC_ALL", locale = oldLocale))
  })

  initAnalysisRuntime(dataset = dataset, makeTests = makeTests)
  args <- fetchRunArgs(name, options)

  if (quiet) {
    sink(tempfile())
    on.exit({suppressWarnings(sink(NULL))}, add = TRUE)
    returnVal <- suppressWarnings(do.call(jaspBase::runJaspResults, args))
    sink(NULL)
  } else {
    returnVal <- do.call(jaspBase::runJaspResults, args)
  }

  transferPlotsFromjaspResults()

  jsonResults <- getJsonResultsFromJaspResults()
  results     <- processJsonResults(jsonResults)

  if (insideTestEnvironment())
    .setInternal("lastResults", jsonResults)

  if (view)
    view(jsonResults)

  if (makeTests)
    makeUnitTestsFromResults(results, name, dataset, options)

  return(invisible(results))
}

fetchRunArgs <- function(name, options) {
  possibleArgs <- list(
    name = name,
    functionCall = findCorrectFunction(name),
    title = "",
    requiresInit = TRUE,
    options = jsonlite::toJSON(options),
    dataKey = "null",
    resultsMeta = "null",
    stateKey = "null"
  )

  runArgs <- formals(jaspBase::runJaspResults)
  argNames <- intersect(names(possibleArgs), names(runArgs))
  return(possibleArgs[argNames])
}

initAnalysisRuntime <- function(dataset, makeTests, ...) {
  # source all the R analysis files
  reinstallChangedModules()
  .setInternal("dataset", dataset) # dataset to be found later when it needs to be read
  .libPaths(c(getPkgOption("pkgs.dir"), .libPaths())) # location of JASP's R packages
  localeRes <- suppressWarnings(Sys.setlocale(category = "LC_ALL", locale = getPkgOption("locale"))) # ensure it defaults to English unless specified otherwise
  initializeCoreJaspPackages()
  if (makeTests)
    set.seed(1)
}

reinstallChangedModules <- function() {
  if (!"jaspBase" %in% installed.packages())
    stop("Cannot find the basic module `jaspBase`; please run `installJaspPkg(\"jaspBase\")`")

  modules   <- getModulePaths()
  reinstall <- getPkgOption("reinstall.modules")
  if (!reinstall || length(modules) == 0)
    return()

  msgPrinted <- FALSE
  md5Sums    <- .getInternal("modulesMd5Sums")
  for (module in modules) {
    files <- list.files(module, include.dirs = FALSE, full.names = TRUE, recursive = TRUE, pattern="(NAMESPACE$)|(DESCRIPTION$)|(\\.R$)")
    if (length(files) == 0)
      next

    newMd5Sums <- tools::md5sum(files)
    if (length(md5Sums) == 0 || !module %in% names(md5Sums) || !all(newMd5Sums %in% md5Sums[[module]])) {
      if (!msgPrinted) {
        message("Reinstalling changed module(s)")
        msgPrinted <- TRUE
      }

      devtools::install_local(module, force = TRUE, upgrade = "never", quiet = TRUE)
      devtools::reload(module)
      md5Sums[[module]] <- newMd5Sums
    }
  }

  .setInternal("modulesMd5Sums", md5Sums)
}

initializeCoreJaspPackages <- function() {
  require(jaspResults)
  require(jaspBase)
  jaspResults::initJaspResults()

  assign("jaspResultsModule", list(create_cpp_jaspResults = function(name, state) get("jaspResults", envir = .GlobalEnv)$.__enclos_env__$private$jaspObject), envir = .GlobalEnv)
}

processJsonResults <- function(jsonResults) {
  if (jsonlite::validate(jsonResults))
    results <- jsonlite::fromJSON(jsonResults, simplifyVector=FALSE)
  else
    stop("Could not process json result from jaspResults")

  results[["state"]] <- .getInternal("state")

  figures <- results$state$figures
  if (length(figures) > 1 && !is.null(names(figures)))
    results$state$figures <- figures[order(as.numeric(tools::file_path_sans_ext(basename(names(figures)))))]

  return(results)
}

transferPlotsFromjaspResults <- function() {
  pathPlotsjaspResults <- file.path(tempdir(), "jaspResults", "plots")
  pathPlotsjaspTools <- file.path(tempdir(), "jaspTools", "html")
  if (dir.exists(pathPlotsjaspResults)) {
    plots <- list.files(pathPlotsjaspResults)
    if (length(plots) > 0) {
      file.copy(file.path(pathPlotsjaspResults, plots), pathPlotsjaspTools, overwrite=TRUE)
    }
  }
}

getJsonResultsFromJaspResults <- function() {
  return(jaspResults$.__enclos_env__$private$getResults())
}

.resetRunTimeInternals <- function() {
  .setInternal("state", list())
  .setInternal("dataset", "")
}

