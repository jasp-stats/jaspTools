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
#' By default the directory in Resources is checked first, unless called within a testthat environment, in which case tests/datasets is checked first.
#' @param options List of options to supply to the analysis (see also
#' \code{analysisOptions}).
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the
#' analysis.
#' @param makeTests Boolean indicating whether to create testthat unit tests and print them to the terminal.
#' @param encodedDataset Boolean indicating whether to assume that the dataset is already encoded.
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
runAnalysis <- function(name, dataset = NULL, options, view = TRUE, quiet = FALSE, makeTests = FALSE, encodedDataset = FALSE, datasets = NULL) {
  if (is.list(options) && is.null(names(options)) && any(names(unlist(lapply(options, attributes))) == "analysisName"))
    stop("The provided list of options is not named. Did you mean to index in the options list (e.g., options[[1]])?")

  if (!is.list(options) || is.null(names(options)))
    stop("The options should be a named list (you can obtain it through `analysisOptions()`")

  if (missing(name)) {
    name <- attr(options, "analysisName")
    if (is.null(name))
      stop("Please supply an analysis name")
  }

  if (insideTestEnvironment()) {
    view  <- FALSE
    quiet <- TRUE
  }

  oldWd       <- getwd()
  oldLang     <- Sys.getenv("LANG")
  oldLanguage <- Sys.getenv("LANGUAGE")
  on.exit({
    .resetRunTimeInternals()
    setwd(oldWd)
    Sys.setenv(LANG = oldLang)
    Sys.setenv(LANGUAGE = oldLanguage)
  }, add = TRUE)

  initAnalysisRuntime(dataset = dataset, options = options, makeTests = makeTests, encodedDataset = encodedDataset, datasets = datasets)
  args <- fetchRunArgs(name, options, datasets)

  if (quiet) {
    sink(tempfile())
    on.exit({suppressWarnings(sink(NULL))}, add = TRUE)
    returnVal <- suppressWarnings(do.call(jaspBase::runJaspResults, args))
    sink(NULL)
  } else {
    returnVal <- do.call(jaspBase::runJaspResults, args)
  }

  # always TRUE after jaspResults is merged into jaspBase
  jsonResults <- if (inherits(returnVal, c("jaspResultsR", "R6"))) {
    getJsonResultsFromJaspResults(returnVal)
  } else {
    getJsonResultsFromJaspResultsLegacy()
  }

  transferPlotsFromjaspResults()

  results <- processJsonResults(jsonResults)

  if (insideTestEnvironment())
    .setInternal("lastResults", jsonResults)

  if (view)
    view(jsonResults)

  if (makeTests)
    makeUnitTestsFromResults(results, name, dataset, options)

  return(invisible(results))
}

fetchRunArgs <- function(name, options, datasets = NULL) {
  possibleArgs <- list(
    name = name,
    functionCall = findCorrectFunction(name),
    title = "",
    requiresInit = TRUE,
    options = jsonlite::toJSON(options),
    dataKey = "null",
    resultsMeta = "null",
    stateKey = "null",
    preloadData = parsePreloadDataFromDescriptionQml(name)
  )

  # multiDataSetAware runs (runAnalysis(..., datasets = <named list of dataframes>)): hand jaspBase
  # the same {ids, names} blob the engine does; the slice queue in rbridge.R then feeds it.
  if (!is.null(datasets)) {
    titles <- attr(datasets, "dataSetNames")
    if (is.null(titles)) titles <- names(datasets)
    possibleArgs$multiDataSetJson <- jsonlite::toJSON(list(
      ids   = names(datasets),
      names = as.list(titles[names(datasets)])
    ), auto_unbox = TRUE)
  }

  runArgs <- formals(jaspBase::runJaspResults)
  argNames <- intersect(names(possibleArgs), names(runArgs))
  return(possibleArgs[argNames])
}

initAnalysisRuntime <- function(dataset, options, makeTests, encodedDataset = FALSE, datasets = NULL, ...) {
  # first we reinstall any changed modules in the personal library
  reinstallChangedModules()

  # dataset to be found in the analysis when it needs to be read
  .setInternal("dataset", dataset)
  preloadDataset(dataset, options, encodedDataset = encodedDataset)
  setupMultiDataSet(datasets)

  # prevent the results from being translated (unless the user explicitly wants to)
  Sys.setenv(LANG = getPkgOption("language"))
  Sys.setenv(LANGUAGE = getPkgOption("language"))

  # jaspBase and jaspResults needs to be loaded until they are merged and the packages handle dependencies correctly
  initializeCoreJaspPackages()

  # ensure that unit tests results are consistent
  if (makeTests)
    set.seed(1)
}

reinstallChangedModules <- function() {
  modulePaths <- getModulePaths()
  if (isFALSE(getPkgOption("reinstall.modules")) || length(modulePaths) == 0)
    return()

  md5Sums <- .getInternal("modulesMd5Sums")
  for (modulePath in modulePaths) {

    if (isBinaryPackage(modulePath))
      next

    srcFiles <- c(
      list.files(modulePath,                   full.names = TRUE, pattern = "(NAMESPACE|DESCRIPTION)$"),
      list.files(file.path(modulePath, "src"), full.names = TRUE, pattern = "(\\.(cpp|c|hpp|h)|(Makevars|Makevars\\.win))$"),
      list.files(file.path(modulePath, "R"),   full.names = TRUE, pattern = "\\.R$")
    )
    if (length(srcFiles) == 0)
      next

    newMd5Sums <- tools::md5sum(srcFiles)
    if (length(md5Sums) == 0 || !modulePath %in% names(md5Sums) || !all(newMd5Sums %in% md5Sums[[modulePath]])) {
      moduleName <- getModuleName(modulePath)
      if (moduleName %in% loadedNamespaces())
        pkgload::unload(moduleName, quiet = TRUE)

      message("Installing ", moduleName, " from source")
      suppressWarnings(install.packages(modulePath, type = "source", repos = NULL, quiet = TRUE, INSTALL_opts = "--no-multiarch"))

      if (moduleName %in% installed.packages()) {
        md5Sums[[modulePath]] <- newMd5Sums
      } else {
        # to prevent the installation output from cluttering the console on each analysis run, we do this quietly.
        # however, it is kinda nice to show errors, so we call the function again here and allow it to print this time (tryCatch/sink doesn't catch the installation failure reason).
        install.packages(modulePath, type = "source", repos = NULL, INSTALL_opts = "--no-multiarch")
        if (!moduleName %in% installed.packages())
          stop("The installation of ", moduleName, " failed; you will need to fix the issue that prevents `install.packages()` from installing the module before any analysis will work")
      }
    }
  }

  .setInternal("modulesMd5Sums", md5Sums)
}

initializeCoreJaspPackages <- function() {
  require(jaspBase)
  if (jaspBaseIsLegacyVersion()) {
    warning("jaspBase should be at least version 0.16.4! Continuing now but if something crashes update jaspBase.", domain = NA)
    require(jaspResults)
    jaspResults::initJaspResults()
    assign("jaspResultsModule", list(create_cpp_jaspResults = function(name, state) get("jaspResults", envir = .GlobalEnv)$.__enclos_env__$private$jaspObject), envir = .GlobalEnv)
  }
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
  pathPlotsjaspResults <- file.path(tempdir(), "jaspResults", "plots") # as defined in jaspResults pkg
  pathPlotsjaspTools <- getTempOutputLocation("html")
  if (dir.exists(pathPlotsjaspResults)) {
    plots <- list.files(pathPlotsjaspResults)
    if (length(plots) > 0) {
      file.copy(file.path(pathPlotsjaspResults, plots), pathPlotsjaspTools, overwrite=TRUE)
    }
  }
}

getJsonResultsFromJaspResults <- function(jaspResults) {
  return(jaspResults$.__enclos_env__$private$getResults())
}

getJsonResultsFromJaspResultsLegacy <- function() {
  return(jaspResults$.__enclos_env__$private$getResults())
}

#' Queue up the datasets of a multiDataSetAware run (runAnalysis(..., datasets=...)).
#' `datasets` is a named list of dataframes keyed by dataset id, optionally with titles in
#' attr(datasets, "dataSetNames") - exactly what jaspBase hands an aware analysis. The slices
#' are handed out in order by .readDataSetRequestedNative (mirroring the engine's slice queue),
#' and a per-dataset encoded-name map (JASPColumn_<id>_<columnIndex>, like DataSet::setupEncoderPrefix)
#' backs the .decodeColNamesForDataSet stub so encoded option values route to the right column.
setupMultiDataSet <- function(datasets) {
  if (is.null(datasets))
    return(invisible(NULL))

  if (!is.list(datasets) || is.null(names(datasets)) || any(!nzchar(names(datasets))))
    stop("`datasets` must be a named list of dataframes, keyed by dataset id")

  .setInternal("multiDataSetQueue", datasets)

  maps <- lapply(seq_along(datasets), function(i) {
    columns <- names(datasets[[i]])
    encoded <- sprintf("JASPColumn_%s_%d", names(datasets)[[i]], seq_along(columns) - 1L)
    stats::setNames(columns, encoded)
  })
  names(maps) <- names(datasets)
  .setInternal("multiDataSetNameMaps", maps)

  invisible(NULL)
}

.resetRunTimeInternals <- function() {
  .setInternal("state", list())
  .setInternal("dataset", "")
  .setInternal("multiDataSetQueue", NULL)
}
