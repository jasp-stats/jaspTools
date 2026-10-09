#' Run a JASP analysis in R.
#'
#' \code{runAnalysis} makes it possible to execute a JASP analysis in R. Usually this
#' process is a bit cumbersome as there are a number of objects unique to the
#' JASP environment. Think .ppi, data-reading, etc. jaspTools sets those up for
#' you by running the real JASP bridge (the SyntaxInterface library that powers
#' jaspSyntax): the dataset is loaded as a genuine JASP DataSet, the options are
#' validated, encoded and stamped with provenance by the analysis' own QML form,
#' and the results are decoded on the way out - exactly like the engine does.
#' This means a jaspTools run behaves like a JASP run: plain column names go in,
#' plain column names come out, and invalid options are rejected the same way.
#' Note that \code{runAnalysis} sources JASP analyses every time it runs, so any
#' change in analysis code between calls is incorporated. The output of the
#' analysis is shown automatically through a call to \code{view} and returned
#' invisibly.
#'
#'
#' @param name String indicating the name of the analysis to run. This name is
#' identical to that of the main function in a JASP analysis.
#' @param dataset Data.frame, matrix, string name or string path; if it's a string then jaspTools
#' first checks if it's valid path and if it isn't if the string matches one of the JASP datasets (e.g., "debug.csv").
#' By default the directory in Resources is checked first, unless called within a testthat environment, in which case tests/datasets is checked first.
#' Ignored when \code{datasets} is given.
#' @param options List of options to supply to the analysis (see also
#' \code{analysisOptions}).
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the
#' analysis.
#' @param makeTests Boolean indicating whether to create testthat unit tests and print them to the terminal.
#' @param datasets Named list of dataframes for a multiDataSetAware analysis: the names become
#' the dataset titles and may be referenced by the form's dataset selection options, one DataSet
#' per entry is loaded into the bridge workspace, and the slices arrive at the analysis keyed by
#' filter id - the same id the selection option holds, so \code{datasets[[key]]} is the whole
#' API (see the datasets contract in \code{jaspBase}). \code{dataset} must then be NULL.
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
runAnalysis <- function(name, dataset = NULL, options, view = TRUE, quiet = FALSE, makeTests = FALSE, datasets = NULL) {
  if (is.list(options) && is.null(names(options)) && any(names(unlist(lapply(options, attributes))) == "analysisName"))
    stop("The provided list of options is not named. Did you mean to index in the options list (e.g., options[[1]])?")

  if (!is.list(options) || is.null(names(options)))
    stop("The options should be a named list (you can obtain it through `analysisOptions()`")

  if (missing(name)) {
    name <- attr(options, "analysisName")
    if (is.null(name))
      stop("Please supply an analysis name")
  }

  if (!is.null(datasets) && !is.null(dataset))
    stop("`datasets` (multiDataSetAware run) and `dataset` (single dataset run) are mutually exclusive")

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

  initAnalysisRuntime(dataset = dataset, options = options, makeTests = makeTests, datasets = datasets)

  # Validate + encode the options exactly like the engine does: through the analysis' own QML
  # form via the SyntaxInterface bridge. This fills defaults, rejects junk, binds each form to
  # the dataset its selection option names (multiDataSetAware runs), stamps the .meta
  # provenance, encodes column values per dataset and queues the per-(dataset,filter) slices.
  parsed <- parseOptionsThroughBridge(name, options)

  args <- fetchRunArgs(name, parsed)

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

  # The analysis only ever saw encoded names (the engine's contract too); decode the whole
  # results payload against every dataset the bridge workspace holds - jaspTools' equivalent
  # of Engine's sendString - so test authors keep matching on the plain names they typed.
  jsonResults <- jaspSyntax::decodeJsonText(jsonResults)

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

# Analysis metadata straight from the module's Description.qml, through the bridge's own
# description parser (jaspSyntax::resolveAnalysisQml) - the func -> qml mapping cannot be
# guessed from conventions (e.g. func "multiDataSetFunc" lives in "testMultiDataSet.qml" in
# jaspTestModule), and preloadData/awareness come from the same authoritative source.
analysisRunInfo <- function(name) {
  modulePath <- getModulePathFromRFunction(name)

  resolved <- tryCatch(
    jaspSyntax::resolveAnalysisQml(modulePath, name),
    error = function(e) {
      if (endsWith(name, "Internal"))
        tryCatch(jaspSyntax::resolveAnalysisQml(modulePath, sub("Internal$", "", name)), error = function(e2) NULL)
      else
        NULL
    }
  )

  if (is.null(resolved))
    stop("Could not find analysis `", name, "` in the Description.qml of the module at ",
         modulePath, " - please use the name it declares there.", call. = FALSE)

  if (!isTRUE(file.exists(resolved$qmlFile)))
    stop("QML file `", resolved$qmlFileName, "` of analysis `", name, "` not found at ",
         resolved$qmlFile, call. = FALSE)

  resolved
}

parseOptionsThroughBridge <- function(name, options) {
  info <- analysisRunInfo(name)

  # Same serialization jaspBase uses for the syntax wrapper (common.R toJSON): auto_unbox so
  # scalar options ("Score", TRUE, filter ids, dataset names in selection options) arrive as
  # scalars - boxed one-element arrays are rejected by the bridge.
  optionsJson <- as.character(jsonlite::toJSON(options, auto_unbox = TRUE, digits = NA, null = "null"))

  status <- jaspSyntax::loadQmlAndParseOptionsStatus(
    info$moduleName, info$analysisName, info$qmlFile, optionsJson, info$version, info$preloadData)

  list(
    options          = status$options,
    multiDataSetJson = if (nzchar(status$multiDataSetJson)) status$multiDataSetJson else NULL,
    preloadData      = info$preloadData
  )
}

fetchRunArgs <- function(name, parsed) {
  possibleArgs <- list(
    name = name,
    functionCall = findCorrectFunction(name),
    title = "",
    requiresInit = TRUE,
    options = parsed$options,
    dataKey = "null",
    resultsMeta = "null",
    stateKey = "null",
    preloadData = parsed$preloadData
  )

  # For multiDataSetAware runs: the blob the bridge built while parsing ({ids = filter keys,
  # names, dataSetIds, primary}) - jaspBase::runJaspResults then pulls the queued slices from
  # the bridge, one read at a time, and hands the analysis `datasets` keyed by filter id.
  if (!is.null(parsed$multiDataSetJson))
    possibleArgs$multiDataSetJson <- parsed$multiDataSetJson

  runArgs <- formals(jaspBase::runJaspResults)
  argNames <- intersect(names(possibleArgs), names(runArgs))
  return(possibleArgs[argNames])
}

initAnalysisRuntime <- function(dataset, options, makeTests, datasets = NULL, ...) {
  # first we reinstall any changed modules in the personal library
  reinstallChangedModules()

  # data goes into the bridge workspace as real DataSets (id + column encoder + default
  # filter), before the options are parsed: the QML forms need them to bind columns and to
  # resolve the dataset selection options. Both loaders clear previous state themselves,
  # repeated runs never inherit datasets; without data the workspace is cleared explicitly.
  if (!is.null(datasets)) {
    if (!is.list(datasets) || is.null(names(datasets)) || any(!nzchar(names(datasets))))
      stop("`datasets` must be a named list of dataframes - the names become the dataset titles")
    datasets <- lapply(datasets, loadCorrectDataset)
    invisible(jaspSyntax::loadDataSets(datasets))
  } else if (!is.null(dataset)) {
    invisible(jaspSyntax::loadDataSet(loadCorrectDataset(dataset)))
  } else {
    jaspSyntax::clearDatasetState()
  }

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
        install.packages(modulePath, type = "source", repos = NULL, quiet = TRUE, INSTALL_opts = "--no-multiarch")
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

.resetRunTimeInternals <- function() {
  .setInternal("state", list())
}
