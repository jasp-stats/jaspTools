#' Run a JASP analysis in R.
#'
#' \code{runAnalysis} makes it possible to execute a JASP analysis in R. jaspTools
#' loads the dataset into jaspSyntax as a genuine JASP DataSet and runs the analysis
#' with `jaspBase::runWrappedAnalysis()`, the function every R wrapper of an analysis
#' calls: the QML form of the analysis validates and encodes the options, and the
#' results come back with the plain column names, as in JASP. Plain column names go in,
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
#' A multiDataSetAware analysis gets its datasets only through \code{datasets}, as with its R
#' wrapper: run with \code{dataset}, it gets no \code{datasets} argument.
#' @param encodedDataset Ignored. Kept for tests written for \code{encodeOptionsAndDataset()}:
#' \code{runAnalysis} encodes the options and the dataset itself.
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
runAnalysis <- function(name, dataset = NULL, options, view = TRUE, quiet = FALSE, makeTests = FALSE, datasets = NULL, encodedDataset = FALSE) {
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

  display <- runDisplay(view, quiet)
  restoreRunState <- saveRunState()
  on.exit(restoreRunState(), add = TRUE)

  datasets <- initAnalysisRuntime(dataset = dataset, options = options, makeTests = makeTests, datasets = datasets)

  # The module, analysis and QML form of the R function, from the Description.qml of its module
  info <- analysisRunInfo(name)

  # Of the defaults analysisOptions() gave, only the options the test set: the form gives the others their default,
  # which can depend on the options that are set (see formOptions.R).
  options <- typesIntoOptionValues(formOptionsAsList(options))

  wrapped <- jaspBaseRunsWrappedAnalyses()

  returnVal <- if (wrapped) {
    args <- list(
      moduleName   = info$moduleName,
      analysisName = info$analysisName,
      qmlFileName  = info$qmlFileName,
      options      = options,
      version      = info$version,
      preloadData  = info$preloadData,
      datasets     = datasets
    )
    runQuietly(jaspBase::runWrappedAnalysis, args, display$quiet)
  } else {
    runAnalysisWithOlderJaspBase(name, info, options, datasets, display$quiet)
  }

  results <- collectRunResults(returnVal, display$view, decode = !wrapped)

  if (makeTests)
    makeUnitTestsFromResults(results, name, dataset, options)

  return(invisible(results))
}

# Quiet hides the output and the warnings of the analysis, but not the warnings of the QML form while it parses the
# options (an unknown option, the upgrade of options of an older module version): a test should not miss those.
runQuietly <- function(fun, args, quiet) {
  if (!quiet)
    return(do.call(fun, args))

  sink(tempfile())
  on.exit(suppressWarnings(sink(NULL)), add = TRUE)
  withCallingHandlers(
    do.call(fun, args),
    warning = function(w) {
      if (!whileParsingOptions(sys.calls()))
        invokeRestart("muffleWarning")
    }
  )
}

# Whether `calls` (the calls being evaluated when a condition was signalled) include the option parse of jaspSyntax.
whileParsingOptions <- function(calls) {
  parseFunctions <- c("loadQmlAndParseOptions", "loadQmlAndParseOptionsStatus")
  any(vapply(calls, function(call) {
    fun <- call[[1L]]
    if (is.call(fun) && identical(fun[[1L]], as.name("::")))
      fun <- fun[[3L]]
    is.name(fun) && as.character(fun) %in% parseFunctions
  }, logical(1L)))
}

# What an analysis run changes, to restore afterwards: the working directory, the language and the state jaspTools kept.
saveRunState <- function() {
  oldWd       <- getwd()
  oldLang     <- Sys.getenv("LANG")
  oldLanguage <- Sys.getenv("LANGUAGE")

  function() {
    .resetRunTimeInternals()
    setwd(oldWd)
    Sys.setenv(LANG = oldLang, LANGUAGE = oldLanguage)
  }
}

# Inside a test nothing is viewed and the analysis runs quietly.
runDisplay <- function(view, quiet) {
  if (insideTestEnvironment())
    return(list(view = FALSE, quiet = TRUE))

  list(view = view, quiet = quiet)
}

# The results of a run as runAnalysis gives them, from what jaspBase returned. jaspBase::runWrappedAnalysis gives the
# results with the plain column names (the analysis only saw encoded ones); after jaspBase::runJaspResults (older
# jaspBase) they are decoded here (`decode`), against every dataset the bridge workspace holds.
collectRunResults <- function(returnVal, view, decode = FALSE) {
  jsonResults <- getJsonResultsFromJaspResults(returnVal)

  if (decode)
    jsonResults <- jaspSyntax::decodeJsonText(jsonResults)

  transferPlotsFromjaspResults()

  results <- processJsonResults(jsonResults)

  if (insideTestEnvironment())
    .setInternal("lastResults", jsonResults)

  if (view)
    view(jsonResults)

  results
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

initAnalysisRuntime <- function(dataset, options, makeTests, datasets = NULL, jaspFile = NULL,
                                reinstall = is.null(jaspFile) || hasModuleDirs(), ...) {
  # first we reinstall any changed modules in the personal library; the analysis of a .jasp file (or an R syntax call)
  # names its module itself, so without module directories the installed module runs it
  if (reinstall)
    reinstallChangedModules()

  # data goes into the bridge workspace as real DataSets (id + column encoder + default
  # filter), before the options are parsed: the QML forms need them to bind columns and to
  # resolve the dataset selection options. Both loaders clear previous state themselves,
  # repeated runs never inherit datasets; without data the workspace is cleared explicitly.
  # The data of a .jasp file is loaded as JASP opens it: with the types, labels and order of
  # the levels the file has, which a data.frame cannot all carry.
  # Several datasets (multiDataSetAware run) are loaded by jaspBase::runWrappedAnalysis itself: they are only
  # checked and read here.
  if (!is.null(jaspFile)) {
    jaspSyntax::clearDatasetState()
    invisible(jaspSyntax::loadDataSetFromJaspFile(jaspFile))
  } else if (!is.null(datasets)) {
    if (!is.list(datasets) || is.null(names(datasets)) || any(!nzchar(names(datasets))))
      stop("`datasets` must be a named list of dataframes - the names become the dataset titles")
    datasets <- lapply(datasets, loadCorrectDataset)
  } else if (!is.null(dataset)) {
    invisible(jaspSyntax::loadDataSet(loadCorrectDataset(dataset)))
  } else {
    jaspSyntax::clearDatasetState()
  }

  # prevent the results from being translated (unless the user explicitly wants to)
  Sys.setenv(LANG = getPkgOption("language"))
  Sys.setenv(LANGUAGE = getPkgOption("language"))

  initializeCoreJaspPackages()

  # ensure that unit tests results are consistent
  if (makeTests)
    set.seed(1)

  invisible(datasets)
}

reinstallChangedModules <- function() {
  modulePaths <- getModulePaths()
  if (isFALSE(getPkgOption("reinstall.modules")) || length(modulePaths) == 0)
    return()

  md5Sums <- .getInternal("modulesMd5Sums")
  for (modulePath in modulePaths) {

    if (isBinaryPackage(modulePath))
      next

    srcFiles <- moduleSourceFiles(modulePath)
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

# The files of a source module whose changes need a reinstall. The QML files count too: jaspBase::runWrappedAnalysis
# reads the form of the installed module.
moduleSourceFiles <- function(modulePath) {
  c(
    list.files(modulePath,                    full.names = TRUE, pattern = "(NAMESPACE|DESCRIPTION)$"),
    list.files(file.path(modulePath, "src"),  full.names = TRUE, pattern = "(\\.(cpp|c|hpp|h)|(Makevars|Makevars\\.win))$"),
    list.files(file.path(modulePath, "R"),    full.names = TRUE, pattern = "\\.R$"),
    list.files(file.path(modulePath, "inst"), full.names = TRUE, pattern = "\\.qml$", recursive = TRUE)
  )
}

initializeCoreJaspPackages <- function() {
  require(jaspBase)
}

# jaspBase 0.20.6 gives the results of a run in R with the plain column names, and runs the analysis of a module without
# R wrappers in runWrappedAnalysis. The module tests may still pin an older jaspBase in their renv.lock: runAnalysis()
# then runs the analysis as jaspTools did before (runAnalysisWithOlderJaspBase).
minimumJaspBaseForWrappedRuns <- "0.20.6"

jaspBaseRunsWrappedAnalyses <- function(version = utils::packageVersion("jaspBase")) {
  version >= minimumJaspBaseForWrappedRuns
}

# runAnalysisFromJaspFile() has no other way to run an analysis than jaspBase::runWrappedAnalysis.
checkJaspBaseVersion <- function(what = "runAnalysisFromJaspFile()", version = utils::packageVersion("jaspBase")) {
  if (!jaspBaseRunsWrappedAnalyses(version))
    stop(what, " needs jaspBase ", minimumJaspBaseForWrappedRuns, " or later (installed: ", version, ")", call. = FALSE)

  invisible(TRUE)
}

# runAnalysis() with a jaspBase before 0.20.6, as jaspTools 0.20 ran an analysis: the options go through the QML form
# (jaspSyntax::loadQmlAndParseOptionsStatus), the analysis is run by jaspBase::runJaspResults, and collectRunResults()
# decodes the results. The datasets of a multiDataSetAware run are loaded here, runWrappedAnalysis loads them itself.
runAnalysisWithOlderJaspBase <- function(name, info, options, datasets, quiet) {
  if (!is.null(datasets))
    invisible(jaspSyntax::loadDataSets(datasets))

  # Same serialization jaspBase uses for the syntax wrapper (common.R toJSON): auto_unbox so scalar options arrive as
  # scalars - boxed one-element arrays are rejected by the bridge.
  optionsJson <- as.character(jsonlite::toJSON(options, auto_unbox = TRUE, digits = NA, null = "null"))

  status <- jaspSyntax::loadQmlAndParseOptionsStatus(
    info$moduleName, info$analysisName, info$qmlFile, optionsJson, info$version, info$preloadData)

  possibleArgs <- list(
    name         = name,
    functionCall = findCorrectFunction(name),
    title        = "",
    requiresInit = TRUE,
    options      = status$options,
    dataKey      = "null",
    resultsMeta  = "null",
    stateKey     = "null",
    preloadData  = info$preloadData
  )

  # For multiDataSetAware runs: the blob the bridge built while parsing ({ids = filter keys, names, dataSetIds,
  # primary}) - jaspBase::runJaspResults then pulls the queued slices from the bridge.
  if (nzchar(status$multiDataSetJson))
    possibleArgs$multiDataSetJson <- status$multiDataSetJson

  args <- possibleArgs[intersect(names(possibleArgs), names(formals(jaspBase::runJaspResults)))]

  runQuietly(jaspBase::runJaspResults, args, quiet)
}

processJsonResults <- function(jsonResults) {
  if (jsonlite::validate(jsonResults))
    results <- jsonlite::fromJSON(jsonResults, simplifyVector=FALSE)
  else
    stop("Could not process json result from jaspResults")

  results[["state"]] <- .getInternal("state")

  figures <- results$state$figures
  if (length(figures) > 1 && !is.null(names(figures)))
    results$state$figures <- figures[orderFiguresByCreation(names(figures))]

  return(results)
}

# jaspBase collects the plots for the state from a std::set of pointers, so their order there
# depends on memory addresses and differs between platforms. Their file names carry a counter
# that only goes up, so sorting on it gives back the order the plots were made in, which is
# what tests indexing `state$figures[[i]]` rely on:
#   - old jaspResults names:          "1.png", "2.png", ...
#   - JASP/jaspSyntax (TempFiles):    "resources/_<counter>_t<millis>.png", optionally with
#                                     "resources/<id>/" in front
orderFiguresByCreation <- function(figureNames) {
  base    <- tools::file_path_sans_ext(basename(figureNames))
  counter <- suppressWarnings(as.numeric(ifelse(
    grepl("^_[0-9]+_t[0-9]+$", base),
    sub("^_([0-9]+)_t[0-9]+$", "\\1", base),
    base
  )))
  order(counter)
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

.resetRunTimeInternals <- function() {
  .setInternal("state", list())
}
