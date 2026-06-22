#' Run a JASP analysis in R
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
#' @param quiet Boolean indicating whether to suppress raw JASP/native output
#' by default.
#' @param verbose Controls which output streams are requested. Use \code{"all"}
#' or \code{TRUE} for both analysis and JASP/native output, \code{"analysis"}
#' for R analysis messages and warnings only, \code{"jasp"} for JASP/native
#' output only, and \code{"none"} or \code{FALSE} for no output. When
#' omitted, \code{getOption("jaspSyntax.verbose")} is honored first, then quiet
#' runs default to \code{"analysis"} and non-quiet runs default to \code{"all"}.
#' @param makeTests Boolean indicating whether to create testthat unit tests and print them to the terminal.
#' @param modulePath Optional path to the module checkout that should be used
#'   for QML resolution and wrapped execution. When omitted, jaspTools first
#'   uses a module path attached to \code{options} by \code{analysisOptions()}
#'   and then falls back to configured \code{module.dirs}.
#' @details
#' Saved/QML-bound options are replayed through the native QML runtime path. Use
#' \code{analysisOptions()} for options that should be passed to
#' \code{runAnalysis()}; options returned by \code{analysisRuntimeOptions()} are
#' backend-prepared diagnostics and are not accepted by this runner.
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
runAnalysis <- function(name, dataset = NULL, options, view = TRUE, quiet = TRUE,
                        makeTests = FALSE, modulePath = NULL,
                        verbose = getOption("jaspTools.runAnalysis.verbose", getOption("jaspSyntax.verbose", NULL))) {
  if (is.list(options) && is.null(names(options)) && any(names(unlist(lapply(options, attributes))) == "analysisName"))
    stop("The provided list of options is not named. Did you mean to index in the options list (e.g., options[[1]])?")

  if (!is.list(options) || is.null(names(options)))
    stop("The options should be a named list (you can obtain it through `analysisOptions()`")

  if (isPreparedOptions(options)) {
    stop(
      "`runAnalysis()` expects saved/QML-bound options. ",
      "`analysisRuntimeOptions()` returns backend-prepared options for inspection only; ",
      "use `analysisOptions()` to obtain runnable options."
    )
  }

  if (missing(name)) {
    name <- attr(options, "analysisName")
    if (is.null(name))
      stop("Please supply an analysis name")
  }

  if (insideTestEnvironment()) {
    view  <- FALSE
    quiet <- TRUE
  }

  verbose <- normalizeRunAnalysisVerbose(verbose, quiet = quiet)

  args <- fetchRunArgs(name, options, modulePath = modulePath)
  modulePath <- attr(args, "modulePath", exact = TRUE)
  runner <- attr(args, "runner", exact = TRUE)
  attr(args, "runner") <- NULL
  attr(args, "modulePath") <- NULL
  if ("quiet" %in% names(formals(runner)))
    args$quiet <- quiet
  if ("verbose" %in% names(formals(runner)))
    args$verbose <- verbose

  oldWd       <- getwd()
  oldLang     <- Sys.getenv("LANG")
  oldLanguage <- Sys.getenv("LANGUAGE")

  rbridgeState <- .snapshotRbridgeEnv(.GlobalEnv)
  on.exit({
    .resetRunTimeInternals()
    .restoreRbridgeEnv(.GlobalEnv, rbridgeState)
    setwd(oldWd)
    Sys.setenv(LANG = oldLang)
    Sys.setenv(LANGUAGE = oldLanguage)
  }, add = TRUE)

  initAnalysisRuntime(
    dataset = dataset,
    options = options,
    modulePath = modulePath,
    analysisName = name,
    makeTests = makeTests
  )

  if (quiet && !runAnalysisShowsJaspOutput(verbose)) {
    sink(tempfile())
    on.exit({suppressWarnings(sink(NULL))}, add = TRUE)
    returnVal <- do.call(runner, args)
    sink(NULL)
  } else {
    returnVal <- do.call(runner, args)
  }

  # always TRUE after jaspResults is merged into jaspBase
  jsonResults <- if (inherits(returnVal, c("jaspResultsR", "R6"))) {
    getJsonResultsFromJaspResults(returnVal)
  } else {
    getJsonResultsFromJaspResultsLegacy()
  }
  storeRawLastResults(jsonResults)

  transferPlotsFromjaspResults()

  results <- processJsonResults(jsonResults)

  viewRunAnalysisResults(results, view)

  if (makeTests)
    makeUnitTestsFromResults(results, name, dataset, options)

  return(invisible(results))
}

normalizeRunAnalysisVerbose <- function(verbose = NULL, quiet = NULL) {
  if (is.null(verbose) || length(verbose) == 0L) {
    if (isFALSE(quiet))
      return("all")

    return("analysis")
  }

  verbose <- verbose[[1L]]
  if (is.na(verbose))
    stop("`verbose` must be one of 'all', 'analysis', 'jasp', 'none', TRUE, or FALSE.", call. = FALSE)

  if (is.logical(verbose))
    return(if (isTRUE(verbose)) "all" else "none")

  if (is.character(verbose)) {
    verbose <- tolower(trimws(verbose))
    if (verbose %in% c("true", "yes", "on", "1"))
      return("all")
    if (verbose %in% c("false", "no", "off", "0"))
      return("none")
    if (verbose %in% c("all", "analysis", "jasp", "none"))
      return(verbose)
  }

  stop("`verbose` must be one of 'all', 'analysis', 'jasp', 'none', TRUE, or FALSE.", call. = FALSE)
}

runAnalysisShowsJaspOutput <- function(verbose) {
  verbose %in% c("all", "jasp")
}

viewRunAnalysisResults <- function(results, enabled) {
  if (!isTRUE(enabled))
    return(invisible(NULL))

  get("view", envir = asNamespace("jaspTools"), inherits = FALSE)(results)
}

fetchRunArgs <- function(name, options, modulePath = NULL) {
  fetchWrappedRunArgs(name, options, modulePath = modulePath)
}

fetchWrappedRunArgs <- function(name, options, modulePath = NULL) {
  runner <- .jaspBaseRunWrappedAnalysis()
  .validateRunWrappedAnalysisContract(runner)

  modulePath <- .resolveRunModulePath(name, options, modulePath = modulePath)
  resolved <- .jaspSyntaxResolveAnalysisQml(modulePath, name)
  .validateOptionsMatchResolvedAnalysis(options, resolved)

  possibleArgs <- list(
    moduleName = resolved$moduleName,
    analysisName = resolved$analysisName,
    qmlFileName = resolved$qmlFileName,
    qmlFile = resolved$qmlFile,
    modulePath = modulePath,
    options = options,
    version = resolved$version,
    preloadData = resolved$preloadData
  )

  runArgs <- formals(runner)
  argNames <- intersect(names(possibleArgs), names(runArgs))
  args <- possibleArgs[argNames]
  attr(args, "runner") <- runner
  attr(args, "modulePath") <- modulePath
  return(args)
}

.jaspBaseRunWrappedAnalysis <- function() {
  if (!exists("runWrappedAnalysis", envir = asNamespace("jaspBase"), inherits = FALSE)) {
    stop(
      "Installed jaspBase does not provide `runWrappedAnalysis()`. ",
      "Update jaspBase so jaspTools can use the native QML/options runtime path."
    )
  }

  get("runWrappedAnalysis", envir = asNamespace("jaspBase"), inherits = FALSE)
}

.validateRunWrappedAnalysisContract <- function(runner) {
  requiredArgs <- c(
    "moduleName",
    "analysisName",
    "qmlFileName",
    "options",
    "version",
    "preloadData",
    "modulePath",
    "qmlFile"
  )
  missingArgs <- setdiff(requiredArgs, names(formals(runner)))

  if (length(missingArgs) > 0L) {
    stop(
      "Installed jaspBase::runWrappedAnalysis() does not support source-module ",
      "replay arguments: ", paste(missingArgs, collapse = ", "), ". ",
      "Install jaspBase >= 0.20.5 from the matching jasp-desktop checkout so ",
      "jaspTools can pass resolved QML and module provenance.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.resolveRunModulePath <- function(name, options, modulePath = NULL) {
  if (!is.null(modulePath))
    return(.normalizeRunModulePath(modulePath))

  optionModulePath <- attr(options, "modulePath", exact = TRUE)
  if (!is.null(optionModulePath))
    return(.normalizeRunModulePath(optionModulePath))

  getModulePathFromRFunction(name)
}

.normalizeRunModulePath <- function(modulePath) {
  if (!is.character(modulePath) || length(modulePath) != 1L ||
      is.na(modulePath) || !nzchar(modulePath)) {
    stop("`modulePath` must be a single non-empty string.", call. = FALSE)
  }

  normalizePath(modulePath, winslash = "/", mustWork = FALSE)
}

.validateOptionsMatchResolvedAnalysis <- function(options, resolved) {
  optionAnalysisName <- .scalarOptionAttribute(options, "analysisName")
  if (!is.null(optionAnalysisName) &&
      !identical(optionAnalysisName, as.character(resolved$analysisName))) {
    stop(
      "`options` are tagged for analysis `", optionAnalysisName,
      "`, but `runAnalysis()` is running `", resolved$analysisName, "`."
    )
  }

  optionModuleName <- .scalarOptionAttribute(options, "moduleName")
  if (!is.null(optionModuleName) &&
      !identical(optionModuleName, as.character(resolved$moduleName))) {
    stop(
      "`options` are tagged for module `", optionModuleName,
      "`, but `runAnalysis()` resolved module `", resolved$moduleName, "`."
    )
  }

  invisible(TRUE)
}

.scalarOptionAttribute <- function(options, name) {
  value <- attr(options, name, exact = TRUE)
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]]) || !nzchar(value[[1L]]))
    return(NULL)

  as.character(value[[1L]])
}

.jaspSyntaxResolveAnalysisQml <- function(modulePath, analysisName) {
  if (!exists("resolveAnalysisQml", envir = asNamespace("jaspSyntax"), inherits = FALSE)) {
    stop(
      "Installed jaspSyntax does not provide `resolveAnalysisQml()`. ",
      "Install the jaspSyntax build that exposes the native QML parser API."
    )
  }

  jaspSyntax::resolveAnalysisQml(modulePath, analysisName)
}

initAnalysisRuntime <- function(dataset, options, makeTests, modulePath = NULL,
                                analysisName = NULL, ...) {
  # first we reinstall any changed modules in the personal library
  reinstallChangedModules()

  # dataset to be found in the analysis when it needs to be read
  .setInternal("dataset", dataset)
  .resetRunStateFile()
  preloadDataset(
    dataset,
    options,
    modulePath = modulePath,
    analysisName = analysisName
  )
  .insertRbridgeIntoEnv(.GlobalEnv)

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

  results <- .jaspSyntaxDecodeAnalysisResults(results)

  results[["state"]] <- .readRunState()

  figures <- results$state$figures
  if (length(figures) > 1 && !is.null(names(figures)))
    results$state$figures <- figures[order(as.numeric(tools::file_path_sans_ext(basename(names(figures)))))]

  return(results)
}

.readRunState <- function() {
  fileState <- .readRunStateFile()
  if (!is.null(fileState))
    return(fileState)

  .getInternal("state")
}

.readRunStateFile <- function() {
  location <- tryCatch(
    .requestStateFileNameNative(),
    error = function(e) NULL
  )
  if (!is.list(location) || is.null(location$root) || is.null(location$relativePath))
    return(NULL)

  stateFile <- file.path(location$root, location$relativePath)
  if (!file.exists(stateFile))
    return(NULL)

  state <- NULL
  loaded <- tryCatch(
    load(stateFile),
    error = function(e) character(0)
  )
  if (!"state" %in% loaded)
    return(NULL)

  state
}

.jaspSyntaxDecodeAnalysisResults <- function(results) {
  requestedDataset <- tryCatch(
    .getInternal("preloadedDataset"),
    error = function(e) NULL
  )
  args <- list(results = results)
  if (is.data.frame(requestedDataset) && ncol(requestedDataset) > 0L)
    args$requestedDataset <- requestedDataset

  columnEncoderContext <- tryCatch(
    .getInternal("preloadedColumnEncoderContext"),
    error = function(e) NULL
  )
  if (!is.null(columnEncoderContext))
    args$columnEncoderContext <- columnEncoderContext

  decoded <- .jaspSyntaxCall(
    "decodeAnalysisResults",
    args = args,
    required = TRUE,
    feature = "native analysis result decoding API",
    requiredArgs = "results"
  )

  if (is.null(decoded))
    return(results)

  decoded
}

storeRawLastResults <- function(jsonResults) {
  if (is.character(jsonResults) && length(jsonResults) == 1L)
    .setInternal("lastResults", jsonResults)

  invisible(jsonResults)
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
  .jaspSyntaxClearNativeState(required = FALSE)
  .resetRunStateFile()
  .setInternal("state", list())
  .setInternal("dataset", "")
  .setInternal("preloadedDataset", data.frame())
  .setInternal("preloadedColumnEncoderContext", NULL)
}
