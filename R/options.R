#' Obtain options to run JASP analyses with
#'
#' \code{analysisOptions} provides an easy way to create analysis options that can be supplied to \code{runAnalysis}.
#'
#' @param source One of three: (1) R function name, (2) path to .jasp file or (3) json string. See the details section for more information.
#' @param modulePath Optional module path, or named list/vector of module paths
#'   keyed by module name or analysis name. When omitted, jaspTools infers the
#'   module from configured module paths or the active module context. Pass this
#'   only to pin a source checkout for .jasp replay, generated tests, or
#'   ambiguous multi-module archives.
#'
#' @details
#' There are three types of allowed input. 1) The name of the R function of the analysis (case-sensitive); jaspTools will attempt to read the .qml file for that analysis and create a set of default options.
#' 2) the path to .jasp file that has one or more analyses. For .jasp files,
#' saved QML-bound options are returned so they can be supplied to
#' \code{runAnalysis()} and replayed once through the native JASP option
#' pipeline. Use \code{analysisRuntimeOptions()} only when you need
#' backend-prepared runtime options for diagnostics. Or (3) a json string that was sent by the
#' JASP application. This json can be obtained by having JASP log to file
#' (JASP>Preferences>Advanced>Log to file).
#' The logs can be found by clicking 'Show logs" in the "Logging options". Click on the file "*Engine*.log" that has "Engine::receiveAnalysisMessage:" (usually Engine 1), copy the content between the \{ and \}.
#' Be sure to use single quotes (') when supplying this string.
#'
#' @return A list containing options of the analysis. If \code{source} is a .jasp file with multiple analyses, then a list of lists.
#' If \code{source} is the name of the R function of the analysis then all default options have been
#' filled in and booleans set to FALSE. The options that have no default are
#' left empty.
#' @examples
#' jaspOptions <- analysisOptions("~/Documents/someFile.jasp")
#' # if there are multiple analyses in the .jasp files you need to select one
#' options <- jaspOptions[[1]]
#'
#' options <- analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' options <- analysisOptions('{
#'"dynamicModuleCall" : "",
#'"id" : 1,
#'"jaspResults" : true,
#'"name" : "BinomialTest",
#'"options" : {
#'  ".meta" : {
#'    "variables" : {
#'      "containsColumn" : true
#'    }
#'  },
#'  "VovkSellkeMPR" : false,
#'  "confidenceInterval" : false,
#'  "confidenceIntervalInterval" : 0.950,
#'  "descriptivesPlots" : false,
#'  "descriptivesPlotsConfidenceInterval" : 0.950,
#'  "hypothesis" : "notEqualToTestValue",
#'  "plotHeight" : 320,
#'  "plotWidth" : 480,
#'  "testValue" : "0.5",
#'  "variables" : [ "contBinom" ]
#'},
#'"perform" : "run",
#'"revision" : 0,
#'"rfile" : "",
#'"title" : "Binomial Test",
#'"typeRequest" : "analysis"
#' }')
#'
#' @export analysisOptions
analysisOptions <- function(source, modulePath = NULL) {
  if (! is.character(source) || length(source) > 1)
    stop("Expecting a character input of length 1 as source")

  source <- trimws(source)

  # Normalize path separators for cross-platform compatibility.
  normalizedSource <- normalizePath(source, winslash = "/", mustWork = FALSE)
  isJaspFilePath <- grepl("\\.jasp$", normalizedSource, ignore.case = TRUE)

  # First check .jasp file paths before JSON, since Windows paths contain ':'.
  if (isJaspFilePath) {
    if (!file.exists(normalizedSource))
      stop("File not found: ", normalizedSource)

    options <- analysisOptionsFromJASPFile(normalizedSource, modulePath = modulePath)
  } else if (grepl("[{}]", source)) { # json string
    if (!grepl("^\\{.*\\}$", source))
      stop("Your json is invalid, please copy the entire message
           including the outer braces { } that was send to R in the Qt terminal.
           Remember to use single quotes around the message.")

    options <- analysisOptionsFromJSONString(source)
  } else if (file.exists(source)) { # other file types
    if (!endsWith(source, ".jasp"))
      stop("The file you provided exists, but it is not a .jasp file")

    options <- analysisOptionsFromJASPFile(source, modulePath = modulePath)
  } else if (.looksLikeMissingFilePath(source)) {
    stop("File not found: ", normalizedSource)
  } else { # analysis name
    options <- analysisOptionsFromQMLFile(source, modulePath = modulePath)
  }

  return(options)
}

analysisOptionsFromQMLFile <- function(analysis, modulePath = NULL) {
  modulePath <- .modulePathForAnalysisName(analysis, modulePath)
  options <- jaspSyntax::readDefaultAnalysisOptions(
    modulePath = modulePath,
    analysisName = analysis,
    includeMeta = FALSE,
    includeTypeOptions = FALSE
  )
  attr(options, "analysisName") <- analysis
  attr(options, "jaspTools.optionShape") <- "qml"
  attr(options, "modulePath") <- normalizePath(modulePath, winslash = "/", mustWork = FALSE)

  return(options)
}

.looksLikeMissingFilePath <- function(source) {
  hasPathSeparator <- grepl("[/\\\\]", source)
  hasDrivePrefix <- grepl("^[A-Za-z]:", source)
  hasFileExtension <- nzchar(tools::file_ext(source))

  hasPathSeparator || hasDrivePrefix || hasFileExtension
}

analysisOptionsFromJSONString <- function(x) {
  json <- try(rjson::fromJSON(x)) # jsonlite can't deal with \n in strings.. rjson can.
  if (inherits(json, "try-error"))
    stop("There was a problem parsing the JSON string, cannot create the options list")

  if (!"options" %in% names(json))
    stop("There is no \"options\" field in your JSON string, cannot create options list")

  options <- json[["options"]]
  if ("name" %in% names(json))
    attr(options, "analysisName") <- json[["name"]]

  return(options)
}

analysisOptionsFromJASPFile <- function(file, modulePath = NULL) {
  records <- .jaspSyntaxReadAnalysisOptionsFromJaspFile(
    file,
    modulePath = modulePath,
    runtime = FALSE,
    includeMeta = TRUE,
    includeTypeOptions = TRUE,
    isolated = TRUE
  )
  options <- .optionsFromJaspRecords(
    records,
    preparedOptions = FALSE,
    optionShape = "saved",
    modulePath = modulePath
  )

  if (length(options) == 1L)
    options <- options[[1L]]

  return(options)
}

#' Obtain backend/runtime options from a JASP file.
#'
#' \code{analysisRuntimeOptions()} reads options from a saved .jasp file,
#' replays them through \code{jaspSyntax} and the native JASP option pipeline,
#' and marks the returned option lists as already prepared. These options are
#' intended for inspection and direct backend diagnostics, not for
#' \code{runAnalysis()}.
#'
#' @param file Path to a .jasp file.
#' @param modulePath Optional module path, or a named list/vector of module
#'   paths keyed by module name or analysis name. Passed to
#'   \code{jaspSyntax::readAnalysisOptionsFromJaspFile()}.
#'
#' @return A prepared options list. If \code{file} contains multiple analyses,
#'   a list of prepared options lists is returned.
#'
#' @export analysisRuntimeOptions
analysisRuntimeOptions <- function(file, modulePath = NULL) {
  if (!is.character(file) || length(file) != 1L)
    stop("Expecting a character input of length 1 as file")

  file <- normalizePath(trimws(file), winslash = "/", mustWork = FALSE)
  if (!file.exists(file))
    stop("File not found: ", file)
  if (!grepl("\\.jasp$", file, ignore.case = TRUE))
    stop("File must have a .jasp extension")

  records <- .jaspSyntaxReadAnalysisOptionsFromJaspFile(
    file,
    modulePath = modulePath,
    runtime = TRUE,
    includeMeta = FALSE,
    includeTypeOptions = TRUE,
    isolated = TRUE
  )
  options <- .optionsFromJaspRecords(
    records,
    preparedOptions = TRUE,
    optionShape = "runtime",
    modulePath = modulePath
  )

  if (length(options) == 1L)
    options <- options[[1L]]

  return(options)
}

.optionsFromJaspRecords <- function(records, preparedOptions, optionShape,
                                    modulePath = NULL) {
  options <- lapply(records, function(record) {
    opts <- record[["options"]]

    if (!is.null(record[["name"]]))
      attr(opts, "analysisName") <- record[["name"]]
    if (!is.null(record[["moduleName"]]))
      attr(opts, "moduleName") <- record[["moduleName"]]
    if (!is.null(record[["moduleVersion"]]))
      attr(opts, "moduleVersion") <- record[["moduleVersion"]]
    attr(opts, "jaspTools.optionShape") <- optionShape

    resolvedModulePath <- .modulePathForOptionRecord(record, modulePath)
    if (!is.null(resolvedModulePath))
      attr(opts, "modulePath") <- resolvedModulePath

    if (preparedOptions)
      opts <- markPreparedOptions(opts)

    opts
  })

  if (!is.null(names(records)) && length(names(records)) == length(options))
    names(options) <- names(records)

  options
}

.modulePathForOptionRecord <- function(record, modulePath = NULL) {
  if (is.null(modulePath))
    return(NULL)

  expectedNames <- c(record[["moduleName"]], record[["name"]])
  expectedNames <- expectedNames[!is.na(expectedNames) & nzchar(expectedNames)]

  .normalizeSingleModulePath(
    modulePath,
    expectedNames = expectedNames,
    context = paste0(
      "analysis `", .recordLabel(record, "name"),
      "` in module `", .recordLabel(record, "moduleName"), "`"
    )
  )
}

.recordLabel <- function(record, field) {
  value <- record[[field]]
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]]) || !nzchar(value[[1L]]))
    return("<unknown>")

  as.character(value[[1L]])
}

.normalizeSingleModulePath <- function(modulePath, expectedNames = character(0),
                                        context = "`modulePath`") {
  pathNames <- names(modulePath)

  if (.hasUsableNames(modulePath) && length(expectedNames) > 0L) {
    matchIndex <- match(expectedNames, pathNames, nomatch = 0L)
    matchIndex <- matchIndex[matchIndex > 0L]
    if (length(matchIndex) > 0L)
      return(.normalizeModulePathValue(modulePath, matchIndex[[1L]]))

    usableNames <- pathNames[!is.na(pathNames) & nzchar(pathNames)]
    stop(
      "`modulePath` names (", paste(usableNames, collapse = ", "),
      ") do not match ", context, ".",
      call. = FALSE
    )
  }

  if (length(modulePath) != 1L) {
    stop(
      "`modulePath` must be a single path or a named collection that matches ",
      context, ".",
      call. = FALSE
    )
  }

  .normalizeModulePathValue(modulePath, 1L)
}

.normalizeModulePathValue <- function(modulePath, index) {
  if (is.list(modulePath))
    modulePath <- modulePath[[index]]
  else
    modulePath <- modulePath[[index]]

  if (!is.character(modulePath) || length(modulePath) != 1L ||
      is.na(modulePath) || !nzchar(modulePath)) {
    stop("`modulePath` must contain non-empty string paths.", call. = FALSE)
  }

  normalizePath(modulePath, winslash = "/", mustWork = FALSE)
}

.hasUsableNames <- function(x) {
  nms <- names(x)
  !is.null(nms) && any(nzchar(nms))
}

.modulePathForAnalysisName <- function(analysis, modulePath = NULL) {
  if (is.null(modulePath))
    return(getModulePathFromRFunction(analysis))

  pathNames <- names(modulePath)
  if (.hasUsableNames(modulePath)) {
    matchIndex <- match(analysis, pathNames, nomatch = 0L)
    if (matchIndex > 0L)
      return(.normalizeModulePathValue(modulePath, matchIndex))
  }

  candidatePaths <- .normalizeModulePathCollection(modulePath)
  matchesAnalysis <- vapply(
    candidatePaths,
    .modulePathContainsAnalysis,
    logical(1L),
    analysis = analysis
  )

  if (sum(matchesAnalysis) == 1L)
    return(candidatePaths[matchesAnalysis][[1L]])

  if (sum(matchesAnalysis) > 1L) {
    stop(
      "`modulePath` is ambiguous for analysis `", analysis,
      "`; more than one supplied module path contains that analysis.",
      call. = FALSE
    )
  }

  .normalizeSingleModulePath(
    modulePath,
    expectedNames = analysis,
    context = paste0("analysis `", analysis, "`")
  )
}

.normalizeModulePathCollection <- function(modulePath) {
  if (is.list(modulePath)) {
    paths <- vapply(modulePath, function(path) {
      if (!is.character(path) || length(path) != 1L ||
          is.na(path) || !nzchar(path)) {
        stop("`modulePath` must contain non-empty string paths.", call. = FALSE)
      }

      normalizePath(path, winslash = "/", mustWork = FALSE)
    }, character(1L))
  } else {
    if (!is.character(modulePath) || any(is.na(modulePath)) || any(!nzchar(modulePath))) {
      stop("`modulePath` must contain non-empty string paths.", call. = FALSE)
    }

    paths <- normalizePath(modulePath, winslash = "/", mustWork = FALSE)
  }

  names(paths) <- names(modulePath)
  paths
}

.modulePathContainsAnalysis <- function(modulePath, analysis) {
  isTRUE(tryCatch({
    jaspSyntax::resolveAnalysisQml(modulePath, analysis)
    TRUE
  }, error = function(e) FALSE)) ||
    isTRUE(tryCatch(rFunctionExistsInModule(analysis, modulePath), error = function(e) FALSE))
}

.jaspSyntaxReadAnalysisOptionsFromJaspFile <- function(file, modulePath = NULL,
                                                       runtime = FALSE,
                                                       includeMeta = TRUE,
                                                       includeTypeOptions = TRUE,
                                                       isolated = TRUE) {
  if (is.null(modulePath)) {
    modulePath <- .jaspSyntaxRuntimeModulePaths()
  }

  .jaspSyntaxReadAnalysisOptionsNative(
    file = file,
    modulePath = modulePath,
    runtime = runtime,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions,
    isolated = isolated
  )
}

.jaspSyntaxReadAnalysisOptionsNative <- function(file, modulePath, runtime,
                                                 includeMeta,
                                                 includeTypeOptions,
                                                 isolated) {
  jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFilePath = file,
    modulePath = modulePath,
    runtime = runtime,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions,
    isolated = isolated
  )
}

.jaspSyntaxRuntimeModulePaths <- function() {
  modulePaths <- tryCatch(getModulePaths(), error = function(e) character(0))
  if (length(modulePaths) == 0L) {
    return(NULL)
  }

  .jaspSyntaxNamedModulePaths(modulePaths)
}

.jaspSyntaxNamedModulePaths <- function(modulePaths) {
  moduleNames <- vapply(
    modulePaths,
    function(modulePath) tryCatch(getModuleName(modulePath), error = function(e) ""),
    character(1L)
  )

  keep <- nzchar(moduleNames)
  if (!any(keep)) {
    return(NULL)
  }

  modulePaths <- normalizePath(modulePaths[keep], winslash = "/", mustWork = FALSE)
  stats::setNames(as.list(modulePaths), moduleNames[keep])
}
