#' Obtain options to run JASP analyses with.
#'
#' \code{analysisOptions} provides an easy way to create analysis options that can be supplied to \code{runAnalysis}.
#'
#' @param source One of three: (1) R function name, (2) path to .jasp file or (3) json string. See the details section for more information.
#'
#' @details
#' There are three types of allowed input. 1) The name of the R function of the analysis (case-sensitive); jaspTools loads the QML form of that analysis through jaspSyntax, as JASP does, and returns its default options.
#' 2) the path to .jasp file that has one or more analyses. Or (3) a json string that was sent by the JASP application. This json can be obtained by having JASP log to file (JASP>Preferences>Advanced>Log to file).
#' The logs can be found by clicking 'Show logs" in the "Logging options". Click on the file "*Engine*.log" that has "Engine::receiveAnalysisMessage:" (usually Engine 1), copy the content between the \{ and \}.
#' Be sure to use single quotes (') when supplying this string.
#'
#' @return A list containing options of the analysis. If \code{source} is a .jasp file with multiple analyses, then a list of lists.
#' If \code{source} is the name of the R function of the analysis then it holds every option of the form,
#' also those of the QML files it imports, with the default value JASP gives it.
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
analysisOptions <- function(source) {
  if (! is.character(source) || length(source) > 1)
    stop("Expecting a character input of length 1 as source")

  source <- trimws(source)

  # Normalize path separators for cross-platform compatibility
  normalizedSource <- normalizePath(source, winslash = "/", mustWork = FALSE)

  # First check if it's a .jasp file path (before JSON check, since Windows paths contain ':')
  if (grepl("\\.jasp$", normalizedSource, ignore.case = TRUE) && file.exists(normalizedSource)) {
    options <- analysisOptionsFromJASPFile(normalizedSource)
  } else if (grepl("[{}\":]", source)) { # json string
    if (!grepl("^\\{.*\\}$", source))
      stop("Your json is invalid, please copy the entire message
           including the outer braces { } that was send to R in the Qt terminal.
           Remember to use single quotes around the message.")

    options <- analysisOptionsFromJSONString(source)
  } else if (file.exists(source)) { # other file types
    if (!endsWith(source, ".jasp"))
      stop("The file you provided exists, but it is not a .jasp file")

    options <- analysisOptionsFromJASPFile(source)
  } else { # analysis name
    options <- analysisOptionsFromQMLFile(source)
  }

  return(options)
}

# The defaults come from the real QML form, loaded through the bridge (the same path runAnalysis
# validates options with), so every control is included, also those of QML files the form imports.
# .meta and the *.types options are left out: tests set e.g. options$dependent <- "contNormal" and
# a default dependent.types would no longer match it. Empty lists are left out as well: for the bridge
# a missing option is the same as its empty default, and it then fills the lists that follow other
# controls (e.g. one contrasts row per factor in options$fixedFactors) instead of keeping them empty.
analysisOptionsFromQMLFile <- function(analysis) {
  info <- analysisRunInfo(analysis)

  options <- jaspSyntax::parseQmlOptions(
    qmlFile            = info$qmlFile,
    moduleName         = info$moduleName,
    analysisName       = info$analysisName,
    version            = info$version,
    preloadData        = info$preloadData,
    includeMeta        = FALSE,
    includeTypeOptions = FALSE,
    isolated           = FALSE
  )
  options <- dropEmptyListOptions(options)
  attr(options, "analysisName") <- analysis

  # runAnalysis gives the form only the options a test sets: see formOptions.R
  return(formDefaultOptions(options))
}

dropEmptyListOptions <- function(options) {
  options[!vapply(options, function(x) is.list(x) && length(x) == 0L, logical(1L))]
}

# Deprecated, kept for the module test helpers that still call jaspTools:::readQML(). A form file is
# loaded through the bridge like in analysisOptions(); a QML file a form imports cannot be loaded on
# its own and gives no options, as analysisOptions() already includes them.
readQML <- function(file) {
  lifecycle::deprecate_warn("0.20.1", "readQML()", "analysisOptions()", env = asNamespace("jaspTools"))

  options <- tryCatch(
    jaspSyntax::parseQmlOptions(file, includeMeta = FALSE, includeTypeOptions = FALSE, isolated = FALSE),
    error = function(e) {
      if (!grepl("Cannot create QML Form", conditionMessage(e), fixed = TRUE))
        stop(e)
      list()
    }
  )

  return(dropEmptyListOptions(options))
}

analysisOptionsFromJSONString <- function(x) {
  json <- try(rjson::fromJSON(x)) # jsonlite can't deal with \n in strings.. rjson can.
  if (inherits(json, "try-error"))
    stop("There was a problem parsing the JSON string, cannot create the options list")

  if (!"options" %in% names(json))
    stop("There is no \"options\" field in your JSON string, cannot create options list")

  options <- json[["options"]]
  if (!is.null(names(options)) && ".meta" %in% names(options))
    options[[".meta"]] <- NULL

  if ("name" %in% names(json))
    attr(options, "analysisName") <- json[["name"]]

  return(options)
}

# Read through jaspSyntax, like the dataset (extractDatasetFromJASPFile): the variables options are
# split into `x` and `x.types` the way JASP does it before it calls an analysis that does not preload
# its data, with the plain column names (see ?jaspSyntax::readAnalysisOptionsFromJaspFile).
analysisOptionsFromJASPFile <- function(file) {
  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(file, flattenTypes = TRUE)

  options <- lapply(unname(records), function(record) {
    options <- record[["options"]]
    attributes(options) <- list(names = names(options), analysisName = record[["name"]])
    options
  })

  if (length(options) == 1)
    options <- options[[1]]

  return(options)
}
