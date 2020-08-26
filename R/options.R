#' Obtain options to run JASP analyses with.
#'
#' \code{analysisOptions} provides an easy way to create analysis options that can be supplied to \code{runAnalysis}.
#'
#' @param source One of three: (1) R function name, (2) path to .jasp file or (3) json string. See the details section for more information.
#'
#' @details
#' There are three types of allowed input. 1) The name of the R function of the analysis (case-sensitive); jaspTools will attempt to read the .qml file for that analysis and create a set of default options.
#' 2) the path to .jasp file that has one or more analyses. Or (3) a json string that was sent by the JASP application. This json can be obtained by having JASP log to file (JASP>Preferences>Advanced>Log to file).
#' The logs can be found by clicking 'Show logs" in the "Logging options". Click on the file "*Engine*.log" that has "Engine::receiveAnalysisMessage:" (usually Engine 1), copy the content between the \{ and \}.
#' Be sure to use single quotes (') when supplying this string.
#'
#' @return A list containing options of the analysis. If \code{source} is a .jasp file with multiple analyses, then a list of lists.
#' If \code{source} is the name of the R function of the analysis then all default options have been
#' filled in and booleans set to FALSE. The options that have no default are
#' left empty.
#' @examples
#' jaspOptions <- analysisOptions("~/Documents/someFile.jasp")
#' options <- jaspOptions[[1]] # if there are multiple analyses in the .jasp files you need to select one
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
  if (grepl("[{}\":]", source)) { # json string
    if (!grepl("^\\{.*\\}$", source))
      stop("Your json is invalid, please copy the entire message
           including the outer braces { } that was send to R in the Qt terminal.
           Remember to use single quotes around the message.")

    options <- analysisOptionsFromJSONString(source)
  } else if (file.exists(source)) { # .jasp file
    if (!endsWith(source, ".jasp"))
      stop("The file you provided exists, but it is not a .jasp file")

    options <- analysisOptionsFromJASPfile(source)
  } else { # analysis name
    options <- analysisOptionsFromQMLFile(source)
  }

  return(options)
}

analysisOptionsFromQMLFile <- function(analysis) {
  file <- getQMLFile(analysis)
  options <- readQML(file)
  attr(options, "analysisName") <- analysis

  return(options)
}

getQMLFile <- function(name) {
  modulePath <- getModulePathFromRFunction(name)
  if (is.null(modulePath))
    stop("Could not locate the module location for ", name)

  possibleQmlFile <- file.path(modulePath, "inst", "qml", paste0(name, ".qml")) # it's optional to specify the qml file in Description.qml, you can also just name it RFunc.qml
  if (file.exists(possibleQmlFile))
    return(possibleQmlFile)

  descrFile <- file.path(modulePath, "inst", "Description.qml")
  if (!file.exists(descrFile))
    stop("Could not locate Description.qml in ", modulePath)

  fileSize <- file.info(descrFile)$size
  fileContents <- readChar(descrFile, nchars = fileSize)
  fileContents <- gsub("[\"']", "", fileContents)
  rFuncLocExpr <- paste0("\\{[^\\{\\}]*func:\\s*", name, "[^\\{\\}]*\\}")
  if (!grepl(rFuncLocExpr, fileContents))
    stop("Could not locate qml file for R function ", name, " in inst/qml folder and did not find the R function in inst/Description.qml to look for an alternative name for the qml file")

  rLocMatch <- stringr::str_match(fileContents, rFuncLocExpr)[1]
  qmlLocExpr <- "[a-zA-Z0-9_]+\\.qml"
  if (!grepl(qmlLocExpr, rLocMatch))
    stop("Could not locate qml file for R function ", name, " in inst/qml folder and did not find a qml entry in inst/Description.qml that describes an alternative for the qml filename")

  qmlFileName <- stringr::str_match(rLocMatch, qmlLocExpr)
  qmlFilePath <- file.path(modulePath, "inst", "qml", qmlFileName)
  if (!file.exists(qmlFilePath))
    stop("Found a qml filename for the R function ", name, " but this qml file does not appear to exist in inst/qml/")

  return(qmlFilePath)
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

analysisOptionsFromJASPfile <- function(file) {
  fileCon <- unz(file, "analyses.json")
  on.exit(close(fileCon))
  contents <- rjson::fromJSON(file = fileCon)
  analyses <- contents[["analyses"]]
  if (length(analyses) == 0)
    stop("No analyses found in the provided file")

  options <- vector("list", length(analyses))
  for (i in seq_along(analyses)) {
    analysis <- analyses[[i]]
    options[[i]] <- analysis[["options"]]
    attr(options[[i]], "analysisName") <- analysis[["name"]]
  }

  if (length(options) == 1)
    options <- options[[1]]

  return(options)
}
