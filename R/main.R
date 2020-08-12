#' View the tables and plots in a results object.
#'
#' \code{view} allows you to view output independently of Qt. It uses the same
#' javascript/css/html and should generate identical output. This function may
#' be called directly, but it is more convenient to use \code{runAnalysis}.
#'
#'
#' @param results A named R list returned from a JASP analysis, or a json
#' results string copied from the Qt terminal.
#' @return A html page is generated and placed in a temp folder.
#' @examples
#'
#' options <- analysisOptions("BinomialTest")
#' results <- runAnalysis("BinomialTest", "debug", options, view=FALSE)
#' view(results)
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' view('{
#'    "id" : 6,
#'    "name" : "BinomialTest",
#'    "results" : {
#'       ".meta" : [
#'          {
#'             "name" : "binomial",
#'             "type" : "table"
#'          },
#'          {
#'             "meta" : [],
#'             "name" : "descriptives",
#'             "type" : "object"
#'          }
#'       ],
#'       "binomial" : {
#'          "citation" : [ "JASP Team (2017). JASP (Version 0.8.2) [Computer software]." ],
#'          "data" : [
#'             {
#'                "case" : "",
#'                "counts" : ".",
#'                "level" : ".",
#'                "lowerCI" : ".",
#'                "p" : ".",
#'                "proportion" : ".",
#'                "total" : ".",
#'                "upperCI" : "."
#'             }
#'          ],
#'          "footnotes" : [
#'             {
#'                "symbol" : "<em>Note.</em>",
#'                "text" : "Proportions tested against value: 0.5."
#'             }
#'          ],
#'          "schema" : {
#'             "fields" : [
#'                {
#'                   "combine" : true,
#'                   "name" : "case",
#'                   "title" : "",
#'                   "type" : "string"
#'                },
#'                {
#'                   "name" : "level",
#'                   "title" : "Level",
#'                   "type" : "string"
#'                },
#'                {
#'                   "name" : "counts",
#'                   "title" : "Counts",
#'                   "type" : "integer"
#'                },
#'                {
#'                   "name" : "total",
#'                   "title" : "Total",
#'                   "type" : "integer"
#'                },
#'                {
#'                   "format" : "sf:4;dp:3",
#'                   "name" : "proportion",
#'                   "title" : "Proportion",
#'                   "type" : "number"
#'                },
#'                {
#'                   "format" : "dp:3;p:.001",
#'                   "name" : "p",
#'                   "title" : "p",
#'                   "type" : "number"
#'                }
#'             ]
#'          },
#'          "title" : "Binomial Test"
#'       },
#'       "title" : "Binomial Test"
#'    },
#'    "revision" : 2,
#'    "status" : "complete"
#' }')
#'
#' @export view
view <- function(results) {
  if (is.character(results) && jsonlite::validate(results) == TRUE) { # assuming a json string
    results <- jsonlite::fromJSON(results, simplifyVector=FALSE)
    if (!"results" %in% names(results))
      stop("Incorrect json provided. Could not locate required field 'results'")
  } else if (!is.list(results) || !"results" %in% names(results)) {
    stop("Incorrect object provided in results,
    please enter a valid json string or a named results list.")
  }

  content <- list(
    id = ifelse(is.null(results[["id"]]), 0, results[["id"]]),
    name = ifelse(is.null(results[["name"]]), "analysis", results[["name"]]),
    status = ifelse(is.null(results[["status"]]), "complete", results[["status"]]),
    results = results[["results"]]
  )
  json <- convertResultsListToJson(content)

  initializeOutputFolder(file.path(tempdir(), "jaspTools", "html"))

  htmlFile <- file.path(tempdir(), "jaspTools", "html", "tmp-index.html")
  insertJsonInHtml(json, htmlFile)
  utils::browseURL(htmlFile)
}



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
    returnVal <- suppressWarnings(do.call(JASP::runJaspResults, args))
    sink(NULL)
  } else {
    returnVal <- do.call(JASP::runJaspResults, args)
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
