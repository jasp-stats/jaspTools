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

convertResultsListToJson <- function(lst) {
  json <- try(jsonlite::toJSON(lst, null="null", auto_unbox=TRUE, digits=NA))
  if (inherits(json, "try-error"))
    json <- paste0("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"Unable to jsonify\" } }")

  json <- gsub("<div class=stack-trace>", "<div>", json, fixed=TRUE) # this makes sure the stacktrace is not hidden
  json <- gsub("\\\"", "\\\\\"", json, fixed=TRUE) # double escape all escaped quotes (otherwise the printed json is invalid)

  return(json)
}

insertJsonInHtml <- function(json, htmlFile) {
  html <- readChar(file.path(getPkgOption("html.dir"), "index.html"), 1000000)
  insertedJS <- paste0(
    "<script>
      var jasp = {}
      jQuery(function($) {
        $(document).ready(function() {
          window.analysisChanged(", json, ")
        })
      })
    </script></body>")
  html <- gsub("</body>", insertedJS, html)
  html <- changeJsIncludeForAdblockers(html)

  writeChar(html, htmlFile)
}

initializeOutputFolder <- function(folder) {
  if (!dir.exists(folder))
    dir.create(folder, recursive=TRUE)

  if (! "js" %in% list.dirs(folder, full.names=FALSE))
    file.copy(list.files(getPkgOption("html.dir"), full.names = TRUE), folder, recursive = TRUE)

  renameJsFileForAdblockers(folder)
}

changeJsIncludeForAdblockers <- function(html) {
  gsub("analysis.js", "jaspanalysis.js", html, fixed = TRUE)
}

renameJsFileForAdblockers <- function(folder) {
  if (file.exists(file.path(folder, "js", "analysis.js")))
    file.rename(file.path(folder, "js", "analysis.js"), file.path(folder, "js", "jaspanalysis.js"))
}
