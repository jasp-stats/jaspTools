# TODO
# fix click actions in browser
# add way of easily creating options
# see if the unicode to html conversion is possible
# create standard options for each analysis
# JASP should init paths on viewPkgSettings()


#' View the tables and plots in a results object.
#'
#' \code{view} allows you to view output independently of Qt. It uses the same
#' javascript/css/html and should consequently generate identical output.
#' This function may be called directly, but it is more convenient to use \code{JASPTools::run}.
#'
#' @param results An R list returned from an analysis (either directly or through run)
#' or a json results string copied from the Qt terminal.
#'
#' @return A html page is generated and placed in a temp folder. All with tables and images
view <- function(results) {

  content <- NULL
  if (is.character(results) && jsonlite::validate(results) == TRUE) { # assuming a json string

    unjsonified <- rjson::fromJSON(results)
    if ("results" %in% names(unjsonified)) {
      results <- unjsonified[["results"]]
      id <- ifelse(is.null(unjsonified[["id"]]), 0, unjsonified[["id"]])
      name <- ifelse(is.null(unjsonified[["name"]]), "analysis", unjsonified[["name"]])
      status <- ifelse(is.null(unjsonified[["status"]]), "complete", unjsonified[["status"]])
    } else {
      stop("Incorrect json provided. Could not locate required field 'results'")
    }

  } else if (is.list(results) && "results" %in% names(results)) {

    id <- ifelse(is.null(results[["id"]]), 0, results[["id"]])
    name <- ifelse(is.null(results[["name"]]), "analysis", results[["name"]])
    status <- ifelse(is.null(results[["status"]]), "complete", results[["status"]])
    results <- results[["results"]]

  } else {

    stop("Incorrect object provided in results,
         please enter a valid json string or a named results list.")

  }

  content <- list(
    id = id,
    name = name,
    status = status,
    results = results
  )
  content <- try(rjson::toJSON(content))
  if (class(content) == "try-error") {
    content <- paste0("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", "Unable to jsonify", "\" } }")
  }

  html <- readChar(file.path(.getPkgOption("html.dir"), "index.html"), 1000000)
  insertedJS <- paste0(
    "<script>
      $(document).ready(function() {
        window.analysisChanged(", content, ")
      })
    </script></body>")
  html <- gsub("</body>", insertedJS, html)

  outputFolder <- file.path(tempdir(), "JASPTools", "html")
  if (! "js" %in% list.files(outputFolder)) {
    file.copy(.getPkgOption("html.dir"), file.path(tempdir(), "JASPTools"), recursive = TRUE)
  }

  file <- file.path(tempdir(), "JASPTools", "html", "tmp-index.html")
  writeChar(html, file)
  browseURL(file)

}

#' @export
run <- function(name, dataset, options, perform = "run") {

  .initRunEnvironment(dataset = dataset, perform = perform)
  analysis <- eval(parse(text = name))

  results <- tryCatch(expr = {
    analysis(dataset = NULL, options = options, perform = perform,
             callback = function(...) list(status = "ok"), state = NULL)
  },
  error = function(e) e)

  if (inherits(results, "error")) {

    error <- gsub("\"", "'", as.character(results), fixed = TRUE)
    error <- gsub("\\\n", " ", error)
    errorResponse <- paste0("{ \"status\" : \"exception\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", error, "\" } }")
    view(errorResponse)

  } else if (is.null(results)) {

    view("null")

  } else {

    if ("results" %in% names(results)) {

      results <- .imgToResults(results)
      results$results <- .addCitationToResults(results$results)
      results$state <- NULL

    } else {

      results <- .addCitationToResults(results)
      results <- list(results = results)
    }

    view(results)
  }

}
