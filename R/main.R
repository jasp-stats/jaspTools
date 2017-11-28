#' View the tables and plots in a results object.
#'
#' \code{view} allows you to view output independently of Qt. It uses the same
#' javascript/css/html and should generate identical output. This function may
#' be called directly, but it is more convenient to use \code{JASPTools::run}.
#'
#'
#' @param results A named R list returned from a JASP analysis, or a json
#' results string copied from the Qt terminal.
#' @return A html page is generated and placed in a temp folder.
#' @examples
#'
#' options <- JASPTools::analysisOptions("BinomialTest")
#' results <- JASPTools::run("BinomialTest", "debug", options, view=FALSE)
#' JASPTools::view(results)
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' JASPTools::view('{
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
    content <- paste0("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"Unable to jsonify\" } }")
  }
  content <- .parseUnicode(content)
  content <- gsub("<div class=stack-trace>", "<div>", content, fixed=TRUE)

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



#' Run a JASP analysis in R.
#'
#' \code{run} makes it possible to execute a JASP analysis in R. Usually this
#' process is a bit cumbersome as there are a number of objects unique to the
#' JASP environment. Think .ppi, data-reading, etc. These (rcpp) objects are
#' replaced in the JASPTools so you do not have to deal with them. Note that
#' \code{run} sources JASP analyses every time it runs, so any change in
#' analysis code between calls is incorporated. The output of the analysis is
#' shown automatically through a call to \code{JASPTools::view} and returned
#' invisibly.
#'
#'
#' @param name String indicating the name of the analysis to run. This name is
#' identical to that of the main function in a JASP analysis.
#' @param dataset Data.frame or string; if it's a string it must match one of
#' the JASP datasets (e.g., "debug").
#' @param options List of options to supply to the analysis (see also
#' \code{JASPTools::analysisOptions}).
#' @param perform String containing either "run" (default) or "init".
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the
#' analysis.
#' @param sideEffects Boolean or character vector indicating which side effects
#' are allowed.  Side effects are persistent changes made by JASPTools or
#' analyses run in JASPTools, they include loading of packages ("pkgLoading"),
#' setting of .libPaths ("libPaths"), modifying of global options() ("options")
#' and altering the global environment ("globalEnv"). Supply the desired side
#' effects in a character vector (or simply TRUE for all). JASPTools will make
#' an effort to prevent any side effect not included in the vector (or all if
#' set to FALSE)
#' @examples
#'
#' options <- JASPTools::analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#' JASPTools::run("BinomialTest", "debug", options)
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' options <- JASPTools::analysisOptions('{
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
#' JASPTools::run("BinomialTest", "debug", options)
#'
#' # If we want R functions sourced to the global env
#' JASPTools::run("BinomialTest", "debug", options, sideEffects="globalEnv")
#'
#' # Or additionally have the .libPaths() set to JASP<e2><80><99>s R packages
#' JASPTools::run("BinomialTest", "debug", options, sideEffects=c("globalEnv", "libPaths"))
#'
#' @export run
run <- function(name, dataset, options, perform = "run", view = TRUE, quiet = FALSE, sideEffects = FALSE) {
  envir <- .GlobalEnv
  if (! isTRUE(sideEffects)) {
    if (! is.logical(sideEffects))
      sideEffects <- tolower(sideEffects)
    if (! "globalenv" %in% sideEffects || identical(sideEffects, FALSE))
      envir <- new.env()

    loadedPkgs <- loadedNamespaces()
    opts <- options()
    libPaths <- .libPaths()
    on.exit({
      .resetInternals()
      if (! "pkgloading" %in% sideEffects || identical(sideEffects, FALSE))
        .restoreNamespaces(loadedPkgs)
      if (! "options" %in% sideEffects || identical(sideEffects, FALSE))
        .restoreOptions(opts)
      if (! "libpaths" %in% sideEffects || identical(sideEffects, FALSE))
        .libPaths(libPaths)
      if (quiet)
        suppressWarnings(sink(NULL))
    })
  } else { # no side effects, but we still need on.exit
    on.exit({
      .resetInternals()
      if (quiet)
        suppressWarnings(sink(NULL))
    })
  }

  .initRunEnvironment(envir = envir, dataset = dataset, perform = perform)

  config <- .getJSON(name, "input=>dataset", "output")
  possibleArgs <- list(
    name = name,
    options.as.json.string = rjson::toJSON(options),
    dataset.cols = config[["dataset"]],
    output.description = config[["output"]],
    perform = perform
  )
  runArgs <- formals(envir$run)
  argNames <- intersect(names(possibleArgs), names(runArgs))
  args <- possibleArgs[argNames]

  if (quiet)
    sink(tempfile())

  results <- do.call(envir$run, args, envir=envir)

  if (quiet)
    sink(NULL)

  if (view)
    view(results)

  if (jsonlite::validate(results))
    results <- rjson::fromJSON(results)

  results[["state"]] <- .getInternal("state")

  return(invisible(results))
}
