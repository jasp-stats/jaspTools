# TODO
# fix click actions in browser
# add way of easily creating options
# see if the unicode to html conversion is possible
# create standard options for each analysis
#
.pkgOptions <- list2env(list(
  r.dir="../../../JASP-Engine/JASP/R/",
  html.dir="../../../JASP-Desktop/html/",
  json.dir="../../../Resources/Library/",
  data.dir="../../../Resources/Data Sets/",
  .ppi=96
))

#' @export
view <- function(results, jsonify=TRUE) {

  if (jsonify == TRUE && is.list(results)) {
    results <- list(
      id = 0,
      name = "analysis",
      status = "complete",
      results = results)

    results <- try(rjson::toJSON(results))
    if (class(results) == "try-error") {
      results <- paste0("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", "Unable to jsonify", "\" } }")
    }
  }

  html <- readChar(paste0(.getPkgOption("html.dir"), "/index.html"), 1000000)
  content <- paste0(
    "<script>
      $(document).ready(function() {
        window.analysisChanged(", results, ")
      })
    </script></body>")
  html <- gsub("</body>", content, html)

  if (! "js" %in% list.files(paste0(tempdir(), "/html"))) {
    file.copy(.getPkgOption("html.dir"), tempdir(), recursive=TRUE)
  }

  file <- paste0(tempdir(), "/html/tmp-index.html")
  writeChar(html, file)
  browseURL(file)

}

#' @export
run <- function(name, dataset, options, perform="run") {

  .initEnvironment(dataset=dataset, perform=perform)
  dataset <- NULL

  analysis <- eval(parse(text=name))

  results <- tryCatch(expr={

    analysis(dataset=NULL, options=options, perform=perform, callback=function(...) list(status="ok"), state=NULL)

  },
  error=function(e) e)

  if (inherits(results, "error")) {

    error <- gsub("\"", "'", as.character(results), fixed=TRUE)
    error <- gsub("\\\n", " ", error)

    errorResponse <- paste0("{ \"status\" : \"exception\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", error, "\" } }")

    view(errorResponse, jsonify=FALSE)

  } else if (is.null(results)) {

    view("null", jsonify=TRUE)

  } else {

    if ("results" %in% names(results)) {

      results <- .imgToResults(results)
      results$results <- .addCitationToResults(results$results)
      results$state <- NULL

    } else {

      results <- .addCitationToResults(results)
    }

    view(results$results, jsonify=TRUE)
  }

}
