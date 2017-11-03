# TODO
# fix click actions in browser
# create standard options for each analysis

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
  content <- .parseUnicode(content)

  if (class(content) == "try-error") {
    content <- paste0("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"Unable to jsonify\" } }")
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

run <- function(name, dataset, options, perform = "run", view = TRUE, sideEffects = FALSE) {
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
      if (! "pkgloading" %in% sideEffects || identical(sideEffects, FALSE))
        .restoreNamespaces(loadedPkgs)
      if (! "options" %in% sideEffects || identical(sideEffects, FALSE))
        .restoreOptions(opts)
      if (! "libpaths" %in% sideEffects || identical(sideEffects, FALSE))
        .libPaths(libPaths)
    })
  }

  .initRunEnvironment(envir = envir, dataset = dataset, perform = perform)

  analysis <- eval(parse(text = name), envir = envir)

  fnEnvir <- envir
  if (identical(envir, .GlobalEnv)) {
    fnEnvir <- environment() # analysis does not exist in the global envir
  }

  results <- evalq(tryCatch(expr = {
    analysis(dataset = NULL, options = options, perform = perform,
             callback = function(...) list(status = "ok"), state = NULL)
  },
  error = function(e) e), fnEnvir)

  if (inherits(results, "expectedError")) {

    errorResponse <- paste0("{ \"status\" : \"error\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", results$message, "\" } }")
    if (view)
      view(errorResponse)

  } else if (inherits(results, "error")) {

    error <- gsub("\"", "'", as.character(results), fixed=TRUE)
    error <- gsub("\\\n", " ", error)

    stackTrace <- as.character(results$stackTrace)
    stackTrace <- gsub("\"", "'", stackTrace, fixed=TRUE)
    stackTrace <- gsub("\\\\", "", stackTrace)
    stackTrace <- paste(stackTrace, collapse="<br><br>")

    errorMessage <- envir$.generateErrorMessage(type='exception', error=error, stackTrace=stackTrace)
    errorResponse <- paste0("{ \"status\" : \"exception\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", errorMessage, "\" } }")
    if (view) view(errorResponse)

  } else if (is.null(results)) {

    if (view)
      view("null")

  } else {

    state <- NULL

    if ("state" %in% names(results)) {

      state <- results$state
      results$state <- NULL

      if (! is.null(names(state))) {
        state[["figures"]] <- c(state[["figures"]], envir$.imgToState(results$results))
      }

    }

    if ("results" %in% names(results)) {

      results <- envir$.imgToResults(results)
      results$results <- envir$.addCitationToResults(results$results)
      results$state <- state

    } else {

      results <- envir$.addCitationToResults(results)
      results <- list(results = results)
    }

    if (view)
      view(results)
  }

  return(invisible(results))
}
