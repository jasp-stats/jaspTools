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
