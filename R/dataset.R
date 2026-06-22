#' Extract a Dataset from a JASP File
#'
#' Thin compatibility wrapper around `jaspSyntax::readDatasetFromJaspFile()`.
#'
#' @param jaspFile Character string specifying the path to the .jasp file.
#' @param dataSetIndex Integer specifying which dataset to extract if the JASP
#'   file contains multiple datasets. Currently only `1L` is supported because
#'   that is the index supported by the `jaspSyntax` backend.
#'
#' @return Either a data.frame containing the extracted dataset or `NULL` when
#'   the `.jasp` file does not contain tabular data.
#'
#' @details
#' `jaspTools` now relies on `jaspSyntax` for reading datasets from saved JASP
#' files, so the native `.jasp` decoding and dataset reconstruction logic lives
#' in one place. Numeric columns, including `Inf` and `-Inf`, are returned
#' directly from the `jaspSyntax` backend without additional post-processing.
#'
#' @examples
#' \dontrun{
#' # Extract dataset from a JASP file
#' df <- extractDatasetFromJASPFile("path/to/analysis.jasp")
#'
#' # View the structure
#' str(df)
#' }
#'
#' @export
extractDatasetFromJASPFile <- function(jaspFile, dataSetIndex = 1L) {
  .jaspSyntaxReadDatasetFromJaspFile(jaspFile, dataSetIndex = dataSetIndex)
}

.jaspSyntaxReadDatasetFromJaspFile <- function(jaspFile, dataSetIndex = 1L) {
  if (!exists("readDatasetFromJaspFile", envir = asNamespace("jaspSyntax"), inherits = FALSE)) {
    stop(
      "Installed jaspSyntax does not provide `readDatasetFromJaspFile()`. ",
      "Install the jaspSyntax build that contains the dataset reader API."
    )
  }

  jaspSyntax::readDatasetFromJaspFile(jaspFile, dataSetIndex = dataSetIndex)
}
.jaspSyntaxHelperName <- function(names, required = TRUE, feature = "jaspSyntax bridge API") {
  namespace <- asNamespace("jaspSyntax")
  for (name in names) {
    if (exists(name, envir = namespace, inherits = FALSE))
      return(name)
  }

  if (isTRUE(required)) {
    stop(
      "Installed jaspSyntax does not provide `", names[[1L]], "()`. ",
      "Install the jaspSyntax build that exposes the ", feature, "."
    )
  }

  NULL
}

.callWithSupportedArgs <- function(fun, args, requiredArgs = names(args),
                                   requiredArgGroups = list(),
                                   functionName = "jaspSyntax bridge function",
                                   feature = "jaspSyntax bridge API") {
  funFormals <- tryCatch(names(formals(fun)), error = function(e) NULL)
  if (is.null(funFormals) || "..." %in% funFormals)
    return(do.call(fun, args))

  missingRequired <- setdiff(requiredArgs, funFormals)
  missingGroups <- vapply(requiredArgGroups, function(group) {
    !any(group %in% funFormals)
  }, logical(1L))

  if (length(missingRequired) > 0L || any(missingGroups)) {
    missingArgs <- c(
      missingRequired,
      vapply(requiredArgGroups[missingGroups], paste, character(1L), collapse = " or ")
    )
    stop(
      "Installed ", functionName, " does not support required bridge argument",
      if (length(missingArgs) == 1L) "" else "s",
      ": ", paste(missingArgs, collapse = ", "), ". ",
      "Install the jaspSyntax build that exposes the ", feature, ".",
      call. = FALSE
    )
  }

  do.call(fun, args[intersect(names(args), funFormals)])
}

.jaspSyntaxCall <- function(names, args = list(), required = TRUE,
                            feature = "jaspSyntax bridge API",
                            requiredArgs = names(args),
                            requiredArgGroups = list()) {
  helperName <- .jaspSyntaxHelperName(names, required = required, feature = feature)
  if (is.null(helperName))
    return(NULL)

  fun <- get(helperName, envir = asNamespace("jaspSyntax"), inherits = FALSE)
  .callWithSupportedArgs(
    fun,
    args,
    requiredArgs = requiredArgs,
    requiredArgGroups = requiredArgGroups,
    functionName = paste0("jaspSyntax::", helperName, "()"),
    feature = feature
  )
}

.jaspSyntaxClearDatasetState <- function(required = TRUE) {
  .jaspSyntaxCall(
    "clearDatasetState",
    required = required,
    feature = "native dataset lifecycle API"
  )
  invisible(NULL)
}

.jaspSyntaxClearNativeState <- function(required = TRUE) {
  .jaspSyntaxCall(
    c("clearNativeState", "clearAllNativeState"),
    required = required,
    feature = "native lifecycle API"
  )
  invisible(NULL)
}

.jaspSyntaxLoadAnalysisDataset <- function(dataset, modulePath = NULL,
                                           analysisName = NULL,
                                           options = NULL) {
  .jaspSyntaxCall(
    "loadAnalysisDataset",
    args = list(
      dataset = dataset,
      modulePath = modulePath,
      analysisName = analysisName,
      options = options
    ),
    required = TRUE,
    feature = "native analysis dataset API",
    requiredArgs = c("dataset", "modulePath", "analysisName", "options")
  )
}

loadCorrectDataset <- function(x) {
  if (is.matrix(x) || is.data.frame(x)) {
    return(x)
  } else if (is.character(x)) {
    if (!endsWith(x, ".csv")) {
      x <- paste0(x, ".csv")
    }

    # check if it's a path to a file
    if (file.exists(x)) {
      return(utils::read.csv(x, header = TRUE, check.names = FALSE, stringsAsFactors = TRUE))
    }

    # check if it's a name of a JASP dataset
    locations <- getPkgOption("data.dirs")
    allDatasets <- c()
    for (location in locations) {

      files <- list.files(location, recursive = TRUE, include.dirs = TRUE)
      datasets <- files[endsWith(files, ".csv")]
      match <- which(basename(datasets) == x)
      if (length(match) > 0) {
        fullPath <- file.path(location, datasets[match[1]])
        if (length(match) > 1) {
          warning("Multiple datasets exists with the same name, choosing '", datasets[match[1]], "'")
        }
        return(data.table::fread(fullPath, header = TRUE, check.names = FALSE, data.table = FALSE))
      }
      allDatasets <- c(allDatasets, basename(datasets))

    }

    cat("It appears", x, "could not be found. Please supply either a full filepath or the name of one of the following datasets:\n",
        paste0(sort(allDatasets), collapse = '\n'), "\n")
    stop(paste(x, "not found"))
  }
  stop(paste("Cannot handle data of type", mode(x)))
}

preloadDataset <- function(datasetPathOrObject, options, modulePath = NULL,
                           analysisName = NULL) {
  .jaspSyntaxClearDatasetState(required = TRUE)

  if (is.null(datasetPathOrObject)) {
    .setInternal("preloadedDataset", data.frame())
    .setInternal("preloadedColumnEncoderContext", NULL)
    return(invisible(NULL))
  }

  dataset <- loadCorrectDataset(datasetPathOrObject)

  if (is.matrix(dataset)) {
    dataset <- as.data.frame(dataset, stringsAsFactors = FALSE)
  }

  datasetState <- .jaspSyntaxLoadAnalysisDataset(
    dataset = dataset,
    modulePath = modulePath,
    analysisName = analysisName,
    options = options
  )

  loadedDataset <- .jaspSyntaxDatasetStateValue(datasetState, "loadedDataset")
  if (is.null(loadedDataset))
    loadedDataset <- .jaspSyntaxReadLoadedDataset(required = TRUE)

  requestedDataset <- .jaspSyntaxDatasetStateValue(datasetState, "requestedDataset")
  if (is.null(requestedDataset))
    requestedDataset <- .jaspSyntaxReadRequestedDataset(required = FALSE)
  if (is.null(requestedDataset))
    requestedDataset <- loadedDataset

  resultDecodingDataset <- .jaspSyntaxDatasetStateValue(datasetState, "resultDecodingDataset")
  if (is.null(resultDecodingDataset))
    resultDecodingDataset <- requestedDataset

  columnEncoderContext <- .jaspSyntaxDatasetStateColumnEncoderContext(datasetState)

  .setInternal("preloadedDataset", resultDecodingDataset)
  .setInternal("preloadedColumnEncoderContext", columnEncoderContext)
  invisible(requestedDataset)
}

.jaspSyntaxReadLoadedDataset <- function(required = TRUE) {
  dataset <- .jaspSyntaxCall(
    "readLoadedDataset",
    required = required,
    feature = "native loaded dataset API"
  )
  .validateJaspSyntaxDataset(dataset, "jaspSyntax::readLoadedDataset()", required)
}

.jaspSyntaxReadRequestedDataset <- function(required = FALSE) {
  dataset <- .jaspSyntaxCall(
    "readRequestedDataset",
    required = required,
    feature = "native requested dataset API"
  )
  .validateJaspSyntaxDataset(dataset, "jaspSyntax::readRequestedDataset()", required)
}

.jaspSyntaxDatasetStateValue <- function(datasetState, name) {
  if (is.null(datasetState))
    return(NULL)

  if (is.data.frame(datasetState) && identical(name, "loadedDataset"))
    return(datasetState)

  if (!is.list(datasetState) || is.null(datasetState[[name]]))
    return(NULL)

  .validateJaspSyntaxDataset(
    datasetState[[name]],
    paste0("jaspSyntax::loadAnalysisDataset()$", name),
    required = TRUE
  )
}

.jaspSyntaxDatasetStateColumnEncoderContext <- function(datasetState) {
  if (!is.list(datasetState) || is.null(datasetState[["columnEncoderContext"]]))
    return(NULL)

  datasetState[["columnEncoderContext"]]
}

.validateJaspSyntaxDataset <- function(dataset, source, required = TRUE) {
  if (is.null(dataset))
    return(NULL)

  if (!is.data.frame(dataset)) {
    if (isTRUE(required))
      stop(source, " returned an unexpected dataset object.")

    return(NULL)
  }

  dataset
}
