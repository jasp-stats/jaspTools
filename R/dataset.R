#' Extract a Dataset from a JASP File
#'
#' Returns the dataset saved in a .jasp file as a data.frame.
#'
#' @param jaspFile Character string specifying the path to the .jasp file.
#' @param dataSetIndex Integer specifying which dataset to extract. Only the first
#'   dataset (1, the default) is supported.
#'
#' @return A data.frame with the columns of the dataset, or `NULL` when the file
#'   holds no data.
#'
#' @details
#' The file is read by [jaspSyntax::readDatasetFromJaspFile()], which opens it the
#' way JASP does: the database in the archive is upgraded from the JASP version that
#' wrote it, so files from every JASP version are read alike. Scale columns are
#' numeric (missing values are `NaN`, infinity is `Inf`); nominal and ordinal columns
#' are character vectors of their values.
#'
#' The data.frame keeps the attributes jaspSyntax sets to recognize it as the
#' dataset of that .jasp file.
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
  jaspSyntax::readDatasetFromJaspFile(jaspFile, dataSetIndex)
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

setFilename <- function(x, name) {
  attr(x, "filename") <- name
  x
}

.debug.csvTypes <- list2env(list(
  V1           = "scale",
  contNormal   = "scale",
  contGamma    = "scale",
  contBinom    = "nominal",
  contExpon    = "scale",
  contWide     = "scale",
  contNarrow   = "scale",
  contOutlier  = "scale",
  contcor1     = "scale",
  contcor2     = "scale",
  facGender    = "nominal",
  facExperim   = "nominal",
  facFive      = "scale",
  facFifty     = "scale",
  facOutlier   = "nominal",
  debString    = "nominal",
  debMiss1     = "scale",
  debMiss30    = "scale",
  debMiss80    = "scale",
  debMiss99    = "scale",
  debBinMiss20 = "nominal",
  debNaN       = "scale",
  debNaN10     = "scale",
  debInf       = "scale",
  debCollin1   = "scale",
  debCollin2   = "scale",
  debCollin3   = "scale",
  debEqual1    = "scale",
  debEqual2    = "scale",
  debSame      = "scale",
  unicode      = "nominal"
))

.jaspDataSets <- list2env(list(
  "debug.csv" = setFilename(name = "debug.csv", .debug.csvTypes),
  "test.csv"  = setFilename(name = "test.csv",  .debug.csvTypes)
))


#' Add types for a dataset.
#'
#' @param name The path to the dataset on disk.
#' @param lst A list of column names and their types.
#'
#' @details Note that this has already been done for debug.csv and test.csv.
#' This is an alternative interface to specify types in jaspTools.
#' The usual way is to specify a key in the options object with the types.
#' For example, if `options[["variables"]] == c("contNormal", "facFive", "contBinom")`
#' then one could indicate the types by writing
#' `options[["variables.types"]] == c("scale", "ordinal", "nominal")`.
#' With `addTypedDataSet` this becomes:
#' ```r
#' addTypedDataSet(
#'  "test.csv",
#'  list(
#'    contNormal   = "scale",
#'    facFive      = "ordinal",
#'    contBinom    = "nominal"
#'  )
#' )
#' ````
#' the main benefit is that this only needs to be done once,
#' instead of being repeated for each options object.
#' @export
#'
#' @examples
#' addTypedDataSet(
#'  "test.csv",
#'  list(
#'    V1           = "scale",
#'    contNormal   = "scale",
#'    contGamma    = "scale",
#'    contBinom    = "nominal"
#'  )
#' )
addTypedDataSet <- function(name, lst) {

  if (!is.list(lst) || any(names(lst) == ""))
    stop("lst should be a named list")

  if (!all(vapply(lst, function(x) is.character(x) && x %in% c("scale", "ordinal", "nominal"), logical(1L))))
    stop("all elements of lst should be charcter and one of \"scale\", \"ordinal\", or \"nominal\"")

  .jaspDataSets[[name]] <- setFilename(name = basename(name), list2env(lst))

}

