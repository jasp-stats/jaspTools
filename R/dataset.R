loadCorrectDataset <- function(x, nrows = -1) {
  if (is.matrix(x) || is.data.frame(x)) {
    return(x)
  } else if (is.character(x)) {
    if (! endsWith(x, ".csv")) {
      x <- paste0(x, ".csv")
    }

    # check if it's a path to a file
    if (file.exists(x)) {
      dataset <- utils::read.csv(x, header = TRUE, check.names = FALSE, stringsAsFactors = TRUE, nrows = nrows)
      if (nrows == 0)
        return(dataset[0, , drop = FALSE])
      return(dataset)
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
        return(data.table::fread(fullPath, header = TRUE, check.names = FALSE, data.table = FALSE,
                                 nrows = if (nrows == -1) Inf else nrows))
      }
      allDatasets <- c(allDatasets, basename(datasets))

    }

    cat("It appears", x, "could not be found. Please supply either a full filepath or the name of one of the following datasets:\n",
        paste0(sort(allDatasets), collapse = '\n'), "\n")
    stop(paste(x, "not found"))
  }
  stop(paste("Cannot handle data of type", mode(x)))
}

findAllColumnNamesInOptions <- function(options, allColumnNames) {
  rapply(options, classes = "character", how = "unlist", f = function(x) {
    check <- x %in% allColumnNames
    if (any(check)) {
      x[check]
    } else {
      NULL
    }
  })
}

preloadDataset <- function(dataset, options) {

  dataset <- loadCorrectDataset(dataset)
  columns <- findAllColumnNamesInOptions(options, colnames(dataset))

  # TODO: here we need to set the correct types somehow!
  # perhaps we can check if options[[<key>.types]] exists and otherwise
  # we do our best? for debug.csv we can at least hardcode the correct types
  # perhaps also always convert character to factor?

  .setInternal("dataset", dataset[columns])

}
