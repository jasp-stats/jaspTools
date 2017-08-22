.readDatasetToEndNative <- function(columns = c(), columns.as.numeric = c(), columns.as.ordinal = c(),
                                    columns.as.factor = c(), all.columns = FALSE) {

  dataset <- get("dataset", envir = as.environment("package:JASPTools"))
  dataset <- .loadCorrectDataset(dataset)

  dataset <- .vdf(dataset, columns, columns.as.numeric, columns.as.ordinal,
                  columns.as.factor, all.columns, exclude.na.listwise = c())

  return(dataset)
}

.readDataSetHeaderNative <- function(columns = c(), columns.as.numeric = c(), columns.as.ordinal = c(),
                                     columns.as.factor = c(), all.columns = FALSE) {

  dataset <- .readDatasetToEndNative(columns, columns.as.numeric, columns.as.ordinal,
                                     columns.as.factor, all.columns)
  dataset <- dataset[0, , drop = FALSE]

  return(dataset)
}

.loadCorrectDataset <- function(x) {
  if (is.character(x)) {
    if (! endsWith(x, ".csv")) {
      x <- paste0(x, ".csv")
    }
    root <- .getPkgOption("data.dir")
    datasets <- list.files(root)
    datasets <- datasets[endsWith(datasets, ".csv")]
    if (! x %in% datasets) {
      cat("Files in data directory:\n")
      cat(paste(datasets, collapse = "\n"))
      stop(paste(x, "not found"))
    }
    fullPath <- file.path(root, x)
    return(read.csv(fullPath, header = TRUE))
  } else {
    return(x)
  }
}
