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
    return(read.csv(fullPath, header = TRUE, check.names = FALSE))
  } else {
    return(x)
  }
}
