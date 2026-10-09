# functions / properties to replace JASP's rcpp functions / properties

# These are not used in combination with getAnywhere() in the code so they cannot be found
.insertRbridgeIntoEnv <- function(env) {
  env[[".automaticColumnEncDecoding"]] <- FALSE
  env[[".encodeColNamesStrict"]]       <- function(x) return(x)
  env[[".decodeColNamesStrict"]]       <- function(x) return(x)
  env[[".encodeColNamesLax"]]          <- function(x) return(x)
  env[[".decodeColNamesLax"]]          <- function(x) return(x)
  env[[".decodeColTypes"]]             <- function(x) return(x)
  env[[".encodeColNamesStrict"]]       <- function(x) return(x)

  env[[".setColumnDataAsScale"]]       <- function(...) return(TRUE)
  env[[".setColumnDataAsOrdinal"]]     <- function(...) return(TRUE)
  env[[".setColumnDataAsNominal"]]     <- function(...) return(TRUE)
  env[[".setColumnDataAsNominalText"]] <- function(...) return(TRUE)

  env[[".allColumnNamesDataset"]]      <- function(...) {
    dataset <- .getInternal("dataset")
    dataset <- loadCorrectDataset(dataset)
    return(colnames(dataset))
  }
}

# These are used in combination with getAnywhere() and can stay in the jaspTools namespace
.ppi <- 192

.baseCitation <- "x"

.readDatasetToEndNative <- function(columns = c(), columns.as.numeric = c(), columns.as.ordinal = c(),
                                    columns.as.factor = c(), all.columns = FALSE) {

  dataset <- .getInternal("dataset")
  dataset <- loadCorrectDataset(dataset)

  if (all.columns) {
    columns <- colnames(dataset)
    columns <- columns[columns != ""]
  }
  dataset <- jaspBase:::.vdf(dataset, columns, columns.as.numeric, columns.as.ordinal,
                        columns.as.factor, all.columns, exclude.na.listwise = c())

  return(dataset)
}

.readFullDatasetToEnd <- function() {

  dataset <- .getInternal("dataset")
  dataset <- loadCorrectDataset(dataset)

  return(dataset)
}

.readDataSetHeaderNative <- function(columns = c(), columns.as.numeric = c(), columns.as.ordinal = c(),
                                     columns.as.factor = c(), all.columns = FALSE) {

  dataset <- .readDatasetToEndNative(columns, columns.as.numeric, columns.as.ordinal,
                                     columns.as.factor, all.columns)
  dataset <- dataset[0, , drop = FALSE]

  return(dataset)
}

.readDataSetRequestedNative <- function() {
  return(.getInternal("preloadedDataset"))
}

.requestTempFileNameNative <- function(...) {
  root <- getTempOutputLocation("html")
  numPlots <- length(list.files(file.path(root, "plots")))
  list(
    root = root,
    relativePath = file.path("plots", paste0(numPlots + 1, ".png"))
  )
}

.requestStateFileNameNative <- function() {
  root <- getTempOutputLocation("state")
  name <- "state"
  list(
    root = root,
    relativePath = name
  )
}

.imageBackground <- function(...) return("white")
