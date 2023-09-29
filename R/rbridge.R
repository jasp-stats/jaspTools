# functions / properties to replace JASP's rcpp functions / properties

# These are not used in combination with getAnywhere() in the code so they cannot be found
.insertRbridgeIntoEnv <- function(env) {
  env[[".automaticColumnEncDecoding"]] <- FALSE
  env[[".encodeColNamesStrict"]]       <- function(x) return(x)
  env[[".decodeColNamesStrict"]]       <- function(x) return(x)
  env[[".encodeColNamesLax"]]          <- function(x) return(x)
  env[[".decodeColNamesLax"]]          <- function(x) return(x)
  env[[".encodeColNamesStrict"]]       <- function(x) return(x)

  env[[".setColumnDataAsScale"]]       <- function(...) return(TRUE)
  env[[".setColumnDataAsOrdinal"]]     <- function(...) return(TRUE)
  env[[".setColumnDataAsNominal"]]     <- function(...) return(TRUE)
  env[[".setColumnDataAsNominalText"]] <- function(...) return(TRUE)

  env[[".allColumnNamesDataset"]]      <- function(...) {
    dataset <- jaspBase::getDataSet(dataset)
    return(colnames(dataset))
  }
}

# These are used in combination with getAnywhere() and can stay in the jaspTools namespace
.ppi <- 192

.baseCitation <- "x"

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

.callbackNative <- function(...) {
  list(status="ok")
}

.imageBackground <- function(...) return("white")
