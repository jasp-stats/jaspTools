loadCorrectDataset <- function(x) {
  if (is.matrix(x) || is.data.frame(x)) {
    return(x)
  } else if (is.character(x)) {
    if (! endsWith(x, ".csv")) {
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

preloadDataset <- function(datasetPathOrObject, options) {

  dataset <- loadCorrectDataset(datasetPathOrObject)

  # repair any names like "", which cause false positives in findAllColumnNamesAndTypes
  # because empty options are often ""
  cnms <- colnames(dataset)
  if (any(cnms == "")) {

    bad <- which(cnms == "")
    newCnms <- make.names(cnms)
    cnms[bad] <- newCnms[bad]
    colnames(dataset) <- cnms

  }
  # columns <- findAllColumnNamesInOptions(options, colnames(dataset))
  temp    <- findAllColumnNamesAndTypes(options, colnames(dataset))

  variables <- temp[["variables"]]
  types     <- temp[["types"]]

  dataset <- convertToTypes(dataset[variables], types, datasetPathOrObject)

  .setInternal("preloadedDataset", dataset)

}

convertToTypes <- function(dataset, types, datasetPathOrObject) {

  typesEnv <- if (is.character(datasetPathOrObject)) {
    datasetName <- basename(datasetPathOrObject)
    if (exists(datasetName, where = .jaspDataSets))
      get(datasetName, envir = .jaspDataSets)
    else NULL
  }

  for (i in seq_along(dataset)) {

    dataset[[i]] <- switch(types[i],
           "scale"   = as.numeric(dataset[[i]]),
           "ordered" = as.ordered(dataset[[i]]),
           "nominal" = as.factor(dataset[[i]]),
           autodetectType(dataset[[i]], colnames(dataset)[i], typesEnv)
    )

  }
  return(dataset)
}

autodetectType <- function(column, name, typesEnv) {

  if (!is.null(typesEnv)) {

    if (!exists(name, envir = typesEnv)) {
      devcat(sprintf("No type information found for column '%s' in dataset %s.\n", name, getFilename(typesEnv)))
    } else {
      type <- get(name, envir = typesEnv)
      devcat(sprintf("type information found for column '%s' in dataset '%s': '%s'.\n", name, getFilename(typesEnv), type))
      return(
        switch(type,
             "scale"   = as.numeric(column),
             "ordered" = as.ordered(column),
             "nominal" = as.factor( column)
      ))
    }

  }

  if (is.character(column)) {
    devcat(sprintf("Converting column '%s' from character to factor.\n", name))
    return(as.factor(column))
  } else {
    devcat(sprintf("Leaving column '%s' as is.\n", name))
  }

  return(column)
}

setFilename <- function(x, name) {
  attr(x, "filename") <- name
  x
}
getFilename <- function(x) {
  attr(x, "filename")
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

predicateVariables <- function(x, allColumnNames) {
  is.character(x) && all(x %in% allColumnNames)
}

predicateVariableTypes <- function(lst, nm) {
  nm2 <- paste0(nm, ".types")
  is.character(lst[[nm2]]) && length(lst[[nm2]]) == length(lst[[nm]]) && all(lst[[nm2]] %in% c("scale", "ordinal", "nominal"))
}

recursivelyLoopOptions <- function(x, allColumnNames, resultEnv) {

  if (is.list(x)) {

    idx <- which(vapply(x, predicateVariables, logical(1L), allColumnNames = allColumnNames))

    if (length(idx) > 0) {
      resultEnv$variables <- c(resultEnv$variables, unlist(x[idx], use.names = FALSE))

      nm <- names(x)[idx]
      idx2 <- vapply(nm, predicateVariableTypes, logical(1L), lst = x)

      for (i in seq_along(idx2)) {
        if (idx2[i]) {
          idx3 <- paste0(nm[idx[i]], ".types")
          resultEnv$types <- c(resultEnv$types, unlist(x[idx3], use.names = FALSE))
        } else {
          resultEnv$types <- c(resultEnv$types, rep(NA_character_, length(x[idx[i]])))
        }
      }

    }

    for (i in seq_along(x)) {
      if (is.list(x[[i]]))
        Recall(x[[i]], allColumnNames, resultEnv)
    }
  }

}

findAllColumnNamesAndTypes <- function(options, allColumnNames) {

  resultEnv <- new.env()
  resultEnv$variables <- character(0L)
  resultEnv$types     <- character(0L)

  recursivelyLoopOptions(options, allColumnNames, resultEnv)

  return(as.list(resultEnv))
}

devcat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                   append = FALSE) {
  if (getOption("jasptools_devcat", FALSE))
    return(cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append))
  invisible(NULL)
}
