#' Extract a Dataset from a JASP File
#'
#' This function extracts the dataset from a saved JASP file (.jasp) and returns
#' it as a data.frame. JASP files are zip archives containing an SQLite database
#' with the data and metadata.
#'
#' @param jaspFile Character string specifying the path to the .jasp file.
#' @param dataSetIndex Integer specifying which dataset to extract if the JASP
#'   file contains multiple datasets. Default is 1 (the first dataset).
#'
#' @return A data.frame containing the extracted dataset with proper column names,
#'   types, and factor levels.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Unpacking the .jasp archive (which is a zip file)
#'   \item Reading the internal.sqlite database
#'   \item Converting Column_N_DBL and Column_N_INT columns to properly named columns
#'   \item Mapping factor levels from the Labels table to create proper R factors
#'   \item Handling both explicitly nominal columns and columns with label mappings
#' }
#'
#' Special values like NA, NaN, and Inf are handled appropriately:
#' \itemize{
#'   \item "nan" values in DBL columns are converted to NA
#'   \item "inf" values in DBL columns are converted to Inf
#'   \item -1 values in INT columns typically indicate missing values
#' }
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

  if (!file.exists(jaspFile)) {
    stop("File not found: ", jaspFile)
  }

  if (!grepl("\\.jasp$", jaspFile, ignore.case = TRUE)) {
    stop("File must have a .jasp extension")
  }

  # Create a temporary directory to extract the JASP file
  tempDir <- tempfile("jasp_extract_")
  dir.create(tempDir)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # Extract the JASP file (it's a zip archive)
  utils::unzip(jaspFile, exdir = tempDir)

  # Check for internal.sqlite

  sqlitePath <- file.path(tempDir, "internal.sqlite")
  if (!file.exists(sqlitePath)) {
    stop("No internal.sqlite found in the JASP file. The file may be corrupted or from an incompatible version.")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Get column metadata
  columns <- DBI::dbGetQuery(con, sprintf(
    "SELECT id, name, columnType, colIdx FROM Columns WHERE dataSet = %d ORDER BY colIdx",
    dataSetIndex
  ))

  if (nrow(columns) == 0) {
    # No data in this JASP file - return NULL
    return(NULL)
  }

  # Get the labels table - include originalValueJson for value reconstruction
  labels <- DBI::dbGetQuery(con, "SELECT columnId, value, label, originalValueJson FROM Labels ORDER BY columnId, value")

  # Get the data from DataSet_N table
  dataTableName <- paste0("DataSet_", dataSetIndex)
  if (!dataTableName %in% DBI::dbListTables(con)) {
    # No data table in this JASP file - return NULL
    return(NULL)
  }

  # Build a query that casts DBL columns to TEXT to preserve nan/inf values
  # SQLite's R driver coerces mixed-type columns, losing "nan" values
  dataColNames <- DBI::dbListFields(con, dataTableName)
  selectParts <- vapply(dataColNames, function(colName) {
    if (grepl("_DBL$", colName)) {
      sprintf("CAST(%s AS TEXT) AS %s", colName, colName)
    } else {
      colName
    }
  }, character(1L))
  selectQuery <- sprintf("SELECT %s FROM %s ORDER BY rowNumber",
                          paste(selectParts, collapse = ", "), dataTableName)

  # Read the raw data with DBL columns as text
  rawData <- DBI::dbGetQuery(con, selectQuery)

  # Build the result data.frame
  result <- data.frame(row.names = seq_len(nrow(rawData)))

  for (i in seq_len(nrow(columns))) {
    colId <- columns$id[i]
    colName <- columns$name[i]
    colType <- columns$columnType[i]
    colIdx <- columns$colIdx[i] + 1  # SQLite uses 0-based indexing

    # Column names in the raw data
    dblColName <- paste0("Column_", colIdx, "_DBL")
    intColName <- paste0("Column_", colIdx, "_INT")

    # Get labels for this column
    colLabels <- labels[labels$columnId == colId, ]

    if (colType %in% c("nominal", "nominalText", "ordinal")) {
      # This is a categorical column - use INT values mapped to labels
      intValues <- rawData[[intColName]]

      # Check if there are labels with actual text
      hasTextLabels <- nrow(colLabels) > 0 && any(nzchar(colLabels$label))

      if (hasTextLabels) {
        # Create a lookup from value to label
        labelLookup <- stats::setNames(colLabels$label, as.character(colLabels$value))

        # Map integer values to labels
        # -1 typically means NA
        charValues <- ifelse(intValues == -1, NA_character_, labelLookup[as.character(intValues)])
        result[[colName]] <- charValues
      } else {
        # No text labels - check if we have originalValueJson to reconstruct values
        # This handles cases like binary 0/1 columns where JASP stores the original values
        if (nrow(colLabels) > 0 && any(nzchar(colLabels$originalValueJson))) {
          # Parse original values from JSON - they're typically "value\n" format
          originalValues <- gsub("\\s*\n$", "", colLabels$originalValueJson)
          # Try to convert to numeric
          numericOriginal <- suppressWarnings(as.numeric(originalValues))

          if (!any(is.na(numericOriginal))) {
            # All values are numeric - create a lookup
            valueLookup <- stats::setNames(numericOriginal, as.character(colLabels$value))
            result[[colName]] <- ifelse(intValues == -1, NA_integer_,
                                         as.integer(valueLookup[as.character(intValues)]))
          } else {
            # Non-numeric original values - use as character
            valueLookup <- stats::setNames(originalValues, as.character(colLabels$value))
            result[[colName]] <- ifelse(intValues == -1, NA_character_,
                                         valueLookup[as.character(intValues)])
          }
        } else {
          # Fallback: use the integer values directly
          result[[colName]] <- ifelse(intValues == -1, NA_integer_, intValues)
        }
      }
    } else {
      # Scale (numeric) column - use DBL values
      dblValues <- rawData[[dblColName]]

      # Handle special string values in DBL column
      if (is.character(dblValues) || (is.list(dblValues))) {
        dblValues <- as.character(dblValues)
        dblValuesLower <- tolower(dblValues)

        # Check if this column contains infinity values
        hasInf <- any(dblValuesLower == "inf" | dblValuesLower == "-inf")

        if (hasInf) {
          # Column has infinity - convert to character with Unicode infinity symbol
          charValues <- dblValues
          charValues[dblValuesLower == "nan"] <- NA_character_
          charValues[dblValuesLower == "inf"] <- "\u221e"  # ∞
          charValues[dblValuesLower == "-inf"] <- "-\u221e"  # -∞

          # Try to convert non-special values to maintain precision display
          normalIdx <- !dblValuesLower %in% c("nan", "inf", "-inf")
          # Keep as character since the column has infinity
          result[[colName]] <- charValues
        } else {
          # No infinity - convert to numeric
          numValues <- suppressWarnings(as.numeric(dblValues))
          numValues[dblValuesLower == "nan"] <- NA_real_
          result[[colName]] <- numValues
        }
      } else {
        result[[colName]] <- dblValues
      }
    }
  }

  return(result)
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

preloadDataset <- function(datasetPathOrObject, options, encodedDataset = FALSE) {

  if (is.null(datasetPathOrObject)) {
    .setInternal("preloadedDataset", data.frame())
    return()
  }

  dataset <- loadCorrectDataset(datasetPathOrObject)

  # If encodedDataset is TRUE, the dataset is already encoded and typed correctly
  # so we skip the column name repair and type detection logic
  if (!encodedDataset) {
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

    # remove any duplicated variables for now
    nonDuplicatedIdx <- !duplicated(variables)
    variables <- variables[nonDuplicatedIdx]
    types     <- types[nonDuplicatedIdx]

    dataset <- convertToTypes(dataset[variables], types, datasetPathOrObject)
  }

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
           "ordinal" = as.ordered(dataset[[i]]),
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


#' Encode Options and Dataset for JASP Analysis
#'
#' This function processes options with `.types` properties and creates an
#' encoded version of both the options and the dataset. Variables are encoded
#' to generic names like "jaspColumn1", "jaspColumn2", etc., and the dataset
#' is filtered and formatted according to the specified types.
#'
#' @param options A named list of analysis options, typically from \code{analysisOptions()}.
#' @param dataset A data.frame or the name/path of a dataset to be encoded.
#'
#' @return A list with three components:
#' \itemize{
#'   \item \code{options}: The encoded options with variable names replaced by "jaspColumnN".
#'   \item \code{dataset}: The encoded dataset containing only the relevant columns,
#'     renamed and formatted according to their types.
#'   \item \code{encodingMap}: A data.frame with columns \code{original}, \code{encoded},
#'     and \code{type} showing the mapping from original variable names to encoded names.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Scans all options for those with a parallel \code{.types} entry
#'     (e.g., \code{variables} and \code{variables.types}).
#'   \item Extracts unique variable-type combinations.
#'   \item Creates an encoding map from original names to "jaspColumn1", "jaspColumn2", etc.
#'   \item Replaces all variable references in the options with their encoded names.
#'   \item Subsets and transforms the dataset to contain only the encoded columns,
#'     applying type coercion:
#'     \itemize{
#'       \item \code{"nominal"}: Converted to factor via \code{as.factor()}.
#'       \item \code{"ordinal"}: Converted to ordered factor.
#'       \item \code{"scale"}: Converted to numeric via \code{as.numeric()}.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' options <- analysisOptions("BinomialTest")
#' options$variables <- "contBinom"
#' options$variables.types <- "nominal"
#'
#' result <- encodeOptionsAndDataset(options, "debug.csv")
#' # result$options$variables is now "jaspColumn1"
#' # result$dataset has column "jaspColumn1" as a factor
#' # result$encodingMap shows the mapping
#' }
#'
#' @export
encodeOptionsAndDataset <- function(options, dataset) {

  # Handle NULL dataset (analysis doesn't require data)
  if (is.null(dataset)) {
    return(list(
      options     = options,
      dataset     = NULL,
      encodingMap = data.frame(original = character(0), encoded = character(0), type = character(0))
    ))
  }

  # Load the dataset if it's a path or name
  dataset <- loadCorrectDataset(dataset)
  allColumnNames <- colnames(dataset)

  # Step 1 & 2: Find all variable-type pairs from options
  varTypePairs <- extractVariableTypePairs(options, allColumnNames)

  if (nrow(varTypePairs) == 0) {
    warning("No variable-type pairs found in options. Returning original options and dataset.")
    return(list(
      options     = options,
      dataset     = dataset,
      encodingMap = data.frame(original = character(0), encoded = character(0), type = character(0))
    ))
  }

  # Step 3: Keep only unique variable-type combinations and create encoding map
  uniquePairs <- unique(varTypePairs)
  uniquePairs$encoded <- paste0("jaspColumn", seq_len(nrow(uniquePairs)))

  encodingMap <- uniquePairs[, c("variable", "encoded", "type")]
  names(encodingMap)[1] <- "original"

  # Step 4: Encode the options
  encodedOptions <- encodeOptionsWithMap(options, encodingMap, allColumnNames)

  # Step 5: Create the encoded dataset
  encodedDataset <- createEncodedDataset(dataset, encodingMap)

  return(list(
    options     = encodedOptions,
    dataset     = encodedDataset,
    encodingMap = encodingMap
  ))
}


#' Extract Variable-Type Pairs from Options
#'
#' Internal function that scans options for variable references with associated types.
#'
#' @param options The options list.
#' @param allColumnNames Vector of valid column names in the dataset.
#'
#' @return A data.frame with columns \code{variable} and \code{type}.
#' @keywords internal
extractVariableTypePairs <- function(options, allColumnNames) {

  result <- data.frame(variable = character(0), type = character(0), stringsAsFactors = FALSE)

  # Find all options that have a parallel .types entry
  optionNames <- names(options)
  optionNames <- optionNames[!grepl("\\.types$", optionNames) & optionNames != ".meta"]

  for (nm in optionNames) {
    typesKey <- paste0(nm, ".types")

    if (typesKey %in% names(options)) {
      # This option has a .types entry
      values <- options[[nm]]
      types  <- options[[typesKey]]

      pairs <- extractPairsFromValueAndType(values, types, allColumnNames)
      if (nrow(pairs) > 0) {
        result <- rbind(result, pairs)
      }
    }
  }

  return(result)
}


#' Extract Pairs from Value and Type Structures
#'
#' Recursively extracts variable-type pairs from potentially nested value and type structures.
#'
#' @param values The values (can be character vector, list, or nested structure).
#' @param types The parallel types structure.
#' @param allColumnNames Vector of valid column names.
#'
#' @return A data.frame with columns \code{variable} and \code{type}.
#' @keywords internal
extractPairsFromValueAndType <- function(values, types, allColumnNames) {

  result <- data.frame(variable = character(0), type = character(0), stringsAsFactors = FALSE)

  # Simple case: both are character vectors of same length

  if (is.character(values) && is.character(types) && length(values) == length(types)) {
    # Filter to only include valid column names
    validIdx <- values %in% allColumnNames
    if (any(validIdx)) {
      result <- data.frame(
        variable = values[validIdx],
        type     = types[validIdx],
        stringsAsFactors = FALSE
      )
    }
    return(result)
  }

  # Simple case: values is a single character matching a column name
  if (is.character(values) && length(values) == 1 && values %in% allColumnNames) {
    if (is.character(types) && length(types) == 1) {
      return(data.frame(variable = values, type = types, stringsAsFactors = FALSE))
    }
  }

  # Complex case: both are lists (parallel structure)
  if (is.list(values) && is.list(types) && length(values) == length(types)) {
    for (i in seq_along(values)) {
      subPairs <- extractPairsFromValueAndType(values[[i]], types[[i]], allColumnNames)
      if (nrow(subPairs) > 0) {
        result <- rbind(result, subPairs)
      }
    }
    return(result)
  }

  # Named list case: match by names
  if (is.list(values) && !is.null(names(values)) && is.list(types) && !is.null(names(types))) {
    commonNames <- intersect(names(values), names(types))
    for (nm in commonNames) {
      subPairs <- extractPairsFromValueAndType(values[[nm]], types[[nm]], allColumnNames)
      if (nrow(subPairs) > 0) {
        result <- rbind(result, subPairs)
      }
    }
    return(result)
  }

  return(result)
}


#' Encode Options Using Encoding Map
#'
#' Replaces variable names in options with their encoded equivalents.
#'
#' @param options The options list.
#' @param encodingMap Data.frame with columns \code{original}, \code{encoded}, \code{type}.
#' @param allColumnNames Vector of valid column names.
#'
#' @return The options list with encoded variable names.
#' @keywords internal
encodeOptionsWithMap <- function(options, encodingMap, allColumnNames) {

  # Create lookup from original to encoded
  lookup <- stats::setNames(encodingMap$encoded, encodingMap$original)

  # Recursively encode values
  encodeValue <- function(x) {
    if (is.character(x)) {
      # Replace any values that match column names in our encoding map
      idx <- x %in% names(lookup)
      if (any(idx)) {
        x[idx] <- lookup[x[idx]]
      }
      return(x)
    } else if (is.list(x)) {
      # Recursively process list elements
      for (i in seq_along(x)) {
        x[[i]] <- encodeValue(x[[i]])
      }
      return(x)
    } else {
      return(x)
    }
  }

  # Process all options except .meta and .types entries
  optionNames <- names(options)
  for (nm in optionNames) {
    if (nm == ".meta" || grepl("\\.types$", nm)) {
      next
    }
    options[[nm]] <- encodeValue(options[[nm]])
  }

  return(options)
}


#' Create Encoded Dataset
#'
#' Creates a new dataset with encoded column names and proper type coercion.
#'
#' @param dataset The original dataset.
#' @param encodingMap Data.frame with columns \code{original}, \code{encoded}, \code{type}.
#'
#' @return A data.frame with encoded column names and proper types.
#' @keywords internal
createEncodedDataset <- function(dataset, encodingMap) {

  # Create new data.frame with encoded columns
  encodedDataset <- data.frame(row.names = seq_len(nrow(dataset)))

  for (i in seq_len(nrow(encodingMap))) {
    origName    <- encodingMap$original[i]
    encodedName <- encodingMap$encoded[i]
    colType     <- encodingMap$type[i]

    if (!origName %in% colnames(dataset)) {
      warning("Column '", origName, "' not found in dataset. Skipping.")
      next
    }

    col <- dataset[[origName]]

    # Apply type coercion
    col <- switch(colType,
      "nominal" = as.factor(col),
      "ordinal" = {
        if (is.factor(col)) {
          factor(col, levels = levels(col), ordered = TRUE)
        } else {
          factor(col, ordered = TRUE)
        }
      },
      "scale" = as.numeric(col),
      col  # default: keep as-is
    )

    encodedDataset[[encodedName]] <- col
  }

  return(encodedDataset)
}
