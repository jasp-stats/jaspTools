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

devcat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                   append = FALSE) {

  if (getOption("jasptools_devcat", FALSE))
    return(cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append))
  invisible(NULL)
}
