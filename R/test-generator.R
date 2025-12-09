makeUnitTestsFromResults <- function(results, name, dataset, options) {
  if (!is.list(results) || is.null(names(results)) || results$status == "error")
    stop("Can't make unit test from results: not a results list")

  tests <- getTests(results$results)
  if (length(tests) == 0)
    stop("Could not identify any tables or plots to test")

  output <- makeExpectations(tests, name, options, dataset)
  cat(output)
}

getTests <- function(results) {
  tests <- list()

  markResultsLocationExtractTests <- function(x) {
    if (! "list" %in% class(x))
      return(x)

    unitTestType <- NULL
    if (all(c("data", "schema") %in% names(x)))
      unitTestType <- "table"
    else if (all(c("data", "width", "height") %in% names(x)))
      unitTestType <- "plot"

    if (!is.null(unitTestType) && unitTestType == "plot" || (unitTestType == "table" && length(x[["data"]]) > 0)) {
      testid <- length(tests)
      tests[[paste0("itemToUnitTest-", testid)]] <<- list(
        title = unlist(x[["title"]]),
        id = testid,
        type = unitTestType,
        data =  ifelse(unitTestType == "table", makeTestTable(x[["data"]], print=FALSE), ""))
      x[["itemToUnitTest"]] <- testid
    }

    return(lapply(x, markResultsLocationExtractTests))
  }

  markedResults <- markResultsLocationExtractTests(results)
  if (length(tests) > 0) {
    tests <- addPathIndexToTests(tests, markedResults)
    tests <- getOneTestPerCollection(tests)
  }

  return(tests)
}

addPathIndexToTests <- function(tests, markedResults) {
  results <- unlist(markedResults)
  for (testName in names(tests)) {
    id <- tests[[testName]][["id"]]
    dotSeparatedPathName <- getTestLocationInResultsById(results, id)
    tests[[testName]][["index"]] <- normalizeTestPath(dotSeparatedPathName)
  }
  return(tests)
}

getTestLocationInResultsById <- function(results, id) {
  testIndices <- which(grepl("itemToUnitTest", names(results)))
  index <- testIndices[results[testIndices] == id]
  if (length(index) != 1)
    stop("Failed to uniquely identify test case in results")

  location <- names(results)[index]
  return(gsub("itemToUnitTest", "data", location))
}

normalizeTestPath <- function(index) {
  indexNames <- unlist(strsplit(index, ".", fixed=TRUE))
  path <- paste0('[["', paste0(indexNames, collapse='"]][["'), '"]]')

  return(path)
}

getOneTestPerCollection <- function(tests) {
  pathNames <- NULL
  for (test in tests) {
    pathNames <- c(pathNames, test$index)
  }

  firstSiblingOrUnique <- rep(TRUE, length(tests))

  purgedTests <- tests[firstSiblingOrUnique]

  return(purgedTests)
}

makeExpectations <- function(tests, name, options, dataset) {
  centralizePreamble <- FALSE
  if (length(tests) > 1)
    centralizePreamble <- TRUE

  expectations <- ""

  if (centralizePreamble) {
    preamble <- addPreambleLines(name, options, dataset)
    expectations <- paste0(preamble, "\n")
  }

  for (test in tests) {
    if (!test$type %in% c("table", "plot"))
      stop("Unknown test type extracted from results, cannot continue: ", test$type)

    expectation <- makeSingleExpectation(test, name, options, dataset, centralizePreamble)
    expectations <- paste(expectations, expectation, sep="\n\n")
  }

  return(expectations)
}

makeSingleExpectation <- function(test, name, options, dataset, centralizePreamble) {
  if (!is.character(test$title) || test$title == "") {
    test$title <- paste("titleless", test$type, test$id, sep="-")
    warning(test$type, " does not have a title, using a generic one: ", test$title, immediate.=TRUE)
  }

  openingLine <- addOpeningLine(test)

  preambleLines <- NA
  if (!centralizePreamble) {
    preambleLines <- addPreambleLines(name, options, dataset)
    preambleLines <- gsub("\n", "\n\t", paste0("\t", preambleLines))
  }

  testSpecificLines <- NA
  if (test$type == "table")
    testSpecificLines <- addTableSpecificLines(test)
  else if (test$type == "plot")
    testSpecificLines <- addPlotSpecificLines(test, name)

  closingLine <- "})"

  expectation <- paste(openingLine, preambleLines, testSpecificLines, closingLine, sep="\n")
  expectation <- gsub("NA\n", "", expectation)

  return(expectation)
}

addOpeningLine <- function(test) {
  opening <- paste0('test_that("', test$title)

  titleContainsTestType <- grepl(test$type, test$title, ignore.case=TRUE)
  if (test$type == "table")
    opening <- paste0(opening, ifelse(titleContainsTestType, '',  ' table'), ' results match", {')
  else if (test$type == "plot")
    opening <- paste0(opening, ifelse(titleContainsTestType, '', ' plot'), ' matches", {')

  return(opening)
}

addPreambleLines <- function(name, options, dataset) {
  settingOfOptions <- addOptionSpecificationLines(name, options)
  settingOfSeed <- "set.seed(1)"
  runningOfAnalysis <- addRunAnalysisLines(name, dataset)

  return(paste(settingOfOptions, settingOfSeed, runningOfAnalysis, sep="\n"))
}

addRunAnalysisLines <- function(name, dataset) {
  if (is.character(dataset))
    dataArg <- paste0('"', dataset ,'"')
  else
    dataArg <- paste0('dataset')

  readingData <- paste0('dataset <- ', paste(capture.output(dput(dataset)), collapse="\n"))

  running <- paste0('results <- runAnalysis("', name, '", ', dataArg, ', options)')

  if (is.character(dataset))
    return(running)
  else
    return(paste(readingData, running, sep="\n"))
}

addTableSpecificLines <- function(test) {
  gettingTable <- paste0('\ttable <- results[["results"]]', test$index)

  comparingTables <- paste0("\tjaspTools::expect_equal_tables(table,\n\t\t", gsub("\n", "\n\t\t", test$data), ")")

  return(paste(gettingTable, comparingTables, sep="\n"))
}

addPlotSpecificLines <- function(test, name) {
  gettingPlotName <- paste0('\tplotName <- results[["results"]]', test$index)

  gettingPlot <- paste0('\ttestPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]')

  title <- gsub("-+", "-", gsub("\\W", "-", tolower(test$title)))
  comparingPlots <- paste0('\tjaspTools::expect_equal_plots(testPlot, "', title, '")')

  return(paste(gettingPlotName, gettingPlot, comparingPlots, sep="\n"))
}

addOptionSpecificationLines <- function(name, options) {
  settingOfOptions <- paste0('options <- analysisOptions("', name, '")')

  nonDefaultOpts <- getNonDefaultOptions(name, options)
  if (length(nonDefaultOpts) > 0) {
    nonDefaults <- paste0("options$", names(nonDefaultOpts), " <- ", nonDefaultOpts, collapse="\n")
    settingOfOptions <- paste0(settingOfOptions, "\n", nonDefaults)
  }

  return(settingOfOptions)
}

getNonDefaultOptions <- function(name, options) {
  defaultOpts <- analysisOptions(name)
  if (!is.list(defaultOpts) || is.null(names(defaultOpts)))
    stop("Couldn't find the default analysis options for this analysis")

  nonDefaultOpts <- NULL
  for (optName in names(options)) {
    optValue <- options[[optName]]
    if (!isTRUE(all.equal(defaultOpts[[optName]], optValue))) {
      options[[optName]] <- prepOptionValueForPrinting(optValue)
      nonDefaultOpts <- c(nonDefaultOpts, options[optName])
    }
  }

  return(nonDefaultOpts)
}

prepOptionValueForPrinting <- function(value) {
  if (is.list(value))
    result <- paste(capture.output(dput(value)), collapse="\n")
  else if (is.character(value) && length(value) == 1 && !startsWith(value, "\""))
    result <- paste0("\"", value, "\"")
  else
    result <- value

  return(result)
}

#' Create test files from JASP example files
#'
#' \code{makeTestFromExamples} transforms a folder of JASP example files into unit test files.
#'
#' @param path String path to directory containing JASP example files. All .jasp files in this directory will be processed.
#' @param sanitize Logical. If TRUE, sanitizes test filenames by replacing non-word characters with hyphens. If FALSE (default), preserves original spacing and characters in filenames.
#'
#' @details
#' This function processes all .jasp files in the specified directory and generates corresponding test files
#' in the module's tests/testthat directory. Each JASP file becomes a single test file named
#' "test-example-{basename}.R", where {basename} is the original JASP filename without the .jasp extension.
#'
#' If a JASP file contains multiple analyses, they are included as separate test_that() blocks within
#' the same test file. Each test block is self-contained with its own options, seed setting, and runAnalysis call.
#'
#' The function locates the appropriate module directory for each analysis and creates the tests/testthat
#' directory if it doesn't exist.
#'
#' **Prerequisites:**
#' - \code{setupJaspTools()} must be run before using this function to configure module paths
#' - Packages 'DBI' and 'RSQLite' are required for extracting data from JASP files
#' - The module(s) containing the analyses must be available in \code{getPkgOption("module.dirs")}
#'
#' @return Invisibly returns a character vector of created test file paths.
#'
#' @examples
#' \dontrun{
#' # Setup jaspTools first
#' library(jaspTools)
#' setupJaspTools()
#' setPkgOption("module.dirs", "path/to/your/module")
#'
#' # Create tests from all JASP files in a directory
#' makeTestFromExamples("path/to/examples")
#'
#' # Sanitize filenames (e.g., "My Example.jasp" -> "test-example-My-Example.R")
#' makeTestFromExamples("path/to/examples", sanitize = TRUE)
#' }
#'
#' @export makeTestFromExamples
makeTestFromExamples <- function(path, sanitize = FALSE) {
  if (!dir.exists(path))
    stop("The specified path does not exist: ", path)

  # Find all JASP files in the directory
  jaspFiles <- list.files(path, pattern = "\\.jasp$", full.names = TRUE)

  if (length(jaspFiles) == 0) {
    warning("No .jasp files found in directory: ", path)
    return(invisible(character(0)))
  }

  createdFiles <- character(0)

  # Process each JASP file
  for (jaspFile in jaspFiles) {
    message("Processing: ", basename(jaspFile))

    # Extract all analyses from the JASP file
    # Call analysisOptionsFromJASPfile directly to avoid the bug in analysisOptions()
    # where it checks for JSON regex pattern before checking if source is a file path
    optionsList <- tryCatch({
      analysisOptionsFromJASPfile(jaspFile)
    }, error = function(e) {
      warning("Failed to extract options from ", basename(jaspFile), ": ", e$message)
      return(NULL)
    })

    if (is.null(optionsList))
      next

    # Ensure optionsList is always a list of options (even if single analysis)
    # Check if it's a single options list (has analysisName attribute) or a list of option lists
    if (!is.null(attr(optionsList, "analysisName"))) {
      # Single analysis - wrap it in a list
      optionsList <- list(optionsList)
    }

    # Extract the actual data from the JASP file
    # This returns a data.frame or stops with error if extraction fails
    datasetToUse <- extractDatasetFromJASPfile(jaspFile, exportDir = NULL)

    # Collect all test code for this JASP file
    allTestCode <- character(0)

    # Process each analysis in the JASP file
    for (i in seq_along(optionsList)) {
      options <- optionsList[[i]]
      analysisName <- attr(options, "analysisName")

      if (is.null(analysisName)) {
        warning("Analysis at index ", i, " in ", basename(jaspFile), " has no name, skipping")
        next
      }

      message("  - Generating test for analysis: ", analysisName)

      # Run the analysis to get results
      results <- tryCatch({
        runAnalysis(analysisName, dataset = datasetToUse, options = options,
                   view = FALSE, quiet = TRUE)
      }, error = function(e) {
        warning("Failed to run analysis ", analysisName, " from ", basename(jaspFile), ": ", e$message)
        return(NULL)
      })

      if (is.null(results))
        next

      # Generate test expectations
      tests <- tryCatch({
        getTests(results$results)
      }, error = function(e) {
        warning("Failed to extract tests from ", analysisName, ": ", e$message)
        return(NULL)
      })

      if (is.null(tests) || length(tests) == 0) {
        warning("No tests generated for ", analysisName, " in ", basename(jaspFile))
        next
      }

      # Generate test code for this analysis
      testCode <- tryCatch({
        makeExpectations(tests, analysisName, options, datasetToUse)
      }, error = function(e) {
        warning("Failed to create expectations for ", analysisName, ": ", e$message)
        return(NULL)
      })

      if (!is.null(testCode)) {
        allTestCode <- c(allTestCode, testCode)
      }
    }

    # Write test file if we have any test code
    if (length(allTestCode) > 0) {
      # Get first analysis name to determine module path
      firstAnalysisName <- attr(optionsList[[1]], "analysisName")
      modulePath <- tryCatch({
        getModulePathFromRFunction(firstAnalysisName)
      }, error = function(e) {
        warning("Could not locate module for ", firstAnalysisName, ": ", e$message)
        return(NULL)
      })

      if (is.null(modulePath)) {
        warning("Skipping test file creation for ", basename(jaspFile), " - no module path found")
        next
      }

      # Create tests/testthat directory if it doesn't exist
      testsDir <- file.path(modulePath, "tests", "testthat")
      if (!dir.exists(testsDir)) {
        dir.create(testsDir, recursive = TRUE)
      }

      # Create test filename
      baseName <- tools::file_path_sans_ext(basename(jaspFile))
      if (sanitize) {
        baseName <- gsub("\\W", "-", baseName)
        baseName <- gsub("-+", "-", baseName)
      }
      testFileName <- paste0("test-example-", baseName, ".R")
      testFilePath <- file.path(testsDir, testFileName)

      # Combine all test code
      fullTestContent <- paste(allTestCode, collapse = "\n\n")

      # Write test file
      tryCatch({
        writeLines(fullTestContent, testFilePath)
        message("Created test file: ", testFilePath)
        createdFiles <- c(createdFiles, testFilePath)
      }, error = function(e) {
        warning("Failed to write test file ", testFilePath, ": ", e$message)
      })
    }
  }

  if (length(createdFiles) > 0) {
    message("\nSuccessfully created ", length(createdFiles), " test file(s)")
  } else {
    warning("No test files were created")
  }

  return(invisible(createdFiles))
}

#' Extract dataset from a JASP file
#'
#' \code{extractDatasetFromJASPfile} extracts the data.frame from a JASP file's internal SQLite database,
#' properly handling factor columns with their labels.
#'
#' @param file String path to a .jasp file.
#' @param exportDir Optional string path to a directory. If provided, the dataset will be saved as a CSV file
#'   in this directory and the path to the CSV will be returned. If NULL (default), returns a data.frame directly.
#'
#' @details
#' This function extracts the dataset stored in a JASP file by:
#' \itemize{
#'   \item Unpacking the .jasp archive (which is a zip file)
#'   \item Reading the internal.sqlite database
#'   \item Converting Column_N_DBL and Column_N_INT columns to properly named columns
#'   \item Mapping factor levels from the Labels table to create proper R factors
#'   \item Handling both explicitly nominal columns and columns with label mappings
#' }
#'
#' Factor columns are automatically detected by checking if:
#' \itemize{
#'   \item The column is marked as "nominal" in the Columns table, OR
#'   \item The column has labels defined in the Labels table AND the _DBL column contains only "nan" values
#' }
#'
#' **Prerequisites:**
#' - Packages 'DBI' and 'RSQLite' are required and will be loaded automatically
#' - Package 'archive' is required for extracting the .jasp file
#'
#' @return If \code{exportDir} is NULL, returns a data.frame with the extracted dataset.
#'   If \code{exportDir} is provided, returns the file path to the exported CSV file.
#'
#' @examples
#' \dontrun{
#' # Extract as data.frame
#' dataset <- extractDatasetFromJASPfile("path/to/file.jasp")
#' str(dataset)
#'
#' # Export to CSV
#' csvPath <- extractDatasetFromJASPfile("path/to/file.jasp", exportDir = "output")
#' }
#'
#' @export extractDatasetFromJASPfile
extractDatasetFromJASPfile <- function(file, exportDir = NULL) {
  # Extract the actual dataset from a JASP file's internal.sqlite database
  # Returns either a data.frame (if exportDir is NULL) or path to exported CSV
  
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Packages 'DBI' and 'RSQLite' are required to extract data from JASP files. ",
         "Install them with: install.packages(c('DBI', 'RSQLite'))")
  }
  
  # Create temporary directory for extraction
  tmpDir <- tempfile("jasp_extract_")
  dir.create(tmpDir)
  on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
  
  # Extract JASP archive to temp directory
  archive::archive_extract(file, dir = tmpDir)
  
  # Connect to SQLite database
  dbPath <- file.path(tmpDir, "internal.sqlite")
  if (!file.exists(dbPath)) {
    stop("No internal.sqlite found in JASP file: ", basename(file))
  }
  
  db <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(db), add = TRUE)
  
  # Find the dataset table (typically DataSet_1, DataSet_2, etc.)
  tables <- DBI::dbListTables(db)
  datasetTables <- grep("^DataSet_[0-9]+$", tables, value = TRUE)
  
  if (length(datasetTables) == 0) {
    stop("No DataSet table found in JASP SQLite database for file: ", basename(file))
  }
  
  # Use the first dataset table (typically DataSet_1)
  datasetTable <- datasetTables[1]
  
  # Read the dataset
  dataset <- DBI::dbReadTable(db, datasetTable)
  
  # Get column metadata to determine column types and names
  if ("Columns" %in% tables) {
    columnsMeta <- DBI::dbGetQuery(db, "SELECT name, colIdx, columnType FROM Columns ORDER BY colIdx")
    
    # Get label data for mapping factor levels
    labels <- NULL
    labelsPerColumn <- list()
    if ("Labels" %in% tables) {
      labels <- DBI::dbGetQuery(db, "SELECT columnId, value, label FROM Labels ORDER BY columnId, value")
      # Group labels by columnId for easier lookup
      for (colId in unique(labels$columnId)) {
        labelsPerColumn[[as.character(colId)]] <- labels[labels$columnId == colId, ]
      }
    }
    
    # Process each column
    for (i in seq_len(nrow(columnsMeta))) {
      colName <- columnsMeta$name[i]
      colType <- columnsMeta$columnType[i]
      colIdx <- columnsMeta$colIdx[i] + 1  # colIdx is 0-based, R is 1-based
      
      dblColName <- paste0("Column_", colIdx, "_DBL")
      intColName <- paste0("Column_", colIdx, "_INT")
      
      # Check if this column has labels defined (indicating it might be nominal even if marked as scale)
      hasLabels <- as.character(colIdx) %in% names(labelsPerColumn) && 
                   nrow(labelsPerColumn[[as.character(colIdx)]]) > 0
      
      # For nominal columns OR columns with labels where _DBL contains "nan", use _INT column and map to labels
      if (intColName %in% colnames(dataset)) {
        dblValues <- if (dblColName %in% colnames(dataset)) dataset[[dblColName]] else NULL
        intValues <- dataset[[intColName]]
        
        # Check if _DBL column contains only "nan" strings (indicating categorical data stored as integers)
        dblIsNan <- !is.null(dblValues) && is.character(dblValues) && all(dblValues == "nan" | is.na(dblValues))
        
        # Use integer column with label mapping if:
        # 1. Column is marked as nominal, OR
        # 2. Column has labels defined AND _DBL contains only "nan"
        if ((!is.na(colType) && colType == "nominal") || (hasLabels && dblIsNan)) {
          colLabels <- labelsPerColumn[[as.character(colIdx)]]
          
          if (!is.null(colLabels) && nrow(colLabels) > 0) {
            # Create a mapping from integer value to label
            labelMap <- setNames(colLabels$label, colLabels$value)
            
            # Map the integer values to labels
            factorValues <- labelMap[as.character(intValues)]
            
            # Create a factor with the correct levels (ordered by value in Labels table)
            orderedLevels <- colLabels$label[order(colLabels$value)]
            dataset[[colName]] <- factor(factorValues, levels = orderedLevels)
          } else {
            # No labels found, just use the integer values as-is
            dataset[[colName]] <- intValues
          }
        }
        # For scale columns with numeric data, use _DBL column
        else if (!is.null(dblValues) && !dblIsNan) {
          dataset[[colName]] <- dblValues
        }
        # Fallback: if _DBL doesn't exist or is all nan, use _INT
        else {
          dataset[[colName]] <- intValues
        }
      }
      # If only _DBL exists (no _INT), use it
      else if (dblColName %in% colnames(dataset)) {
        dataset[[colName]] <- dataset[[dblColName]]
      }
    }
  }
  
  # Remove JASP-internal columns (rowNumber, Filter_*, Column_*_INT, Column_*_DBL)
  internalCols <- grep("^(rowNumber|Filter_|Column_.*_(INT|DBL))$", colnames(dataset))
  if (length(internalCols) > 0) {
    dataset <- dataset[, -internalCols, drop = FALSE]
  }
  
  # If exportDir is provided, save to CSV and return path
  if (!is.null(exportDir)) {
    if (!dir.exists(exportDir)) {
      dir.create(exportDir, recursive = TRUE)
    }
    
    # Generate CSV filename from JASP filename
    baseName <- tools::file_path_sans_ext(basename(file))
    csvPath <- file.path(exportDir, paste0(baseName, ".csv"))
    
    # Write CSV
    utils::write.csv(dataset, csvPath, row.names = FALSE)
    message("  - Extracted data to: ", csvPath)
    
    return(csvPath)
  } else {
    # Return the dataset directly
    return(dataset)
  }
}
