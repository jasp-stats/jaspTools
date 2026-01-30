#' Create test files from JASP example files
#'
#' \code{makeTestsFromExamples} transforms JASP example files into unit test files.
#'
#' @param path Optional string path to a directory containing JASP example files.
#'   If provided, the JASP files at this path will be copied to the module's
#'   \code{examples/} folder before generating tests. If missing, the function processes
#'   JASP files already present in the module's \code{examples/} folder.
#' @param module.dir String path to the module directory. If missing, uses the current
#'   working directory.
#' @param sanitize Logical. If TRUE, sanitizes test filenames by replacing non-word characters
#'   with hyphens. If FALSE (default), preserves original spacing and characters in filenames.
#' @param overwrite Logical. If TRUE, overwrites existing test files. If FALSE (default),
#'   skips files that already exist.
#' @param forceEncode Optional character vector of option names that should be forcibly
#'   encoded using regular expression replacement. This is useful for options like
#'   \code{model} that contain variable names embedded in strings (e.g., formula syntax
#'   "A~B") but do not have a parallel \code{.types} entry. These options will have all
#'   column names replaced with their encoded equivalents using word-boundary-aware regex.
#'
#' @details
#' This function processes JASP example files and generates corresponding test files in
#' the module's \code{tests/testthat} directory. Each JASP file becomes a single test file
#' named "test-example-{basename}.R", where {basename} is the original JASP filename without
#' the .jasp extension.
#'
#' The JASP example files must be located in the module's \code{examples/} folder for the
#' generated tests to work at runtime. When \code{path} is provided, files are automatically
#' copied to the module's \code{examples/} folder.
#'
#' If a JASP file contains multiple analyses, they are included as separate \code{test_that()}
#' blocks within the same test file.
#'
#' **Prerequisites:**
#' - \code{setupJaspTools()} must be run before using this function
#' - Packages 'DBI' and 'RSQLite' are required for extracting data from JASP files
#'
#' @return Invisibly returns a character vector of created/processed test file paths.
#'
#' @examples
#' \dontrun{
#' # Setup jaspTools first
#' library(jaspTools)
#' setupJaspTools()
#'
#' # Create tests from JASP files already in the module's examples/ folder
#' # (uses working directory as module)
#' makeTestsFromExamples()
#'
#' # Specify the module directory explicitly
#' makeTestsFromExamples(module.dir = "path/to/your/module")
#'
#' # Import JASP files from another directory, copy to examples/, and generate tests
#' makeTestsFromExamples(path = "path/to/jasp/files", module.dir = "path/to/module")
#'
#' # Overwrite existing test files
#' makeTestsFromExamples(overwrite = TRUE)
#'
#' # Force encode 'model' option for analyses with embedded variable names
#' makeTestsFromExamples(forceEncode = "model")
#' }
#'
#' @export makeTestsFromExamples
makeTestsFromExamples <- function(path, module.dir, sanitize = FALSE, overwrite = FALSE,
                                  forceEncode = NULL) {
  # Determine module directory

  if (missing(module.dir)) {
    module.dir <- getwd()
    message("Using working directory as module: ", module.dir)
  }

  if (!dir.exists(module.dir)) {
    stop("Module directory does not exist: ", module.dir)
  }

  pkgAnalyses <- NULL
  if (isBinaryPackage(module.dir)) {
    qmlPath <- file.path(module.dir, "Description.qml")
  } else {
    qmlPath <- file.path(module.dir, "inst", "Description.qml")
  }

  if (file.exists(qmlPath)) {
    qmlContent <- parseDescriptionQmlFromPath(qmlPath)
    pkgAnalyses <- setdiff(names(qmlContent), "Description")
  } else {
    stop("Description.qml not found at path: ", qmlPath, 
         ". Make sure the module contains inst/Description.qml (source) or Description.qml (installed).")
  }


  # Collect JASP files to process
  jaspFiles <- character(0)
  copyToExamples <- FALSE

  if (!missing(path)) {
    # Path provided: collect files from this path and mark them for copying
    if (!dir.exists(path)) {
      stop("Directory does not exist: ", path)
    }

    jaspFiles <- list.files(path, pattern = "\\.jasp$", full.names = TRUE)
    if (length(jaspFiles) == 0) {
      stop("No .jasp files found in directory: ", path)
    }

    copyToExamples <- TRUE
    message("Will copy JASP files to module examples/ folder before generating tests.\n")
  } else {
    # No path provided: collect files from module's examples/ folder
    examplesDir <- file.path(module.dir, "examples")
    if (dir.exists(examplesDir)) {
      jaspFiles <- list.files(examplesDir, pattern = "\\.jasp$", full.names = TRUE)
    }

    if (length(jaspFiles) == 0) {
      stop("No .jasp files found in module's examples/ folder: ", examplesDir)
    }
  }

  createdFiles <- character(0)
  skippedFiles <- character(0)
  copiedFiles <- character(0)

  for (jaspFile in jaspFiles) {
    message("Processing: ", basename(jaspFile))

    tryCatch(
      {
        result <- makeTestsFromSingleJASPFile(jaspFile,
          module.dir = module.dir,
          sanitize = sanitize, overwrite = overwrite,
          copyToExamples = copyToExamples,
          pkgAnalyses = pkgAnalyses,
          forceEncode = forceEncode
        )
        if (!is.null(result)) {
          if (!is.null(attr(result, "copiedTo"))) {
            copiedFiles <- c(copiedFiles, attr(result, "copiedTo"))
          }
          if (isTRUE(attr(result, "skipped"))) {
            skippedFiles <- c(skippedFiles, result)
            message("  Skipped (already exists): ", result)
          } else {
            createdFiles <- c(createdFiles, result)
            message("  Created: ", result)
          }
        } else {
          message("  No tests created (all analyses were skipped)")
        }
      },
      error = function(e) {
        warning("Failed to process ", basename(jaspFile), ": ", e$message, call. = FALSE)
      }
    )
  }

  if (length(createdFiles) == 0 && length(skippedFiles) == 0) {
    warning("No test files were created.")
  } else {
    if (length(copiedFiles) > 0) {
      message("\nCopied ", length(copiedFiles), " JASP file(s) to module examples/ folder(s).")
    }
    if (length(createdFiles) > 0) {
      message("Created ", length(createdFiles), " test file(s).")
    }
    if (length(skippedFiles) > 0) {
      message("Skipped ", length(skippedFiles), " existing test file(s). Use overwrite = TRUE to regenerate.")
    }
  }

  invisible(createdFiles)
}


#' Create a test file from a single JASP file
#'
#' Internal function that processes a single JASP file and generates a test file.
#'
#' @param jaspFile Path to the .jasp file.
#' @param module.dir Path to the module directory.
#' @param sanitize Whether to sanitize the filename.
#' @param overwrite Whether to overwrite existing test files.
#' @param copyToExamples Whether to copy the JASP file to the module's examples folder.
#'
#' @param pkgAnalyses Optional character vector of allowed analysis names for this module.
#'   If provided, analyses not in this list will be skipped.
#' @param forceEncode Optional character vector of option names to force-encode via regex.
#'
#' @return The path to the created test file (with attr "skipped" if skipped,
#'   and attr "copiedTo" if copied), or NULL if no tests were generated
#'   (e.g., all analyses were skipped or processing failed).
#' @keywords internal
makeTestsFromSingleJASPFile <- function(jaspFile, module.dir, sanitize = FALSE,
                                        overwrite = FALSE, copyToExamples = FALSE,
                                        pkgAnalyses = NULL, forceEncode = NULL) {
  # Extract options from the JASP file
  allOptions <- analysisOptions(jaspFile)

  # Ensure it's a list of options (even if single analysis)
  if (!is.null(attr(allOptions, "analysisName"))) {
    # Single analysis - wrap in list
    allOptions <- list(allOptions)
  }

  if (length(allOptions) == 0) {
    stop("No analyses found in JASP file")
  }

  # Extract dataset from the JASP file
  dataset <- extractDatasetFromJASPFile(jaspFile)

  # Get the base name for the test file
  baseName <- tools::file_path_sans_ext(basename(jaspFile))
  if (sanitize) {
    sanitizedName <- gsub("\\W+", "-", baseName)
    sanitizedName <- gsub("^-+|-+$", "", sanitizedName) # trim leading/trailing hyphens
  } else {
    sanitizedName <- baseName
  }

  # Track if we copied the file
  copiedTo <- NULL

  # Copy JASP file to module's examples folder if requested
  if (copyToExamples) {
    examplesDir <- file.path(module.dir, "examples")
    if (!dir.exists(examplesDir)) {
      dir.create(examplesDir, recursive = TRUE)
      message("  Created directory: ", examplesDir)
    }
    destFile <- file.path(examplesDir, basename(jaspFile))
    file.copy(jaspFile, destFile, overwrite = TRUE)
    copiedTo <- destFile
    message("  Copied to: ", destFile)
  }

  # Create tests/testthat directory if needed
  testDir <- file.path(module.dir, "tests", "testthat")

  if (!dir.exists(testDir)) {
    dir.create(testDir, recursive = TRUE)
    message("  Created directory: ", testDir)
  }

  # Determine test file path using "test-example-Name.R" format
  testFileName <- paste0("test-example-", sanitizedName, ".R")
  testFilePath <- file.path(testDir, testFileName)

  # Check if file already exists
  if (file.exists(testFilePath) && !overwrite) {
    result <- testFilePath
    attr(result, "skipped") <- TRUE
    attr(result, "copiedTo") <- copiedTo
    return(result)
  }

  # Run each analysis and generate test expectations
  testBlocks <- list()

  for (i in seq_along(allOptions)) {
    opts <- allOptions[[i]]
    analysisName <- attr(opts, "analysisName")

    if (is.null(analysisName)) {
      warning("Analysis ", i, " has no name, skipping")
      next
    }

    if (!is.null(pkgAnalyses) && !analysisName %in% pkgAnalyses) {
      message("Analysis ", analysisName, " skipped because it is not exported from the current module.")
      next
    }

    message("  Running analysis ", i, "/", length(allOptions), ": ", analysisName)

    # Encode options and dataset
    encoded <- encodeOptionsAndDataset(opts, dataset, forceEncode = forceEncode)

    # Run the analysis to get results
    tryCatch(
      {
        set.seed(1)
        results <- runAnalysis(analysisName, encoded$dataset, encoded$options,
          view = FALSE, quiet = TRUE, encodedDataset = TRUE
        )

        # Generate test block with expectations from results
        testBlock <- generateExampleTestBlock(
          analysisName = analysisName,
          analysisIndex = i,
          totalAnalyses = length(allOptions),
          jaspFileName = basename(jaspFile),
          results = results,
          forceEncode = forceEncode
        )

        testBlocks <- c(testBlocks, list(testBlock))
      },
      error = function(e) {
        warning("  Failed to run analysis ", analysisName, ": ", e$message, call. = FALSE)
        # Generate a basic test block that just checks for no error
        testBlock <- generateExampleTestBlockBasic(
          analysisName = analysisName,
          analysisIndex = i,
          totalAnalyses = length(allOptions),
          jaspFileName = basename(jaspFile),
          forceEncode = forceEncode
        )
        testBlocks <<- c(testBlocks, list(testBlock))
      }
    )
  }

  # Check if any tests were generated (all analyses might have been skipped)
  if (length(testBlocks) == 0) {
    return(NULL)
  }

  # Generate the test file content
  testContent <- generateExampleTestFileContent(baseName, sanitizedName, testBlocks)

  # Write the test file
  writeLines(testContent, testFilePath)

  # Add copiedTo attribute if applicable
  attr(testFilePath, "copiedTo") <- copiedTo

  return(testFilePath)
}


#' Generate test file content for example-based tests
#'
#' @param baseName Original JASP file name without extension.
#' @param sanitizedName Sanitized name for use in code.
#' @param testBlocks List of test block strings.
#'
#' @return Character string with complete test file content.
#' @keywords internal
generateExampleTestFileContent <- function(baseName, sanitizedName, testBlocks) {
  lines <- character(0)

  # Header
  lines <- c(lines, paste0('context("Example: ', baseName, '")'))
  lines <- c(lines, "")

  # Helper comment
  lines <- c(lines, "# This test file was auto-generated from a JASP example file.")
  lines <- c(lines, "# The JASP file is stored in the module's examples/ folder.")
  lines <- c(lines, "")

  # Add each test block
  for (block in testBlocks) {
    lines <- c(lines, block, "")
  }

  return(paste(lines, collapse = "\n"))
}


#' Generate a test block with expectations from analysis results
#'
#' @param analysisName Name of the analysis function.
#' @param analysisIndex Index of this analysis in the JASP file.
#' @param totalAnalyses Total number of analyses in the file.
#' @param jaspFileName Name of the JASP file.
#' @param results The analysis results.
#' @param forceEncode Optional character vector of option names to force-encode via regex.
#'
#' @return Character string with the test_that block.
#' @keywords internal
generateExampleTestBlock <- function(analysisName, analysisIndex, totalAnalyses, jaspFileName, results,
                                     forceEncode = NULL) {
  # Extract tests from results
  tests <- tryCatch(
    {
      getTests(results$results)
    },
    error = function(e) {
      list()
    }
  )

  # Build the test block
  lines <- character(0)

  # Test description
  if (totalAnalyses > 1) {
    testDesc <- paste0(analysisName, " (analysis ", analysisIndex, ") results match")
  } else {
    testDesc <- paste0(analysisName, " results match")
  }

  lines <- c(lines, paste0('test_that("', testDesc, '", {'))
  lines <- c(lines, "")

  # Extract from JASP file in module's examples folder
  lines <- c(lines, "  # Load from JASP example file")
  lines <- c(lines, paste0('  jaspFile <- testthat::test_path("..", "..", "examples", "', jaspFileName, '")'))

  # Generate appropriate options extraction based on number of analyses
  if (totalAnalyses == 1) {
    # Single analysis: analysisOptions returns options directly (not in a list)
    lines <- c(lines, "  opts <- jaspTools::analysisOptions(jaspFile)")
  } else {
    # Multiple analyses: analysisOptions returns a list
    lines <- c(lines, paste0("  opts <- jaspTools::analysisOptions(jaspFile)[[", analysisIndex, "]]"))
  }

  lines <- c(lines, "  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)")
  lines <- c(lines, "")

  # Encode and run - include forceEncode if provided
  lines <- c(lines, "  # Encode and run analysis")
  if (!is.null(forceEncode) && length(forceEncode) > 0) {
    forceEncodeStr <- paste0('c("', paste(forceEncode, collapse = '", "'), '")')
    lines <- c(lines, paste0("  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = ", forceEncodeStr, ")"))
  } else {
    lines <- c(lines, "  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)")
  }
  lines <- c(lines, "  set.seed(1)")
  lines <- c(lines, paste0('  results <- jaspTools::runAnalysis("', analysisName, '", encoded$dataset, encoded$options, encodedDataset = TRUE)'))
  lines <- c(lines, "")

  # Add expectations
  if (length(tests) > 0) {
    # Add table and plot expectations
    figureNumber <- 0
    for (test in tests) {
      if (test$type == "table") {
        lines <- c(lines, paste0('  table <- results[["results"]]', test$index))
        # Format table data nicely - add proper indentation
        tableData <- gsub("\n\t", "\n    ", test$data) # Convert tabs to spaces
        lines <- c(lines, paste0("  jaspTools::expect_equal_tables(table,"))
        lines <- c(lines, paste0("    ", tableData, ")"))
        lines <- c(lines, "")
      } else if (test$type == "plot") {
        figureNumber <- figureNumber + 1
        lines <- c(lines, paste0('  plotName <- results[["results"]]', test$index))
        lines <- c(lines, '  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]')
        # Prefix with analysis-N_figure-M_name to avoid duplicates
        plotTitle <- gsub("-+", "-", gsub("\\W", "-", tolower(test$title)))
        plotTitle <- paste0("analysis-", analysisIndex, "_figure-", figureNumber, "_", plotTitle)
        lines <- c(lines, paste0('  jaspTools::expect_equal_plots(testPlot, "', plotTitle, '")'))
        lines <- c(lines, "")
      }
    }
  } else {
    # Fallback: just check no error
    lines <- c(lines, "  # Basic check - analysis runs without error")
    lines <- c(lines, '  expect_false(isTRUE(results[["status"]] == "error"),')
    lines <- c(lines, '               info = results[["results"]][["error"]])')
  }

  lines <- c(lines, "})")

  return(paste(lines, collapse = "\n"))
}


#' Generate a basic test block (fallback when analysis fails)
#'
#' @param analysisName Name of the analysis function.
#' @param analysisIndex Index of this analysis in the JASP file.
#' @param totalAnalyses Total number of analyses in the file.
#' @param jaspFileName Name of the JASP file.
#' @param forceEncode Optional character vector of option names to force-encode via regex.
#'
#' @return Character string with the test_that block.
#' @keywords internal
generateExampleTestBlockBasic <- function(analysisName, analysisIndex, totalAnalyses, jaspFileName,
                                          forceEncode = NULL) {
  lines <- character(0)

  # Test description
  if (totalAnalyses > 1) {
    testDesc <- paste0(analysisName, " (analysis ", analysisIndex, ") runs without error")
  } else {
    testDesc <- paste0(analysisName, " runs without error")
  }

  lines <- c(lines, paste0('test_that("', testDesc, '", {'))
  lines <- c(lines, "")

  # Extract from JASP file in module's examples folder
  lines <- c(lines, "  # Load from JASP example file")
  lines <- c(lines, paste0('  jaspFile <- testthat::test_path("..", "..", "examples", "', jaspFileName, '")'))

  # Generate appropriate options extraction based on number of analyses
  if (totalAnalyses == 1) {
    # Single analysis: analysisOptions returns options directly (not in a list)
    lines <- c(lines, "  opts <- jaspTools::analysisOptions(jaspFile)")
  } else {
    # Multiple analyses: analysisOptions returns a list
    lines <- c(lines, paste0("  opts <- jaspTools::analysisOptions(jaspFile)[[", analysisIndex, "]]"))
  }

  lines <- c(lines, "  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)")
  lines <- c(lines, "")

  # Encode and run - include forceEncode if provided
  lines <- c(lines, "  # Encode and run analysis")
  if (!is.null(forceEncode) && length(forceEncode) > 0) {
    forceEncodeStr <- paste0('c("', paste(forceEncode, collapse = '", "'), '")')
    lines <- c(lines, paste0("  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = ", forceEncodeStr, ")"))
  } else {
    lines <- c(lines, "  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)")
  }
  lines <- c(lines, "  set.seed(1)")
  lines <- c(lines, paste0('  results <- jaspTools::runAnalysis("', analysisName, '", encoded$dataset, encoded$options, encodedDataset = TRUE)'))
  lines <- c(lines, "")

  # Basic expectation
  lines <- c(lines, "  # Check analysis runs without error")
  lines <- c(lines, '  expect_false(isTRUE(results[["status"]] == "error"),')
  lines <- c(lines, '               info = results[["results"]][["error"]])')

  lines <- c(lines, "})")

  return(paste(lines, collapse = "\n"))
}


makeUnitTestsFromResults <- function(results, name, dataset, options) {
  if (!is.list(results) || is.null(names(results)) || results$status == "error") {
    stop("Can't make unit test from results: not a results list")
  }

  tests <- getTests(results$results)
  if (length(tests) == 0) {
    stop("Could not identify any tables or plots to test")
  }

  output <- makeExpectations(tests, name, options, dataset)
  cat(output)
}

getTests <- function(results) {
  tests <- list()

  markResultsLocationExtractTests <- function(x) {
    if (!"list" %in% class(x)) {
      return(x)
    }

    unitTestType <- NULL
    if (all(c("data", "schema") %in% names(x))) {
      unitTestType <- "table"
    } else if (all(c("data", "width", "height") %in% names(x))) {
      unitTestType <- "plot"
    }

    if (!is.null(unitTestType) && unitTestType == "plot" || (unitTestType == "table" && length(x[["data"]]) > 0)) {
      testid <- length(tests)
      tests[[paste0("itemToUnitTest-", testid)]] <<- list(
        title = unlist(x[["title"]]),
        id = testid,
        type = unitTestType,
        data = ifelse(unitTestType == "table", makeTestTable(x[["data"]], print = FALSE), "")
      )
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
  if (length(index) != 1) {
    stop("Failed to uniquely identify test case in results")
  }

  location <- names(results)[index]
  return(gsub("itemToUnitTest", "data", location))
}

normalizeTestPath <- function(index) {
  indexNames <- unlist(strsplit(index, ".", fixed = TRUE))
  path <- paste0('[["', paste0(indexNames, collapse = '"]][["'), '"]]')

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
  if (length(tests) > 1) {
    centralizePreamble <- TRUE
  }

  expectations <- ""

  if (centralizePreamble) {
    preamble <- addPreambleLines(name, options, dataset)
    expectations <- paste0(preamble, "\n")
  }

  for (test in tests) {
    if (!test$type %in% c("table", "plot")) {
      stop("Unknown test type extracted from results, cannot continue: ", test$type)
    }

    expectation <- makeSingleExpectation(test, name, options, dataset, centralizePreamble)
    expectations <- paste(expectations, expectation, sep = "\n\n")
  }

  return(expectations)
}

makeSingleExpectation <- function(test, name, options, dataset, centralizePreamble) {
  if (!is.character(test$title) || test$title == "") {
    test$title <- paste("titleless", test$type, test$id, sep = "-")
    warning(test$type, " does not have a title, using a generic one: ", test$title, immediate. = TRUE)
  }

  openingLine <- addOpeningLine(test)

  preambleLines <- NA
  if (!centralizePreamble) {
    preambleLines <- addPreambleLines(name, options, dataset)
    preambleLines <- gsub("\n", "\n\t", paste0("\t", preambleLines))
  }

  testSpecificLines <- NA
  if (test$type == "table") {
    testSpecificLines <- addTableSpecificLines(test)
  } else if (test$type == "plot") {
    testSpecificLines <- addPlotSpecificLines(test, name)
  }

  closingLine <- "})"

  expectation <- paste(openingLine, preambleLines, testSpecificLines, closingLine, sep = "\n")
  expectation <- gsub("NA\n", "", expectation)

  return(expectation)
}

addOpeningLine <- function(test) {
  opening <- paste0('test_that("', test$title)

  titleContainsTestType <- grepl(test$type, test$title, ignore.case = TRUE)
  if (test$type == "table") {
    opening <- paste0(opening, ifelse(titleContainsTestType, "", " table"), ' results match", {')
  } else if (test$type == "plot") {
    opening <- paste0(opening, ifelse(titleContainsTestType, "", " plot"), ' matches", {')
  }

  return(opening)
}

addPreambleLines <- function(name, options, dataset) {
  settingOfOptions <- addOptionSpecificationLines(name, options)
  settingOfSeed <- "set.seed(1)"
  runningOfAnalysis <- addRunAnalysisLines(name, dataset)

  return(paste(settingOfOptions, settingOfSeed, runningOfAnalysis, sep = "\n"))
}

addRunAnalysisLines <- function(name, dataset) {
  if (is.character(dataset)) {
    dataArg <- paste0('"', dataset, '"')
  } else {
    dataArg <- paste0("dataset")
  }

  readingData <- paste0("dataset <- ", paste(capture.output(dput(dataset)), collapse = "\n"))

  running <- paste0('results <- runAnalysis("', name, '", ', dataArg, ", options)")

  if (is.character(dataset)) {
    return(running)
  } else {
    return(paste(readingData, running, sep = "\n"))
  }
}

addTableSpecificLines <- function(test) {
  gettingTable <- paste0('\ttable <- results[["results"]]', test$index)

  comparingTables <- paste0("\tjaspTools::expect_equal_tables(table,\n\t\t", gsub("\n", "\n\t\t", test$data), ")")

  return(paste(gettingTable, comparingTables, sep = "\n"))
}

addPlotSpecificLines <- function(test, name) {
  gettingPlotName <- paste0('\tplotName <- results[["results"]]', test$index)

  gettingPlot <- paste0('\ttestPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]')

  title <- gsub("-+", "-", gsub("\\W", "-", tolower(test$title)))
  comparingPlots <- paste0('\tjaspTools::expect_equal_plots(testPlot, "', title, '")')

  return(paste(gettingPlotName, gettingPlot, comparingPlots, sep = "\n"))
}

addOptionSpecificationLines <- function(name, options) {
  settingOfOptions <- paste0('options <- analysisOptions("', name, '")')

  nonDefaultOpts <- getNonDefaultOptions(name, options)
  if (length(nonDefaultOpts) > 0) {
    nonDefaults <- paste0("options$", names(nonDefaultOpts), " <- ", nonDefaultOpts, collapse = "\n")
    settingOfOptions <- paste0(settingOfOptions, "\n", nonDefaults)
  }

  return(settingOfOptions)
}

getNonDefaultOptions <- function(name, options) {
  defaultOpts <- analysisOptions(name)
  if (!is.list(defaultOpts) || is.null(names(defaultOpts))) {
    stop("Couldn't find the default analysis options for this analysis")
  }

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
  if (is.list(value)) {
    result <- paste(capture.output(dput(value)), collapse = "\n")
  } else if (is.character(value) && length(value) == 1 && !startsWith(value, "\"")) {
    result <- paste0("\"", value, "\"")
  } else {
    result <- value
  }

  return(result)
}
