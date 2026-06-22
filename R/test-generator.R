#' Create test files from JASP example files
#'
#' \code{makeTestsFromExamples} transforms JASP example files into unit test files.
#'
#' @param path Optional string path to a directory containing JASP example files.
#'   If provided, the JASP files at this path will be copied to the module's
#'   \code{tests/testthat/jaspfiles/other/} folder and test files named
#'   \code{test-other-{name}.R} are generated. The \code{source} argument is
#'   ignored when \code{path} is provided.
#' @param module.dir String path to the module directory. If missing, uses the current
#'   working directory.
#' @param source Character vector specifying which source folders under
#'   \code{tests/testthat/jaspfiles/} to process. Allowed values are
#'   \code{"library"}, \code{"verified"}, and \code{"other"}. Defaults to all
#'   three when \code{overwrite = FALSE} and to \code{c("library", "other")}
#'   when \code{overwrite = TRUE}. If \code{"verified"} is included and
#'   \code{overwrite = TRUE}, the user is prompted for confirmation.
#' @param sanitize Logical. If TRUE, sanitizes test filenames by replacing non-word characters
#'   with hyphens. If FALSE (default), preserves original spacing and characters in filenames.
#' @param overwrite Logical. If TRUE, overwrites existing test files. If FALSE (default),
#'   skips files that already exist.
#' @param forceEncode Compatibility argument retained for older callers. Supplying
#'   a non-`NULL` value now aborts because generated tests run directly against
#'   the extracted options and dataset without a separate encoding step.
#'
#' @details
#' This function processes JASP example files stored under
#' \code{tests/testthat/jaspfiles/{library,verified,other}/} and generates corresponding
#' test files in the module's \code{tests/testthat} directory. Each JASP file becomes a
#' single test file named \code{test-{source}-{basename}.R}, where \code{{source}} is the
#' source folder name (\code{library}, \code{verified}, or \code{other}) and \code{{basename}}
#' is the original JASP filename without the .jasp extension.
#'
#' If a JASP file contains multiple analyses, they are included as separate \code{test_that()}
#' blocks within the same test file.
#'
#' **Prerequisites:**
#' - \code{setupJaspTools()} must be run before using this function
#'
#' @return Invisibly returns a character vector of created/processed test file paths.
#'
#' @examples
#' \dontrun{
#' # Setup jaspTools first
#' library(jaspTools)
#' setupJaspTools()
#'
#' # Create tests from all source folders (library, verified, other)
#' makeTestsFromExamples()
#'
#' # Specify the module directory explicitly
#' makeTestsFromExamples(module.dir = "path/to/your/module")
#'
#' # Only generate tests from the 'library' source folder
#' makeTestsFromExamples(source = "library")
#'
#' # Import JASP files from another directory (copies to jaspfiles/other/)
#' makeTestsFromExamples(path = "path/to/jasp/files", module.dir = "path/to/module")
#'
#' # Overwrite existing test files (skips verified by default)
#' makeTestsFromExamples(overwrite = TRUE)
#'
#' }
#'
#' @export makeTestsFromExamples
makeTestsFromExamples <- function(path, module.dir, source, sanitize = FALSE,
                                  overwrite = FALSE, forceEncode = NULL) {
  .rejectForceEncodeArgument(forceEncode)

  validSources <- c("library", "verified", "other")

  # Determine module directory
  if (missing(module.dir)) {
    module.dir <- getwd()
    cli::cli_inform("Using working directory as module: {.path {module.dir}}")
  }

  if (!dir.exists(module.dir)) {
    cli::cli_abort("Module directory does not exist: {.path {module.dir}}")
  }

  pkgAnalyses <- readModuleAnalysisNames(module.dir)

  # When path is provided, always target "other" and ignore source
  if (!missing(path)) {
    if (!dir.exists(path)) {
      cli::cli_abort("Directory does not exist: {.path {path}}")
    }

    jaspFiles <- list.files(path, pattern = "\\.jasp$", full.names = TRUE)
    if (length(jaspFiles) == 0) {
      cli::cli_abort("No {.file .jasp} files found in directory: {.path {path}}")
    }

    # Ensure destination directory exists
    otherDir <- file.path(module.dir, "tests", "testthat", "jaspfiles", "other")
    if (!dir.exists(otherDir)) {
      dir.create(otherDir, recursive = TRUE)
    }

    cli::cli_h2("Copying JASP files to {.path tests/testthat/jaspfiles/other/} and generating tests")

    result <- .processJaspFiles(jaspFiles,
      module.dir      = module.dir,
      sourceFolder    = "other",
      sanitize        = sanitize,
      overwrite       = overwrite,
      copyToJaspfiles = TRUE,
      pkgAnalyses     = pkgAnalyses
    )

    .printTestGenerationSummary(result$created, result$skipped, result$copied, "other")
    return(invisible(result$created))
  }

  # No path provided: process source folders
  if (missing(source)) {
    if (overwrite) {
      source <- c("library", "other")
    } else {
      source <- validSources
    }
  }

  source <- match.arg(source, validSources, several.ok = TRUE)

  # Warn if overwriting verified tests
  if (overwrite && "verified" %in% source) {
    if (interactive()) {
      answer <- utils::menu(
        choices = c("Yes", "No"),
        title   = "WARNING: You are about to overwrite verified test files. Are you sure?"
      )
      if (answer != 1) {
        cli::cli_alert_danger("Aborted. Remove {.val verified} from {.arg source} or set {.code overwrite = FALSE}.")
        return(invisible(character(0)))
      }
    } else {
      cli::cli_warn("Overwriting verified test files in non-interactive mode.")
    }
  }

  createdFiles <- character(0)
  skippedFiles <- character(0)

  for (src in source) {
    srcDir <- file.path(module.dir, "tests", "testthat", "jaspfiles", src)

    if (!dir.exists(srcDir)) {
      cli::cli_alert_info("Source folder does not exist, skipping: {.path {srcDir}}")
      next
    }

    jaspFiles <- list.files(srcDir, pattern = "\\.jasp$", full.names = TRUE)
    if (length(jaspFiles) == 0) {
      cli::cli_alert_info("No .jasp files found in: {.path {srcDir}}")
      next
    }

    cli::cli_h2("Processing source: {.val {src}} ({length(jaspFiles)} file{?s})")

    result <- .processJaspFiles(jaspFiles,
      module.dir      = module.dir,
      sourceFolder    = src,
      sanitize        = sanitize,
      overwrite       = overwrite,
      copyToJaspfiles = FALSE,
      pkgAnalyses     = pkgAnalyses
    )
    createdFiles <- c(createdFiles, result$created)
    skippedFiles <- c(skippedFiles, result$skipped)
  }

  .printTestGenerationSummary(createdFiles, skippedFiles, character(0), source)
  invisible(createdFiles)
}

readModuleAnalysisNames <- function(module.dir) {
  description <- tryCatch(
    .jaspSyntaxReadModuleDescription(module.dir),
    error = function(e) {
      cli::cli_abort(c(
        "Could not read module description through jaspSyntax.",
        "i" = conditionMessage(e)
      ))
    }
  )

  analyses <- description[["analyses"]]
  if (!is.list(analyses) || length(analyses) == 0L) {
    cli::cli_abort("Module description does not contain any analyses.")
  }

  analysisNames <- vapply(
    analyses,
    function(analysis) {
      name <- analysis[["name"]]
      if (is.null(name) || length(name) == 0L || is.na(name)) {
        return("")
      }
      as.character(name)
    },
    character(1L)
  )
  analysisNames <- analysisNames[nzchar(analysisNames)]

  if (length(analysisNames) == 0L) {
    cli::cli_abort("Module description analyses do not contain analysis names.")
  }

  analysisNames
}

.jaspSyntaxReadModuleDescription <- function(module.dir) {
  if (!exists("readModuleDescription", envir = asNamespace("jaspSyntax"), inherits = FALSE)) {
    cli::cli_abort(
      "Installed jaspSyntax does not provide {.fn readModuleDescription}. Update jaspSyntax before generating tests."
    )
  }

  jaspSyntax::readModuleDescription(module.dir)
}

.printTestGenerationSummary <- function(createdFiles, skippedFiles, copiedFiles, sources) {
  if (length(createdFiles) == 0 && length(skippedFiles) == 0) {
    cli::cli_warn("No test files were created.")
  } else {
    cli::cli_h3("Summary")
    bullets <- character(0)
    if (length(copiedFiles) > 0) {
      paths <- paste0("tests/testthat/jaspfiles/", sources, "/")
      bullets <- c(bullets, stats::setNames(
        paste0("Copied ", length(copiedFiles), " JASP file(s) to {.path ", paths, "}."),
        "v"
      ))
    }
    if (length(createdFiles) > 0) {
      bullets <- c(bullets, stats::setNames(
        paste0("Created ", length(createdFiles), " test file(s) from source{?s}: ",
               "{.val {sources}}."),
        "v"
      ))
    }
    if (length(skippedFiles) > 0) {
      bullets <- c(bullets, stats::setNames(
        paste0("Skipped ", length(skippedFiles), " existing test file(s). Use {.code overwrite = TRUE} to regenerate."),
        "!"
      ))
    }
    cli::cli_bullets(bullets)
  }
}

# Process a vector of JASP files: call makeTestsFromSingleJASPFile on each,
# collect created/skipped/copied paths, and report per-file progress.
# Returns a list with components $created, $skipped, $copied.
.processJaspFiles <- function(jaspFiles, module.dir, sourceFolder, sanitize,
                              overwrite, copyToJaspfiles, pkgAnalyses) {
  createdFiles <- character(0)
  skippedFiles <- character(0)
  copiedFiles  <- character(0)

  for (jaspFile in jaspFiles) {
    cli::cli_inform("Processing: {.file {basename(jaspFile)}}")

    tryCatch(
      {
        result <- makeTestsFromSingleJASPFile(jaspFile,
          module.dir      = module.dir,
          sourceFolder    = sourceFolder,
          sanitize        = sanitize,
          overwrite       = overwrite,
          copyToJaspfiles = copyToJaspfiles,
          pkgAnalyses     = pkgAnalyses
        )
        if (!is.null(result)) {
          if (!is.null(attr(result, "copiedTo"))) {
            copiedFiles <- c(copiedFiles, attr(result, "copiedTo"))
          }
          if (isTRUE(attr(result, "skipped"))) {
            skippedFiles <- c(skippedFiles, result)
            cli::cli_alert_info("Skipped (already exists): {.file {result}}")
          } else {
            createdFiles <- c(createdFiles, result)
            cli::cli_alert_success("Created: {.file {result}}")
          }
        } else {
          cli::cli_alert_warning("No tests created (all analyses were skipped)")
        }
      },
      error = function(e) {
        cli::cli_warn("Failed to process {.file {basename(jaspFile)}}: {e$message}")
      }
    )
  }

  list(created = createdFiles, skipped = skippedFiles, copied = copiedFiles)
}


#' Create a test file from a single JASP file
#'
#' Internal function that processes a single JASP file and generates a test file.
#'
#' @param jaspFile Path to the .jasp file.
#' @param module.dir Path to the module directory.
#' @param sourceFolder String indicating the source folder: \code{"library"},
#'   \code{"verified"}, or \code{"other"}.
#' @param sanitize Whether to sanitize the filename.
#' @param overwrite Whether to overwrite existing test files.
#' @param copyToJaspfiles Whether to copy the JASP file to
#'   \code{tests/testthat/jaspfiles/{sourceFolder}/}.
#' @param pkgAnalyses Optional character vector of allowed analysis names for this module.
#'   If provided, analyses not in this list will be skipped.
#' @param forceEncode Compatibility argument retained for older callers. Supplying
#'   a non-`NULL` value now aborts.
#'
#' @return The path to the created test file (with attr "skipped" if skipped,
#'   and attr "copiedTo" if copied), or NULL if no tests were generated
#'   (e.g., all analyses were skipped or processing failed).
#' @keywords internal
makeTestsFromSingleJASPFile <- function(jaspFile, module.dir, sourceFolder,
                                        sanitize = FALSE, overwrite = FALSE,
                                        copyToJaspfiles = FALSE,
                                        pkgAnalyses = NULL, forceEncode = NULL) {
  .rejectForceEncodeArgument(forceEncode)

  # Extract options from the JASP file through the module currently under test.
  allOptions <- analysisOptionsFromJASPFile(
    jaspFile,
    modulePath = .jaspSyntaxNamedModulePaths(module.dir)
  )

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

  # Copy JASP file to module's jaspfiles/{sourceFolder}/ if requested
  if (copyToJaspfiles) {
    destDir <- file.path(module.dir, "tests", "testthat", "jaspfiles", sourceFolder)
    if (!dir.exists(destDir)) {
      dir.create(destDir, recursive = TRUE)
      cli::cli_alert_info("Created directory: {.path {destDir}}")
    }
    destFile <- file.path(destDir, basename(jaspFile))
    file.copy(jaspFile, destFile, overwrite = TRUE)
    copiedTo <- destFile
    cli::cli_alert_success("Copied to: {.file {destFile}}")
  }

  # Create tests/testthat directory if needed
  testDir <- file.path(module.dir, "tests", "testthat")

  if (!dir.exists(testDir)) {
    dir.create(testDir, recursive = TRUE)
    cli::cli_alert_info("Created directory: {.path {testDir}}")
  }

  # Determine test file path using "test-{sourceFolder}-Name.R" format
  testFileName <- paste0("test-", sourceFolder, "-", sanitizedName, ".R")
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
      cli::cli_warn("Analysis {i} has no name, skipping.")
      next
    }

    if (!is.null(pkgAnalyses) && !analysisName %in% pkgAnalyses) {
      cli::cli_alert_info("Analysis {.val {analysisName}} skipped (not exported from the current module).")
      next
    }

    cli::cli_inform("Running analysis {i}/{length(allOptions)}: {.val {analysisName}}")

    # Run the analysis
    tryCatch(
      {
        set.seed(1)
        results <- runAnalysis(analysisName, dataset, opts, view = FALSE, quiet = TRUE, modulePath = module.dir)

        # Generate test block with expectations from results
        testBlock <- generateExampleTestBlock(
          analysisName = analysisName,
          analysisIndex = i,
          totalAnalyses = length(allOptions),
          jaspFileName = basename(jaspFile),
          sourceFolder = sourceFolder,
          results = results
        )

        testBlocks <- c(testBlocks, list(testBlock))
      },
      error = function(e) {
        cli::cli_warn("Failed to run analysis {.val {analysisName}}: {e$message}")
        # Generate a basic test block that just checks for no error
        testBlock <- generateExampleTestBlockBasic(
          analysisName = analysisName,
          analysisIndex = i,
          totalAnalyses = length(allOptions),
          jaspFileName = basename(jaspFile),
          sourceFolder = sourceFolder
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
  testContent <- generateExampleTestFileContent(baseName, sanitizedName, sourceFolder, testBlocks)

  # Write the test file
  writeLines(testContent, testFilePath)

  # Add copiedTo attribute if applicable
  attr(testFilePath, "copiedTo") <- copiedTo

  return(testFilePath)
}


#' Generate test file content for source-based tests
#'
#' @param baseName Original JASP file name without extension.
#' @param sanitizedName Sanitized name for use in code.
#' @param sourceFolder String indicating the source folder: \code{"library"},
#'   \code{"verified"}, or \code{"other"}.
#' @param testBlocks List of test block strings.
#'
#' @return Character string with complete test file content.
#' @keywords internal
generateExampleTestFileContent <- function(baseName, sanitizedName, sourceFolder, testBlocks) {
  lines <- character(0)

  # Header
  sourceLabelCap <- paste0(toupper(substring(sourceFolder, 1, 1)), substring(sourceFolder, 2))
  lines <- c(lines, paste0('context("', sourceLabelCap, ': ', baseName, '")'))
  lines <- c(lines, "")

  # Helper comment
  lines <- c(lines, "# This test file was auto-generated from a JASP example file.")
  lines <- c(lines, paste0("# The JASP file is stored in tests/testthat/jaspfiles/", sourceFolder, "/."))
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
#' @param sourceFolder String indicating the source folder: \code{"library"},
#'   \code{"verified"}, or \code{"other"}.
#' @param results The analysis results.
#' @return Character string with the test_that block.
#' @keywords internal
generateExampleTestBlock <- function(analysisName, analysisIndex, totalAnalyses, jaspFileName,
                                     sourceFolder, results) {
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

  testDesc <- .generatedExampleTestDescription(
    analysisName,
    analysisIndex,
    totalAnalyses,
    suffix = "results match"
  )

  lines <- c(lines, paste0('test_that("', testDesc, '", {'))
  lines <- c(lines, "")

  lines <- c(lines, .generatedExampleReplayLines(
    analysisName = analysisName,
    analysisIndex = analysisIndex,
    totalAnalyses = totalAnalyses,
    jaspFileName = jaspFileName,
    sourceFolder = sourceFolder
  ))
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
    lines <- .appendGeneratedExampleStatusExpectation(
      lines,
      comment = "  # Basic check - analysis runs without error"
    )
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
#' @param sourceFolder String indicating the source folder: \code{"library"},
#'   \code{"verified"}, or \code{"other"}.
#' @return Character string with the test_that block.
#' @keywords internal
generateExampleTestBlockBasic <- function(analysisName, analysisIndex, totalAnalyses, jaspFileName,
                                          sourceFolder) {
  lines <- character(0)

  testDesc <- .generatedExampleTestDescription(
    analysisName,
    analysisIndex,
    totalAnalyses,
    suffix = "runs without error"
  )

  lines <- c(lines, paste0('test_that("', testDesc, '", {'))
  lines <- c(lines, "")

  lines <- c(lines, .generatedExampleReplayLines(
    analysisName = analysisName,
    analysisIndex = analysisIndex,
    totalAnalyses = totalAnalyses,
    jaspFileName = jaspFileName,
    sourceFolder = sourceFolder
  ))
  lines <- c(lines, "")

  # Basic expectation
  lines <- .appendGeneratedExampleStatusExpectation(
    lines,
    comment = "  # Check analysis runs without error"
  )

  lines <- c(lines, "})")

  return(paste(lines, collapse = "\n"))
}

.rejectForceEncodeArgument <- function(forceEncode) {
  if (is.null(forceEncode))
    return(invisible(FALSE))

  cli::cli_abort(c(
    "{.arg forceEncode} is no longer supported.",
    "i" = "Generated tests now replay saved .jasp options and extracted data through jaspSyntax/jaspBase without a jaspTools-side encoding step."
  ))
}

.generatedExampleTestDescription <- function(analysisName, analysisIndex,
                                             totalAnalyses, suffix) {
  if (totalAnalyses > 1) {
    return(paste0(analysisName, " (analysis ", analysisIndex, ") ", suffix))
  }

  paste0(analysisName, " ", suffix)
}

.generatedExampleReplayLines <- function(analysisName, analysisIndex,
                                         totalAnalyses, jaspFileName,
                                         sourceFolder) {
  optionLine <- if (totalAnalyses == 1) {
    "  opts <- jaspTools::analysisOptions(jaspFile, modulePath = modulePath)"
  } else {
    paste0(
      "  opts <- jaspTools::analysisOptions(jaspFile, modulePath = modulePath)[[",
      analysisIndex,
      "]]"
    )
  }

  c(
    "  # Load from JASP example file",
    paste0('  jaspFile <- testthat::test_path("jaspfiles", "', sourceFolder, '", "', jaspFileName, '")'),
    '  modulePath <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)',
    optionLine,
    "  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)",
    "",
    "  # Run analysis",
    "  set.seed(1)",
    paste0('  results <- jaspTools::runAnalysis("', analysisName, '", dataset, opts, modulePath = modulePath)')
  )
}

.appendGeneratedExampleStatusExpectation <- function(lines, comment) {
  c(
    lines,
    comment,
    "  jaspTools:::.expectNoGeneratedExampleFailureStatus(results)"
  )
}

.expectNoGeneratedExampleFailureStatus <- function(results) {
  testthat::expect_false(
    .generatedExampleHasFailureStatus(results),
    info = .generatedExampleFailureInfo(results)
  )
}

.generatedExampleFailureStatuses <- function() {
  c("error", "validationError", "fatalError")
}

.generatedExampleStatus <- function(results) {
  status <- if (is.list(results)) results[["status"]] else NULL
  if (!is.character(status) || length(status) != 1L || is.na(status))
    return(NA_character_)

  status
}

.generatedExampleHasFailureStatus <- function(results) {
  .generatedExampleStatus(results) %in% .generatedExampleFailureStatuses()
}

.generatedExampleFailureInfo <- function(results) {
  status <- .generatedExampleStatus(results)
  errorMessage <- NULL

  if (is.list(results) && is.list(results[["results"]])) {
    errorMessage <- results[["results"]][["errorMessage"]]
    if (is.null(errorMessage))
      errorMessage <- results[["results"]][["error"]]
  }

  lastError <- tryCatch(getErrorMsgFromLastResults(), error = function(e) NULL)
  if (is.list(lastError) && !is.null(lastError[["type"]]) &&
      .generatedExampleHasFailureStatus(results)) {
    errorMessage <- lastError[["message"]]
  }

  info <- paste0("JASP result status: ", status)
  if (!is.null(errorMessage) && length(errorMessage) > 0L)
    info <- paste(info, paste(as.character(errorMessage), collapse = "\n"), sep = "\n")

  info
}


makeUnitTestsFromResults <- function(results, name, dataset, options) {
  if (!is.list(results) || is.null(names(results)) || .generatedExampleHasFailureStatus(results)) {
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
    cli::cli_warn("{test$type} does not have a title, using a generic one: {.val {test$title}}")
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
