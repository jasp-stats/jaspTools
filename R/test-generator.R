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
#' Each test runs the R syntax of its analysis: the call of the R wrapper of the analysis, as JASP
#' shows it, with the options saved in the file upgraded to the version of the module. It runs
#' with \code{\link{runAnalysisSyntax}} on the data of the file. Its tables and HTML output
#' are recorded in the test file's \code{_snaps} folder. Generating the tests again stages
#' changes to existing references for review. If a generated test file has custom table
#' tolerances or refresh settings, rerun that test file to stage changes under its
#' settings. Regeneration stops before overwriting these customizations.
#' Plots use \code{\link{expect_equal_plots}}.
#' When the tables differ from the results JASP saved in the file
#' (see \code{\link{expect_equal_jasp_file}}), a warning says what differs. An analysis without
#' R wrapper is run with \code{\link{runAnalysisFromJaspFile}} instead.
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

#' }
#'
#' @export makeTestsFromExamples
makeTestsFromExamples <- function(path, module.dir, source, sanitize = FALSE,
                                  overwrite = FALSE) {
  validSources <- c("library", "verified", "other")

  # Determine module directory
  if (missing(module.dir)) {
    module.dir <- getwd()
    cli::cli_inform("Using working directory as module: {.path {module.dir}}")
  }

  if (!dir.exists(module.dir)) {
    cli::cli_abort("Module directory does not exist: {.path {module.dir}}")
  }

  pkgAnalyses <- NULL
  if (isBinaryPackage(module.dir)) {
    qmlPath <- file.path(module.dir, "Description.qml")
  } else {
    qmlPath <- file.path(module.dir, "inst", "Description.qml")
  }

  if (file.exists(qmlPath)) {
    pkgAnalyses <- names(jaspSyntax::parseModuleDescription(module.dir)$analyses)
  } else {
    cli::cli_abort(c(
      "{.file Description.qml} not found at path: {.path {qmlPath}}.",
      "i" = "Make sure the module contains {.file inst/Description.qml} (source) or {.file Description.qml} (installed)."
    ))
  }

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
#'
#' @return The path to the created test file (with attr "skipped" if skipped,
#'   and attr "copiedTo" if copied), or NULL if no tests were generated
#'   (e.g., all analyses were skipped or processing failed).
#' @keywords internal
makeTestsFromSingleJASPFile <- function(jaspFile, module.dir, sourceFolder,
                                        sanitize = FALSE, overwrite = FALSE,
                                        copyToJaspfiles = FALSE,
                                        pkgAnalyses = NULL) {
  # The analyses of the file and the results JASP saved for them
  analyses <- jaspSyntax::readAnalysisResultsFromJaspFile(jaspFile)

  # Get the base name for the test file
  baseName <- tools::file_path_sans_ext(basename(jaspFile))
  if (sanitize) {
    sanitizedName <- gsub("\\W+", "-", baseName)
    sanitizedName <- gsub("^-+|-+$", "", sanitizedName) # trim leading/trailing hyphens
  } else {
    sanitizedName <- baseName
  }

  copyExample <- function() {
    if (!copyToJaspfiles)
      return(NULL)
    destDir <- file.path(module.dir, "tests", "testthat", "jaspfiles", sourceFolder)
    if (!dir.exists(destDir)) {
      dir.create(destDir, recursive = TRUE)
      cli::cli_alert_info("Created directory: {.path {destDir}}")
    }
    destFile <- file.path(destDir, basename(jaspFile))
    if (normalizePath(jaspFile, mustWork = FALSE) != normalizePath(destFile, mustWork = FALSE) &&
        !file.copy(jaspFile, destFile, overwrite = TRUE))
      stop("Could not copy the JASP example to ", destFile, call. = FALSE)
    cli::cli_alert_success("Copied to: {.file {destFile}}")
    destFile
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
    attr(result, "copiedTo") <- copyExample()
    return(result)
  }
  previous <- if (file.exists(testFilePath))
    generatedSnapshotIDs(parse(testFilePath, keep.source = FALSE), regenerating = testFileName)

  # Run each analysis and generate test expectations
  testBlocks <- list()
  snapshots <- list()

  # The R syntax of an analysis is made with the QML form of the installed module
  if (hasModuleDirs())
    reinstallChangedModules()

  for (i in seq_along(analyses)) {
    analysisName <- analyses[[i]]$name

    if (is.null(analysisName)) {
      cli::cli_warn("Analysis {i} has no name, skipping.")
      next
    }

    if (!is.null(pkgAnalyses) && !analysisName %in% pkgAnalyses) {
      cli::cli_alert_info("Analysis {.val {analysisName}} skipped (not exported from the current module).")
      next
    }

    cli::cli_inform("Running analysis {i}/{length(analyses)}: {.val {analysisName}}")

    # The R syntax of the analysis, with the options of the file: the test runs it
    syntax <- tryCatch(
      jaspFileAnalysisSyntax(jaspFile, i),
      error = function(e) {
        cli::cli_warn("No R syntax for analysis {.val {analysisName}}, the test runs it with runAnalysisFromJaspFile(): {e$message}")
        NULL
      }
    )

    # Run the analysis again with the options and data of the file, as the test does
    results <- tryCatch({
      set.seed(1)
      result <- if (is.null(syntax)) runAnalysisFromJaspFile(jaspFile, i, view = FALSE, quiet = TRUE)
                else                 runJaspFileAnalysisSyntax(syntax, jaspFile)
      if (isTRUE(result$status %in% c("error", "fatalError", "validationError")))
        stop(paste(c(result$status, result$results$errorMessage), collapse = ": "), call. = FALSE)
      result
    }, error = function(e) {
      if (file.exists(testFilePath))
        stop("Could not regenerate ", testFileName, " because analysis ", i, " (", analysisName,
             ") failed: ", conditionMessage(e), call. = FALSE)
      cli::cli_warn("Failed to run analysis {.val {analysisName}}: {e$message}")
      NULL
    })

    if (is.null(results)) {
      testBlock <- generateExampleTestBlockBasic(analysisName, i, length(analyses),
                                                 basename(jaspFile), sourceFolder, syntax)
    } else {
      mismatches <- jaspFileMismatches(results, analyses[[i]])
      if (length(mismatches) > 0)
        warnJaspFileMismatches(i, analysisName, jaspFile, mismatches)

      tests <- getTests(results$results, analysisIndex = i)
      testBlock <- generateExampleTestBlock(analysisName, i, length(analyses),
                                            basename(jaspFile), sourceFolder, results, syntax, tests)
      snapshots <- c(snapshots, Filter(function(test) test$type != "plot", tests))
    }
    testBlocks[[length(testBlocks) + 1L]] <- testBlock
  }

  # Check if any tests were generated (all analyses might have been skipped)
  if (length(testBlocks) == 0) {
    return(NULL)
  }

  # Generate the test file content
  testContent <- generateExampleTestFileContent(baseName, sanitizedName, sourceFolder, testBlocks)

  if (length(previous)) {
    current <- generatedSnapshotIDs(parse(text = testContent, keep.source = FALSE))
    removed <- setdiff(previous, current)
    if (length(removed))
      stop("Regenerating ", testFileName, " would remove snapshot expectations: ", paste(removed, collapse = ", "),
           ". Review these references and remove their expectations from the existing test file ",
           "if you intend to retire them, then run generation again.", call. = FALSE)
  }

  snapshotDir <- file.path(testDir, "_snaps", paste0(sourceFolder, "-", sanitizedName))
  for (test in snapshots)
    stageGeneratedSnapshot(test$node, test$name, test$type, snapshotDir)

  copiedTo <- copyExample()
  # Write the test file
  writeLines(testContent, testFilePath)

  # Add copiedTo attribute if applicable
  attr(testFilePath, "copiedTo") <- copiedTo

  return(testFilePath)
}

# Inspect generated R code without evaluating a module's test file or its settings.
generatedSnapshotIDs <- function(code, regenerating = NULL) {
  ids <- character()
  types <- c(expect_equal_plots = "plot", expect_snapshot_table = "table", expect_snapshot_html = "html")
  customSettings <- function() {
    stop("Regenerating ", regenerating, " would overwrite custom table snapshot settings. ",
         "Run the existing test file to stage changes with its settings.", call. = FALSE)
  }
  visit <- function(expr) {
    if (is.call(expr)) {
      fun <- expr[[1L]]
      if (is.call(fun) && identical(fun[[1L]], quote(`::`)) && identical(fun[[2L]], quote(jaspTools)))
        fun <- fun[[3L]]
      if (is.symbol(fun)) {
        name <- as.character(fun)
        if (!is.null(regenerating) && name == "local_table_snapshot_tolerance")
          customSettings()
        if (name %in% names(types)) {
          signature <- switch(name, expect_equal_plots = expect_equal_plots,
                              expect_snapshot_html = expect_snapshot_html, expect_snapshot_table)
          args <- match.call(signature, expr)
          if (!is.null(regenerating) && name == "expect_snapshot_table" &&
              any(c("rtol", "atol", "columns", "refresh") %in% names(args)))
            customSettings()
          id <- args[["name"]]
          if (is.character(id) && length(id) == 1L)
            ids <<- c(ids, paste(types[[name]], id))
        }
      }
    }
    if (is.call(expr) || is.expression(expr))
      for (i in seq_along(expr))
        visit(expr[[i]])
  }
  visit(code)
  unique(ids)
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
  lines <- c(lines, paste0("context(", encodeString(paste0(sourceLabelCap, ": ", baseName), quote = '"'), ")"))
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


# The messages are not cli format strings: their braces (R code in an error, a table cell) are doubled so cli shows them.
warnJaspFileMismatches <- function(index, analysisName, jaspFile, mismatches) {
  cli::cli_warn(c(
    "Analysis {index} ({.val {analysisName}}) does not give the results saved in {.file {basename(jaspFile)}}; the test expects the new results:",
    stats::setNames(gsub("([{}])", "\\1\\1", mismatches), rep("*", length(mismatches)))
  ))
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
#' @param syntax The R syntax the test runs, or NULL to run the analysis with
#'   \code{runAnalysisFromJaspFile()}.
#' @param tests Output nodes collected from the analysis results.
#'
#' @return Character string with the test_that block.
#' @keywords internal
generateExampleTestBlock <- function(analysisName, analysisIndex, totalAnalyses, jaspFileName,
                                     sourceFolder, results, syntax = NULL,
                                     tests = getTests(results$results, analysisIndex)) {
  # Build the test block
  lines <- character(0)

  # Test description
  if (totalAnalyses > 1) {
    testDesc <- paste0(analysisName, " (analysis ", analysisIndex, ") results match")
  } else {
    testDesc <- paste0(analysisName, " results match")
  }

  lines <- c(lines, paste0('test_that("', testDesc, '", {'))
  lines <- c(lines, "  jaspTools::preserveSnapshotReferences()", "")

  lines <- c(lines, runFromJaspFileLines(sourceFolder, jaspFileName, analysisIndex, syntax))
  lines <- c(lines, resultStatusExpectation(), "", outputExpectationLines(tests), "})")

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
#'
#' @param syntax The R syntax the test runs, or NULL to run the analysis with
#'   \code{runAnalysisFromJaspFile()}.
#'
#' @return Character string with the test_that block.
#' @keywords internal
generateExampleTestBlockBasic <- function(analysisName, analysisIndex, totalAnalyses, jaspFileName,
                                          sourceFolder, syntax = NULL) {
  lines <- character(0)

  # Test description
  if (totalAnalyses > 1) {
    testDesc <- paste0(analysisName, " (analysis ", analysisIndex, ") runs without error")
  } else {
    testDesc <- paste0(analysisName, " runs without error")
  }

  lines <- c(lines, paste0('test_that("', testDesc, '", {'))
  lines <- c(lines, "  jaspTools::preserveSnapshotReferences()", "")

  lines <- c(lines, runFromJaspFileLines(sourceFolder, jaspFileName, analysisIndex, syntax))

  lines <- c(lines, resultStatusExpectation(), "})")

  return(paste(lines, collapse = "\n"))
}


# The lines of a generated test that run analysis `analysisIndex` of a JASP file of the module again: its R syntax
# (the call of its R wrapper) on the data of the file, or without `syntax` the analysis as the file saved it.
runFromJaspFileLines <- function(sourceFolder, jaspFileName, analysisIndex, syntax = NULL) {
  jaspFileLine <- sprintf('  jaspFile <- testthat::test_path("jaspfiles", %s, %s)',
                          encodeString(sourceFolder, quote = '"'), encodeString(jaspFileName, quote = '"'))

  if (is.null(syntax))
    return(c(
      "  # Run the analysis again with the options and data of the JASP example file",
      jaspFileLine,
      "  set.seed(1)",
      paste0("  results <- jaspTools::runAnalysisFromJaspFile(jaspFile, ", analysisIndex, ")"),
      ""
    ))

  c(
    paste0("  # Run the R syntax of analysis ", analysisIndex, " of the JASP example file on the data of the file"),
    jaspFileLine,
    "  set.seed(1)",
    paste0("  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, ", syntax, ")"),
    ""
  )
}


makeUnitTestsFromResults <- function(results, name, dataset, options) {
  if (!is.list(results) || is.null(names(results)) || isTRUE(results$status %in% c("error", "fatalError", "validationError"))) {
    stop("Can't make unit test from results: not a results list")
  }

  tests <- getTests(results$results)
  if (length(tests) == 0) {
    stop("Could not identify any tables, HTML output, or plots to test")
  }

  output <- makeExpectations(tests, name, options, dataset)
  cat(output)
}

getTests <- function(results, analysisIndex = NULL) {
  tests <- list()
  figureNumber <- 0L

  visit <- function(node, path, meta = NULL) {
    declared <- meta[["type"]]
    type <- switch(if (is.null(declared)) "" else declared,
                   table = "table", image = "plot", htmlNode = "html", reportNode = "html", NULL)

    # Results from older analyses can omit metadata. Recognise the node itself in that case.
    if (is.null(declared) && is.list(node)) {
      fields <- names(node)
      if (isResultsTable(node))
        type <- "table"
      else if (all(c("data", "width", "height") %in% fields))
        type <- "plot"
      else if (all(c("rawtext", "text") %in% fields) || all(c("html", "topHtml") %in% fields))
        type <- "html"
    }

    if (!is.null(type)) {
      if (is.null(node) || !is.list(node))
        stop("Missing ", type, " result at ", paste(path, collapse = "/"), call. = FALSE)
      if (type == "plot") {
        figureNumber <<- figureNumber + 1L
        # Keep the names of the SVG and structural references used by existing plot tests.
        title <- node[["title"]]
        if (!is.null(analysisIndex)) {
          name <- gsub("-+", "-", gsub("\\W", "-", tolower(title)))
          name <- paste0("analysis-", analysisIndex, "_figure-", figureNumber, "_", name)
        } else {
          if (!is.character(title) || length(title) != 1L || is.na(title) || !nzchar(title))
            title <- if (length(path)) as.character(path[[length(path)]]) else type
          name <- gsub("-+", "-", gsub("\\W", "-", tolower(title)))
          used <- vapply(tests, function(test) str_standardise_snapshot_name(test$name), character(1L))
          if (!nzchar(str_standardise_snapshot_name(name)) || str_standardise_snapshot_name(name) %in% used)
            name <- resultSnapshotName("plot", path)
        }
      } else {
        name <- resultSnapshotName(type, path, analysisIndex)
      }
      tests[[length(tests) + 1L]] <<- list(type = type, path = path, node = node, name = name)
      return()
    }

    if (!is.list(node) || (!is.null(declared) && !declared %in% c("collection", "object")))
      return()

    children <- node
    childrenMeta <- if (is.null(declared)) node[[".meta"]] else meta[["meta"]]
    if (identical(declared, "collection")) {
      children <- node[["collection"]]
      path <- c(path, list("collection"))
    }
    if (length(childrenMeta)) {
      names(childrenMeta) <- vapply(childrenMeta, "[[", character(1L), "name")
    }
    # Retain the result-map order used to number existing plots. Older unnamed
    # collections can be indexed by position; a literal key such as "1" stays a string.
    keys <- names(children)
    for (i in seq_along(children)) {
      key <- if (!is.null(keys) && nzchar(keys[i])) keys[i] else i
      if (!identical(key, ".meta"))
        visit(children[[i]], c(path, list(key)), if (is.character(key)) childrenMeta[[key]])
    }
    for (key in setdiff(names(childrenMeta), keys))
      visit(NULL, c(path, list(key)), childrenMeta[[key]])
  }

  visit(results, list())
  tests
}

# Include the whole path in the hash so punctuation, non-ASCII keys and identically named
# leaves in different collections remain distinct. Keep a short, readable leaf name in the file.
resultSnapshotName <- function(type, path, analysisIndex = NULL) {
  parts <- vapply(path, function(key) {
    if (is.numeric(key)) return(paste0("#", key))
    key <- enc2utf8(key)
    paste0(nchar(key, type = "bytes"), ":", key)
  }, character(1L))
  key <- paste(parts, collapse = "/")
  hash <- digest::digest(enc2utf8(key), algo = "xxhash64", serialize = FALSE)
  leaf <- if (length(path)) as.character(path[[length(path)]]) else ""
  leaf <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", leaf)
  leaf <- gsub("[^a-z0-9]+", "-", leaf, perl = TRUE)
  leaf <- gsub("^-|-$", "", substr(leaf, 1L, 40L))
  paste(c("analysis", analysisIndex, type, leaf[nzchar(leaf)], hash), collapse = "-")
}

snapshotExpectationLines <- function(test) {
  index <- paste(vapply(test$path, function(key) {
    paste0("[[", if (is.numeric(key)) key else encodeString(key, quote = '"'), "]]")
  }, character(1L)), collapse = "")
  node <- paste0('results[["results"]]', index)
  name <- encodeString(test$name, quote = '"')
  if (test$type == "plot")
    return(c(paste0('  plotName <- ', node, '[["data"]]'),
             '  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]',
             paste0("  jaspTools::expect_equal_plots(testPlot, ", name, ")")))

  paste0("  jaspTools::expect_snapshot_", test$type, "(", node, ", ", name, ")")
}

outputExpectationLines <- function(tests) {
  # A plot backend can skip the remainder of this test block.
  plots <- vapply(tests, function(test) test$type == "plot", logical(1))
  ordered <- c(tests[!plots], tests[plots])
  unlist(lapply(ordered, function(test) c(snapshotExpectationLines(test), "")), use.names = FALSE)
}

resultStatusExpectation <- function() {
  c('  expect_false(results[["status"]] %in% c("error", "fatalError", "validationError"),',
    '               info = results[["results"]][["errorMessage"]])')
}

makeExpectations <- function(tests, name, options, dataset) {
  desc <- encodeString(paste(name, "results match"), quote = '"')
  preamble <- addPreambleLines(name, options, dataset)
  lines <- c(paste0("test_that(", desc, ", {"), "  jaspTools::preserveSnapshotReferences()",
             paste0("  ", gsub("\n", "\n  ", preamble)), "", resultStatusExpectation(),
             "", outputExpectationLines(tests), "})")
  paste(lines, collapse = "\n")
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
