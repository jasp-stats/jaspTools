#' Run an analysis of a JASP file again
#'
#' Runs one analysis of a .jasp file with the options and the data saved in that file, the way JASP
#' runs it after it opened the file.
#'
#' @param jaspFile Path to the .jasp file.
#' @param analysis Which analysis of the file: its position (1 is the first analysis) or its name
#'   (the first analysis with that name).
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the analysis.
#'
#' @details Every step is done by jaspSyntax and jaspBase, as in JASP and in R syntax:
#' - the data is loaded from the file as JASP opens it, with the types and labels of its columns
#'   ([jaspSyntax::loadDataSetFromJaspFile()]);
#' - the options are read from the file as they were saved ([jaspSyntax::readAnalysisOptionsFromJaspFile()]);
#' - the analysis is run by [jaspBase::runWrappedAnalysis()], the function every R wrapper of an
#'   analysis calls: its QML form checks the options and upgrades those of an older version of the
#'   module (the version that saved the file) before the analysis gets them.
#'
#' The module of the analysis must be installed; jaspTools first reinstalls the modules of
#' `module.dirs` that changed. Only the first dataset of the file is loaded.
#'
#' No seed is set before the analysis runs: call `set.seed()` first when its
#' results depend on random numbers.
#'
#' @return The results, as [runAnalysis()] gives them.
#' @seealso [expect_equal_jasp_file()] to compare them with the results saved in the file.
#' @examples
#' \dontrun{
#' set.seed(1)
#' results <- runAnalysisFromJaspFile("tests/testthat/jaspfiles/library/Precognition.jasp", 2)
#' results[["results"]][["estimates"]]
#' }
#' @export
runAnalysisFromJaspFile <- function(jaspFile, analysis = 1L, view = TRUE, quiet = FALSE) {
  jaspFile <- normalizePath(jaspFile, winslash = "/", mustWork = TRUE)
  record   <- selectJaspFileAnalysis(jaspSyntax::readAnalysisOptionsFromJaspFile(jaspFile, includeMeta = FALSE), analysis)

  if (insideTestEnvironment()) {
    view  <- FALSE
    quiet <- TRUE
  }

  oldWd       <- getwd()
  oldLang     <- Sys.getenv("LANG")
  oldLanguage <- Sys.getenv("LANGUAGE")
  on.exit({
    .resetRunTimeInternals()
    setwd(oldWd)
    Sys.setenv(LANG = oldLang)
    Sys.setenv(LANGUAGE = oldLanguage)
  }, add = TRUE)

  initAnalysisRuntime(dataset = NULL, options = record$options, makeTests = FALSE, jaspFile = jaspFile)

  moduleName <- record$moduleName
  if (is.null(moduleName))
    stop("The .jasp file does not say which module analysis `", record$name, "` comes from", call. = FALSE)
  checkJaspBaseRunsModuleAnalysis(moduleName, record$name)

  info    <- jaspSyntax::resolveAnalysisQml(find.package(moduleName), record$name)
  version <- if (is.null(record$moduleVersion)) info$version else record$moduleVersion

  args <- list(
    moduleName   = moduleName,
    analysisName = record$name,
    qmlFileName  = info$qmlFileName,
    options      = record$options,
    version      = version,
    preloadData  = info$preloadData
  )

  returnVal <- runQuietly(jaspBase::runWrappedAnalysis, args, quiet)

  return(invisible(collectRunResults(returnVal, view)))
}

# jaspBase::runWrappedAnalysis runs <analysis>Internal, which only a module with R wrappers has. jaspBase 0.20.6
# runs the analysis itself when there is none.
checkJaspBaseRunsModuleAnalysis <- function(moduleName, analysisName) {
  if (paste0(analysisName, "Internal") %in% getNamespaceExports(moduleName))
    return(invisible(TRUE))

  if (utils::packageVersion("jaspBase") < "0.20.6")
    stop("Module ", moduleName, " has no R wrapper for analysis ", analysisName, ": running it from a .jasp file ",
         "needs jaspBase 0.20.6 or later (installed: ", utils::packageVersion("jaspBase"), ")", call. = FALSE)

  invisible(TRUE)
}

# The record of `analysis` (position or name) in the records jaspSyntax read from a .jasp file.
selectJaspFileAnalysis <- function(records, analysis) {
  if (is.character(analysis) && length(analysis) == 1L) {
    names <- vapply(records, function(record) if (is.null(record$name)) NA_character_ else record$name, character(1L))
    index <- match(analysis, names)
    if (is.na(index))
      stop("The .jasp file has no analysis `", analysis, "`", call. = FALSE)
    return(records[[index]])
  }

  if (!is.numeric(analysis) || length(analysis) != 1L || analysis < 1 || analysis > length(records) || analysis != round(analysis))
    stop("`analysis` must be the name of an analysis of the .jasp file or a position from 1 to ", length(records), call. = FALSE)

  records[[analysis]]
}

#' Compare results with the results saved in a JASP file
#'
#' Checks that the results of an analysis run again (by [runAnalysisFromJaspFile()]) have the
#' tables JASP saved in the .jasp file for that analysis, with the same values.
#'
#' @param results The results of [runAnalysisFromJaspFile()] (or [runAnalysis()]).
#' @param jaspFile Path to the .jasp file.
#' @param analysis Which analysis of the file: its position or its name, as in
#'   [runAnalysisFromJaspFile()].
#'
#' @details Every table JASP saved must be in the results, at the same place, and have the same
#' cells; numbers are compared as in [expect_equal_tables()]. A table of the results that the
#' file does not have is not an error. An analysis JASP ran without error must run without error
#' again. Plots are not compared: the file only has an image of them.
#'
#' The results saved in the file are the reference, so a test passes only as long as the analysis
#' gives what the version of the module that saved the file gave.
#'
#' @examples
#' \dontrun{
#' jaspFile <- "tests/testthat/jaspfiles/library/Precognition.jasp"
#' results  <- runAnalysisFromJaspFile(jaspFile, 2)
#' expect_equal_jasp_file(results, jaspFile, 2)
#' }
#' @export
expect_equal_jasp_file <- function(results, jaspFile, analysis = 1L) {
  stored     <- selectJaspFileAnalysis(jaspSyntax::readAnalysisResultsFromJaspFile(jaspFile), analysis)
  mismatches <- jaspFileMismatches(results, stored)

  expect(length(mismatches) == 0L,
         paste0("The results differ from those saved in ", basename(jaspFile), " for analysis ", stored$name, ":\n",
                paste(mismatches, collapse = "\n")))
}

# What differs between `results` (as runAnalysis gives them) and the record of jaspSyntax::readAnalysisResultsFromJaspFile()
# of the same analysis: one message per table, and one for the status.
jaspFileMismatches <- function(results, stored) {
  mismatches <- character(0)

  if (identical(stored$status, "complete") && !identical(results$status, "complete")) {
    error      <- results$results$errorMessage
    mismatches <- c(mismatches, paste0("The analysis ran with status `", results$status, "` instead of `complete`",
                                       if (!is.null(error)) paste0(": ", error)))
  }

  storedTables <- resultsTables(stored$results)
  newTables    <- resultsTables(results$results)

  for (path in names(storedTables)) {
    storedData <- storedTables[[path]]$data
    label      <- paste0("Table `", path, "`")

    if (!path %in% names(newTables)) {
      mismatches <- c(mismatches, paste0(label, " is missing"))
      next
    }

    newData <- newTables[[path]]$data
    if (length(storedData) == 0L || length(newData) == 0L) {
      if (length(storedData) != length(newData))
        mismatches <- c(mismatches, paste0(label, " has ", length(newData), " rows instead of ", length(storedData)))
      next
    }

    mismatch <- tableMismatch(newData, collapseTestTable(storedData), label)
    if (mismatch != "")
      mismatches <- c(mismatches, mismatch)
  }

  mismatches
}

# The tables of a results tree, named by their place in it ("estimates/collection/estimates_mean").
resultsTables <- function(results, path = character(0)) {
  if (!is.list(results))
    return(list())

  if (all(c("data", "schema") %in% names(results)))
    return(stats::setNames(list(results), paste(path, collapse = "/")))

  tables <- list()
  for (name in names(results))
    tables <- c(tables, resultsTables(results[[name]], c(path, name)))

  tables
}
