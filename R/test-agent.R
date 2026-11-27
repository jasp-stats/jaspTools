#' Run all tests with agent-friendly output.
#'
#' Runs all tests in the currently monitored modules but returns a compact summary
#' suitable for LLM/MCP consumption. Detailed results (failures, warnings, backtraces)
#' are written to a JSON log file that can be queried on demand.
#'
#' Uses testthat's agent reporter when available. Earlier testthat versions
#' collect the same test results and snapshots with the silent reporter.
#'
#' @param logFile Path for the detailed JSON log. Defaults to a temp file.
#'   The path is printed in the summary so it can be read later.
#'
#' @return Invisibly returns a \code{jaspAgentTestResults} object (S3 class) with:
#'   \describe{
#'     \item{status}{0 = all passed, 1 = failures, errors, or module-level errors.}
#'     \item{summary}{Named list of counts: \code{fail}, \code{error}, \code{warn}, \code{skip}, \code{pass}, \code{time}.}
#'     \item{logFile}{Path to the detailed JSON log file.}
#'     \item{snapshotReviews}{Named list of standalone HTML reports for modules
#'       with pending snapshots in the tested files.}
#'     \item{failures}{Data frame with columns \code{module}, \code{file}, \code{test}, \code{message}
#'       — one row per failed expectation or test error.}
#'     \item{warnings}{Data frame with columns \code{module}, \code{file}, \code{test}, \code{message}.}
#'     \item{skips}{Data frame with columns \code{module}, \code{file}, \code{test}, \code{reason}.}
#'     \item{tests}{Data frame of all individual tests with columns \code{module}, \code{file},
#'       \code{context}, \code{test}, \code{nb}, \code{failed}, \code{skipped}, \code{error},
#'       \code{warning}, \code{passed}, \code{time}.}
#'     \item{errorModules}{Named character vector of module-level error messages (empty if none).}
#'   }
#'   Printing the object shows a compact one-line summary plus failure details.
#'
#' @examples
#' \dontrun{
#' results <- agentTestAll()
#' results$status
#' results$summary
#' results$failures
#' results$tests
#' }
#'
#' @export agentTestAll
agentTestAll <- function(logFile = NULL) {
  if (is.null(logFile))
    logFile <- tempfile(pattern = "jasp-test-results-", tmpdir = dirname(tempdir()), fileext = ".json")

  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true")

  optsValue <- getOption("testthat.progress.max_fails")
  options("testthat.progress.max_fails" = 1000)

  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
    options("testthat.progress.max_fails" = optsValue)
  })

  modulePaths <- getModulePathsForTesting()

  allResults <- list()
  for (modulePath in modulePaths) {
    testDir <- file.path(modulePath, "tests", "testthat")

    allResults[[basename(modulePath)]] <- runTestsQuietly(function() {
      testthat::test_dir(
        testDir,
        reporter        = agentTestReporter(),
        stop_on_failure = FALSE,
        stop_on_warning = FALSE
      )
    })
  }

  finishAgentTestRun(allResults, modulePaths, logFile)
}


#' Run a specific analysis test with agent-friendly output.
#'
#' Tests a specific R analysis but returns compact output suitable for LLM/MCP
#' consumption. Detailed results are written to a JSON log file.
#'
#' @param name String name of the analysis to test (case sensitive).
#' @param logFile Path for the detailed JSON log. Defaults to a temp file.
#' @param includeAll Logical (default \code{TRUE}). When \code{TRUE}, also
#'   includes auto-generated example test files that reference this analysis.
#'   Set \code{includeAll = FALSE} to restrict to the standard test file only.
#'
#' @return Invisibly returns a \code{jaspAgentTestResults} object. See \code{\link{agentTestAll}}
#'   for the full description of fields.
#'
#' @examples
#' \dontrun{
#' results <- agentTestAnalysis("Correlation")
#' results$status
#' results$failures
#' results$warnings
#' }
#'
#' @export agentTestAnalysis
agentTestAnalysis <- function(name, logFile = NULL, includeAll = TRUE) {
  if (is.null(logFile))
    logFile <- tempfile(pattern = paste0("jasp-test-", name, "-"), tmpdir = dirname(tempdir()), fileext = ".json")

  modulePath <- getModulePathFromRFunction(name)
  filesToTest <- getTestFilesMatchingName(name, modulePath, includeAll = includeAll)

  envirValue <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true")
  on.exit({
    Sys.setenv("NOT_CRAN" = envirValue)
  })

  testDir <- file.path(modulePath, "tests", "testthat")

  allResults <- list()
  allResults[[basename(modulePath)]] <- runTestsQuietly(function() {
    testthat:::test_files(
      test_paths   = filesToTest,
      test_dir     = testDir,
      reporter     = agentTestReporter(),
      test_package = NULL
    )
  })

  finishAgentTestRun(allResults, modulePath, logFile)
}


# ---- Internal helpers --------------------------------------------------------

finishAgentTestRun <- function(allResults, modulePaths, logFile) {
  result <- buildAgentTestResult(allResults, logFile)
  result$snapshotReviews <- agentSnapshotReviews(modulePaths, result$tests)
  writeAgentTestLog(allResults, logFile, result$snapshotReviews)
  print(result)
  invisible(result)
}


agentTestReporter <- function() {
  if ("LlmReporter" %in% getNamespaceExports("testthat"))
    testthat::LlmReporter$new()
  else
    testthat::SilentReporter$new()
}

agentSnapshotReviews <- function(modulePaths, tests) {
  reports <- list()
  for (module in modulePaths) {
    files <- unique(tests$file[tests$module == basename(module)])
    if (!length(files)) next
    report <- tryCatch({
      ids <- snapshotReviewIds(module, files = files)
      if (length(ids)) writeSnapshotReview(module, ids, output = NULL, open = FALSE)
    }, error = function(e) {
      warning("Could not build the snapshot review for ", basename(module), ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (!is.null(report)) reports[[basename(module)]] <- report
  }
  reports
}

#' Run a test function quietly, capturing stdout and muffling sink warnings.
#'
#' Captures standard output while preserving conditions recorded by testthat.
#'
#' @param testFun A zero-argument function that runs tests (e.g.,
#'   \code{function() testthat::test_dir(...)}).
#' @return The test results, or a module-level error with its message.
#' @keywords internal
runTestsQuietly <- function(testFun) {
  # Agent tools may set cli.spinner = FALSE, which cli::get_spinner() rejects.
  oldSpinner <- getOption("cli.spinner")
  if (!is.null(oldSpinner) && !is.character(oldSpinner) && !is.list(oldSpinner))
    options(cli.spinner = "line")
  on.exit(options(cli.spinner = oldSpinner), add = TRUE)

  # We CANNOT muffle all warnings because testthat's expectation_warning
  # inherits from warning — muffling them breaks the test framework's
  # condition recording and causes hangs. Only muffle the specific
  # "no sink to remove" warning that leaks in MCP/btw contexts.
  env <- environment()
  env$res <- NULL
  errMsg <- NULL
  tryCatch(
    withCallingHandlers({
      invisible(utils::capture.output(
        env$res <- testFun(),
        type = "output"
      ))
    }, warning = function(w) {
      if (grepl("no sink to remove", conditionMessage(w), fixed = TRUE))
        invokeRestart("muffleWarning")
    }),
    error = function(e) {
      errMsg <<- conditionMessage(e)
    }
  )
  if (!is.null(errMsg) || is.null(env$res))
    return(list(error = TRUE, errorMessage = if (is.null(errMsg)) "Unknown error" else errMsg))
  env$res
}


#' Build a rich S3 result object from raw test results.
#' @keywords internal
buildAgentTestResult <- function(allResults, logFile) {
  totalFail  <- 0L
  totalWarn  <- 0L
  totalSkip  <- 0L
  totalPass  <- 0L
  totalError <- 0L
  totalTime  <- 0
  errorModules <- character()

  # Collectors for failures/warnings/skips data frames
  fail_mod <- character(); fail_file <- character()
  fail_test <- character(); fail_msg <- character()

  warn_mod <- character(); warn_file <- character()
  warn_test <- character(); warn_msg <- character()

  skip_mod <- character(); skip_file <- character()
  skip_test <- character(); skip_reason <- character()

  testDfList <- list()

  for (moduleName in names(allResults)) {
    res <- allResults[[moduleName]]

    if (isTRUE(res$error)) {
      errorModules[moduleName] <- trimws(res$errorMessage)
      totalError <- totalError + 1L
      next
    }

    df <- as.data.frame(res)
    totalFail  <- totalFail  + sum(df$failed)
    totalWarn  <- totalWarn  + sum(df$warning)
    totalSkip  <- totalSkip  + sum(df$skipped)
    totalPass  <- totalPass  + sum(df$passed)
    totalError <- totalError + sum(as.logical(df$error))
    totalTime  <- totalTime  + sum(df$real)

    # Clean test df: drop list-column and timing details, add module, rename
    dfClean <- df[, setdiff(names(df), c("result", "user", "system")), drop = FALSE]
    names(dfClean)[names(dfClean) == "real"] <- "time"
    dfClean <- cbind(module = moduleName, dfClean, stringsAsFactors = FALSE)
    testDfList[[moduleName]] <- dfClean

    # Extract failures and errors (one row per failed/errored expectation)
    for (i in which(df$failed > 0 | as.logical(df$error))) {
      row <- df[i, ]
      details <- extractDetailedFailures(row$result[[1]])
      for (d in details) {
        fail_mod  <- c(fail_mod, moduleName)
        fail_file <- c(fail_file, row$file)
        fail_test <- c(fail_test, row$test)
        fail_msg  <- c(fail_msg, d$message)
      }
    }

    # Extract warnings (one row per warning)
    for (i in which(df$warning > 0)) {
      row <- df[i, ]
      msgs <- extractDetailedWarnings(row$result[[1]])
      for (m in msgs) {
        warn_mod  <- c(warn_mod, moduleName)
        warn_file <- c(warn_file, row$file)
        warn_test <- c(warn_test, row$test)
        warn_msg  <- c(warn_msg, m)
      }
    }

    # Extract skips (one row per skipped test)
    for (i in which(as.logical(df$skipped))) {
      row <- df[i, ]
      reason <- extractSkipReason(row$result[[1]])
      skip_mod    <- c(skip_mod, moduleName)
      skip_file   <- c(skip_file, row$file)
      skip_test   <- c(skip_test, row$test)
      skip_reason <- c(skip_reason,
                       if (is.null(reason)) "(no reason)" else reason)
    }
  }

  # Build data frames (typed character columns even when empty)
  failures <- data.frame(module = fail_mod, file = fail_file,
                         test = fail_test, message = fail_msg,
                         stringsAsFactors = FALSE)

  warnings <- data.frame(module = warn_mod, file = warn_file,
                         test = warn_test, message = warn_msg,
                         stringsAsFactors = FALSE)

  skips <- data.frame(module = skip_mod, file = skip_file,
                      test = skip_test, reason = skip_reason,
                      stringsAsFactors = FALSE)

  # Combined tests df (empty df with correct columns if no results)
  tests <- if (length(testDfList) > 0) {
    combined <- do.call(rbind, testDfList)
    rownames(combined) <- NULL
    combined
  } else {
    data.frame(module = character(0), file = character(0),
               context = character(0), test = character(0),
               nb = integer(0), failed = integer(0),
               skipped = logical(0), error = logical(0),
               warning = integer(0), passed = integer(0),
               time = numeric(0), stringsAsFactors = FALSE)
  }

  status <- if (totalFail > 0 || totalError > 0 || length(errorModules) > 0) 1L else 0L

  structure(
    list(
      status       = status,
      summary      = list(fail = totalFail, error = totalError, warn = totalWarn,
                          skip = totalSkip, pass = totalPass, time = totalTime),
      logFile      = logFile,
      failures     = failures,
      warnings     = warnings,
      skips        = skips,
      tests        = tests,
      errorModules = errorModules
    ),
    class = "jaspAgentTestResults"
  )
}


#' Print method for jaspAgentTestResults.
#'
#' Prints a compact one-line summary of test counts plus failure details.
#'
#' @param x A \code{jaspAgentTestResults} object.
#' @param ... Ignored.
#'
#' @export
print.jaspAgentTestResults <- function(x, ...) {
  s <- x$summary
  cat(sprintf("== Test Results == FAIL: %d | ERROR: %d | WARN: %d | SKIP: %d | PASS: %d | Time: %.1fs\n",
              s$fail, s$error, s$warn, s$skip, s$pass, s$time))

  if (length(x$errorModules) > 0) {
    cat("\nMODULE ERRORS:\n")
    for (nm in names(x$errorModules)) {
      cat(sprintf("  %s: %s\n", nm, x$errorModules[[nm]]))
    }
  }

  if (nrow(x$failures) > 0) {
    cat("\nFAILURES:\n")
    # Show one line per failed test (not per expectation) using compact message
    seen <- character()
    for (i in seq_len(nrow(x$failures))) {
      row <- x$failures[i, ]
      key <- paste(row$module, row$file, row$test, sep = "::")
      if (key %in% seen) next
      seen <- c(seen, key)
      msg1 <- strsplit(row$message, "\n", fixed = TRUE)[[1]][1]
      cat(sprintf("  [%s] %s :: %s\n    %s\n", row$module, row$file, row$test, msg1))
    }
  }

  for (module in names(x$snapshotReviews))
    cat(sprintf("\nSnapshot review (%s): %s\n", module, x$snapshotReviews[[module]]))
  cat(sprintf("\nDetails: %s\n", x$logFile))
  invisible(x)
}


#' Write detailed test results to a JSON log file.
#' @param allResults Per-module testthat results.
#' @param logFile Output JSON path.
#' @param snapshotReviews Named list of module report paths.
#' @keywords internal
writeAgentTestLog <- function(allResults, logFile, snapshotReviews = list()) {
  logData <- list()

  for (moduleName in names(allResults)) {
    res <- allResults[[moduleName]]

    if (isTRUE(res$error)) {
      logData[[moduleName]] <- list(
        error = TRUE,
        errorMessage = res$errorMessage,
        tests = list()
      )
      next
    }

    df <- as.data.frame(res)
    tests <- list()

    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      testEntry <- list(
        file    = row$file,
        context = row$context,
        test    = row$test,
        passed  = row$passed,
        failed  = row$failed,
        skipped = row$skipped,
        warning = row$warning,
        error   = row$error,
        time    = row$real
      )

      # Add failure/error details if any
      if (row$failed > 0 || isTRUE(as.logical(row$error))) {
        testEntry$failureDetails <- extractDetailedFailures(row$result[[1]])
      }

      # Add warning details if any
      if (row$warning > 0) {
        testEntry$warningDetails <- extractDetailedWarnings(row$result[[1]])
      }

      # Add skip reason if skipped
      if (isTRUE(as.logical(row$skipped))) {
        testEntry$skipReason <- extractSkipReason(row$result[[1]])
      }

      tests[[length(tests) + 1]] <- testEntry
    }

    # Compute module summary
    logData[[moduleName]] <- list(
      summary = list(
        fail  = sum(df$failed),
        error = sum(as.logical(df$error)),
        warn  = sum(df$warning),
        skip  = sum(df$skipped),
        pass  = sum(df$passed),
        time  = sum(df$real)
      ),
      tests = tests
    )
  }

  for (module in names(snapshotReviews))
    logData[[module]]$snapshotReview <- snapshotReviews[[module]]
  jsonlite::write_json(logData, logFile, pretty = TRUE, auto_unbox = TRUE)
}


#' Extract detailed failure messages with backtraces from result list.
#' @keywords internal
extractDetailedFailures <- function(resultList) {
  if (!is.list(resultList)) return(list())

  failures <- list()
  for (item in resultList) {
    if (inherits(item, "expectation_failure") || inherits(item, "expectation_error")) {
      entry <- list(message = conditionMessage(item))

      # Extract backtrace if available
      if (!is.null(item$trace)) {
        entry$backtrace <- paste(format(item$trace), collapse = "\n")
      }

      # Extract source reference if available
      if (!is.null(item$srcref)) {
        entry$srcref <- paste(utils::capture.output(print(item$srcref)), collapse = "")
      }

      failures[[length(failures) + 1]] <- entry
    }
  }
  failures
}


#' Extract warning messages from result list.
#' @keywords internal
extractDetailedWarnings <- function(resultList) {
  if (!is.list(resultList)) return(list())

  warnings <- character()
  for (item in resultList) {
    if (inherits(item, "expectation_warning")) {
      warnings <- c(warnings, conditionMessage(item))
    }
  }
  unique(warnings)
}


#' Extract skip reason from result list.
#' @keywords internal
extractSkipReason <- function(resultList) {
  if (!is.list(resultList)) return(NULL)

  for (item in resultList) {
    if (inherits(item, "expectation_skip")) {
      return(conditionMessage(item))
    }
  }
  NULL
}
