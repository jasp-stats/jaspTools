.jaspToolsSubprocessPayload <- function(extra = list(), env = list()) {
  payload <- list(
    wd = getwd(),
    libPaths = .libPaths(),
    sourcePath = .jaspToolsSourcePath(),
    pkgOptions = .pkgenv[["pkgOptions"]],
    rOptions = .jaspToolsSubprocessOptions(),
    env = env
  )

  if (length(extra) > 0L)
    payload[names(extra)] <- extra

  payload
}

.jaspToolsSubprocessOptions <- function() {
  optionNames <- c("jaspLegacyRngKind")
  currentOptions <- options()
  currentOptions[intersect(optionNames, names(currentOptions))]
}

.jaspToolsSubprocessEnv <- function(childFlag, inherited = character()) {
  env <- if (length(inherited) > 0L)
    as.list(Sys.getenv(inherited, unset = ""))
  else
    list()
  env[[childFlag]] <- "true"
  env
}

.jaspToolsRunSubprocess <- function(task, payload, failureMessage,
                                    isError = function(result) FALSE) {
  logPath <- tempfile(paste0("jaspTools-", task, "-"), fileext = ".log")
  result <- tryCatch(
    callr::r(
      func = function(task, payload) {
        .libPaths(payload$libPaths)
        sourcePath <- payload$sourcePath
        if (!is.null(sourcePath) && is.character(sourcePath) && length(sourcePath) == 1L &&
            file.exists(file.path(sourcePath, "R", "run.R"))) {
          if (!requireNamespace("pkgload", quietly = TRUE))
            stop("pkgload is required to run jaspTools child processes from a source checkout", call. = FALSE)

          pkgload::load_all(sourcePath, quiet = TRUE)
        } else {
          suppressPackageStartupMessages(library(jaspTools))
        }

        jaspTools:::.jaspToolsSubprocessMain(task, payload)
      },
      args = list(task = task, payload = payload),
      libpath = payload$libPaths,
      stdout = logPath,
      stderr = logPath,
      env = .jaspToolsNormalizeSubprocessEnv(payload$env),
      cmdargs = c("--slave", "--no-save", "--no-restore"),
      error = "error"
    ),
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "jaspTools.subprocessError")
    }
  )

  resultIsError <- isTRUE(isError(result)) || inherits(result, "jaspTools.subprocessError")
  if (resultIsError) {
    stop(
      failureMessage,
      ": ",
      .jaspToolsSubprocessErrorMessage(result),
      ". Log: ",
      logPath,
      .jaspToolsSubprocessLogTail(logPath),
      call. = FALSE
    )
  }

  if (is.list(result) && !inherits(result, "jaspTools.subprocessError"))
    result$output <- .jaspToolsSubprocessLog(logPath)

  unlink(logPath, force = TRUE)
  result
}

.jaspToolsNormalizeSubprocessEnv <- function(env) {
  if (!is.list(env) || length(env) == 0L)
    return(character(0))

  values <- unlist(env, use.names = TRUE)
  values[!is.na(names(values)) & nzchar(names(values))]
}

.jaspToolsSubprocessMain <- function(task, payload) {
  .jaspToolsRestoreForSubprocess(payload)

  switch(
    task,
    runAnalysis = .jaspToolsRunAnalysisSubprocess(payload$args),
    stop("Unknown jaspTools subprocess task: ", task, call. = FALSE)
  )
}

.jaspToolsRestoreForSubprocess <- function(payload) {
  .libPaths(payload$libPaths)
  setwd(payload$wd)

  if (is.list(payload$env) && length(payload$env) > 0L)
    do.call(Sys.setenv, payload$env)
  if (is.list(payload$rOptions) && length(payload$rOptions) > 0L)
    do.call(options, payload$rOptions)

  .jaspToolsRestorePkgOptionsForSubprocess(payload$pkgOptions)
  invisible(NULL)
}

.jaspToolsRestorePkgOptionsForSubprocess <- function(pkgOptions) {
  if (!is.list(pkgOptions) || length(pkgOptions) == 0L)
    return(invisible(NULL))

  pkgEnv <- get(".pkgenv", envir = asNamespace("jaspTools"), inherits = FALSE)
  pkgEnv[["internal"]][["setupCompleteOverride"]] <- TRUE
  pkgEnv[["pkgOptions"]][names(pkgOptions)] <- pkgOptions
  get(".initOutputDirs", envir = asNamespace("jaspTools"), inherits = FALSE)()
  invisible(NULL)
}

.jaspToolsRunAnalysisSubprocess <- function(args) {
  warnings <- character(0)
  messages <- character(0)
  result <- tryCatch(
    withCallingHandlers(
      do.call(jaspTools::runAnalysis, args),
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        tryInvokeRestart("muffleMessage")
      },
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        tryInvokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "jaspTools.subprocessError")
    }
  )

  list(
    result = result,
    lastResults = tryCatch(jaspTools:::.getInternal("lastResults"), error = function(e) NULL),
    htmlFiles = tryCatch(jaspTools:::collectSubprocessHtmlFiles(), error = function(e) NULL),
    messages = unique(messages),
    warnings = unique(warnings)
  )
}

.jaspToolsSourcePath <- function() {
  namespacePath <- tryCatch(
    getNamespaceInfo("jaspTools", "path"),
    error = function(e) NULL
  )

  if (!is.character(namespacePath) || length(namespacePath) != 1L)
    return(NULL)
  if (!file.exists(file.path(namespacePath, "R", "run.R")))
    return(NULL)

  normalizePath(namespacePath, winslash = "/", mustWork = FALSE)
}

.jaspToolsSubprocessErrorMessage <- function(result) {
  if (is.list(result) && !is.null(result$message))
    return(result$message)
  if (is.list(result) && inherits(result$result, "jaspTools.subprocessError"))
    return(result$result$message)

  "subprocess failed before returning a result"
}

.jaspToolsSubprocessLogTail <- function(logPath, n = 40L) {
  if (!file.exists(logPath))
    return("")

  logTail <- paste(utils::tail(readLines(logPath, warn = FALSE), n), collapse = "\n")
  if (nzchar(logTail)) paste0("\n", logTail) else ""
}

.jaspToolsSubprocessLog <- function(logPath) {
  if (!file.exists(logPath))
    return(character(0))

  readLines(logPath, warn = FALSE)
}

.stopIfJaspToolsSubprocessError <- function(result) {
  if (inherits(result, "jaspTools.subprocessError"))
    stop(result$message, call. = FALSE)

  invisible(result)
}
