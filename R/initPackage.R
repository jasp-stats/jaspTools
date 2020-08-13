#' Initialize the jaspTools package in an interactive manner.
#'
#' This is a wrapper around \code{initJaspTools} that asks a few questions to determine the desired setup procedure.
#' Ensures that analyses can be run, tested and debugged locally by fetching all of the basic dependencies.
#' This includes fetching the data library and html files, installing jaspBase and jaspGraphs and adding the required-files.
#'
#' @export initJaspToolsInteractive
initJaspToolsInteractive <- function() {
  if (interactive()) {

    if (isSetupComplete()) {
      continue <- menu(c("Yes", "No"), title = "You have previously completed the setup procedure, are you sure you want to do it again?")
      if (continue == 1)
        removeCompletedSetupFiles()
      else
        return(message("Setup aborted."))
    }

    message("Three questions will follow to correctly fetch dependencies required by jaspTools.\n")

    wantsInstallJaspPkgs  <- menu(c("Yes", "No"), title = "1. Would you like jaspTools to install the JASP modules, jaspGraphs and jaspBase located at github.com/jasp-stats?")
    if (wantsInstallJaspPkgs == 0) return(message("Setup aborted."))

    hasJaspRequiredPkgs <- menu(c("Yes", "No"), title = "2. Do you have a clone of jasp-stats/jasp-required-files on your system?")
    if (hasJaspRequiredPkgs == 0) return(message("Setup aborted."))

    pathJaspRequiredPkgs <- NULL
    if (hasJaspRequiredPkgs == 1)
      pathJaspRequiredPkgs <- validateJaspResourceDir(readline(prompt = "Please provide path/to/jasp-required-files: \n"), isJaspRequiredFilesDir, "jasp-required-files")

    hasJaspdesktop <- menu(c("Yes", "No"), title = "3. Do you have a clone of jasp-stats/jasp-desktop on your system?")
    if (hasJaspdesktop == 0) return(message("Setup aborted."))

    pathJaspDesktop <- NULL
    if (hasJaspdesktop == 1)
      pathJaspDesktop <- validateJaspResourceDir(readline(prompt = "Please provide path/to/jasp-desktop: \n"), isJaspDesktopDir, "jasp-desktop")

    installJaspPkgs <- wantsInstallJaspPkgs == 1

    initJaspTools(installJaspPkgs, pathJaspRequiredPkgs, pathJaspDesktop)

    showInitArgsForFutureReference(installJaspPkgs, pathJaspRequiredPkgs, pathJaspDesktop)
  }
}

#' Initialize the jaspTools package.
#'
#' Ensures that analyses can be run, tested and debugged locally by fetching all of the basic dependencies.
#' This includes fetching the data library and html files, installing jaspBase and jaspGraphs and adding the required-files.
#'
#'
#' @param installJaspPkgs Boolean. Should jaspTools install all the JASP modules, jaspBase and jaspGraphs?
#' @param pathJaspRequiredPkgs Character path to the root of jasp-required-files if present on the system.
#' @param pathJaspDesktop Character path to the root of jasp-desktop if present on the system.
#' @param quiet Boolean. Should the installation produce output?
#'
#' @export initJaspTools
initJaspTools <- function(installJaspPkgs = TRUE, pathJaspRequiredPkgs = NULL, pathJaspDesktop = NULL, quiet = FALSE) {
  pathJaspRequiredPkgs <- validateJaspResourceDir(pathJaspRequiredPkgs, isJaspRequiredFilesDir, "jasp-required-files")
  pathJaspDesktop <- validateJaspResourceDir(pathJaspDesktop, isJaspDesktopDir, "jasp-desktop")

  message("Fetching resources...\n")

  depsOK <- fetchJaspDesktopDependencies(pathJaspDesktop, quiet = quiet)

  if (is.character(pathJaspRequiredPkgs))
    setLocationJaspRequiredFiles(pathJaspRequiredPkgs)
  else if (is.null(pathJaspRequiredPkgs) && isJaspRequiredFilesLocationSet())
    removeJaspRequiredFilesLocationFile()

  if (isTRUE(installJaspPkgs))
    installAllJaspPkgs()

  if (!depsOK) {
    message("jaspTools setup could not be completed. Reason: could not fetch the jasp-stats/jasp-desktop repo and as a result the required dependencies are not installed.\n
            If this problem persists clone jasp-stats/jasp-desktop manually and provide the path to `initJaspTools()` in `pathJaspDesktop`.")
  } else {
    setSetupComplete()
    setupJaspToolsInternals(TRUE)
  }
}

showInitArgsForFutureReference <- function(installJaspPkgs, pathJaspRequiredPkgs, pathJaspDesktop) {
  if (is.character(pathJaspRequiredPkgs))
    showPathPkgs <- paste0("\"", pathJaspRequiredPkgs, "\"")
  else
    showPathPkgs <- "NULL"

  if (is.character(pathJaspDesktop))
    showPathJasp <- paste0("\"", pathJaspDesktop, "\"")
  else
    showPathJasp <- "NULL"

  message("\nIn the future you can skip these questions by calling `initJaspTools(", installJaspPkgs, ", ", showPathPkgs, ", ", showPathJasp, ")`")
}

validateJaspResourceDir <- function(path, validationFn, title) {
  if (!is.null(path)) {
    if (is.character(path))
      path <- normalizePath(gsub("[\"']", "", path))

    if (!is.character(path) || !do.call(validationFn, list(path = path)))
      stop("Invalid path provided for ", title, "; could not find the correct resources within: ", path)
  }
  return(path)
}

isJaspRequiredFilesDir <- function(path) {
  dirs <- dir(path)
  if (getOS() == "windows")
    return("R" %in% dirs && dir.exists(file.path(path, "R", "library")))
  else if (getOS() == "osx")
    return("Frameworks" %in% dirs && dir.exists(file.path(path, "Frameworks", "R.framework")))
  else if (getOS() == "linux")
    stop("jasp-required-files are not used on Linux")
}

isJaspDesktopDir <- function(path) {
  dirs <- dir(path, pattern = "JASP-*")
  return(all(c("JASP-Common", "JASP-Desktop", "JASP-Engine", "JASP-R-Interface") %in% dirs))
}

findRequiredPkgs <- function(pathToRequiredFiles) {
  result <- ""
  if (isJaspRequiredFilesDir(pathToRequiredFiles)) {
    if (getOS() == "windows")
      result <- getPkgDirWindows(pathToRequiredFiles)
    else if (getOS() == "osx")
      result <- getPkgDirOSX(pathToRequiredFiles)
  }
  return(result)
}

getPkgDirWindows <- function(pathToRequiredFiles) {
  potentialPkgDir <- file.path(pathToRequiredFiles, "R", "library")

  if (dirHasBundledPackages(potentialPkgDir, hasPkg="Rcpp"))
    return(potentialPkgDir)

  return("")
}

getPkgDirOSX <- function(pathToRequiredFiles) {
  basePathPkgs <- file.path(pathToRequiredFiles, "Frameworks", "R.framework", "Versions")
  rVersions <- list.files(basePathPkgs)
  if (identical(rVersions, character(0)))
    return("")

  rVersions <- suppressWarnings(as.numeric(rVersions))
  r <- sort(rVersions, decreasing = TRUE)[1]
  potentialPkgDir <- file.path(basePathPkgs, r, "Resources", "library")

  if (dirHasBundledPackages(potentialPkgDir, hasPkg="Rcpp"))
    return(potentialPkgDir)

  return("")
}

dirHasBundledPackages <- function(dir, hasPkg) {
  if (!dir.exists(dir))
    return(FALSE)

  pkgs <- list.files(dir)
  if (!identical(pkgs, character(0)) && hasPkg %in% pkgs) {
    return(TRUE)
  }

  return(FALSE)
}

getOS <- function() {
  os <- NULL
  if (!is.null(Sys.info())) {
    os <- Sys.info()["sysname"]
    if (os == "Darwin")
      os <- "osx"
  } else {
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

setupJaspToolsInternals <- function(ready) {
  .internal <- list2env(list(
    dataset        = NULL,
    state          = NULL,
    modulesMd5Sums = NULL
  ))

  .pkgOptions <- list2env(list())

  if (ready) {
    .pkgOptions <- list2env(list(
      module.dirs = NULL,
      reinstall.modules     = TRUE,
      html.dir              = getJavascriptLocation(),
      data.dir              = getDatasetsLocation(),
      pkgs.dir              = readJaspRequiredFilesLocation(),
      locale                = "en_US.UTF-8",
      .ppi                  = 96
    ))

    # create the temp (html) directory for the output
    pathToTools <- file.path(tempdir(), "jaspTools")
    if (!dir.exists(pathToTools)) {
      dir.create(file.path(pathToTools, "html", "plots"), recursive = TRUE)
      dir.create(file.path(pathToTools, "state"))
      message(paste("Note: temp output files may be found at", pathToTools))
    }

  }

  # create globals for setup / JASP to find
  # env <- as.environment("package:jaspTools")
  env <- try(as.environment("package:jaspTools"), silent = TRUE)
  if (inherits(env, "try-error"))
    stop("please load jaspTools first!")
  isLocked <- environmentIsLocked(env)
  if (isLocked) {
    try(silent = TRUE, {
      unlockBinding(".pkgOptions",   env)
      unlockBinding(".internal",     env)
      unlockBinding(".ppi",          env)
      unlockBinding(".baseCitation", env)
      unlockBinding(".masks",        env)
    })
  }

  assign(".pkgOptions",   .pkgOptions,          envir = env)
  assign(".internal",     .internal,            envir = env)
  assign(".ppi",          NULL,                 envir = env)
  assign(".baseCitation", "x",                  envir = env)
  assign(".masks",        ".ppi",               envir = env)

  if (isLocked) {
    try(silent = TRUE, {
      lockBinding(".pkgOptions",   env)
      lockBinding(".internal",     env)
      lockBinding(".ppi",          env)
      lockBinding(".baseCitation", env)
      lockBinding(".masks",        env)
    })
  }

  # this is not used in combination with getAnywhere() in the code so it cannot be found
  assign(".automaticColumnEncDecoding", FALSE, envir = .GlobalEnv)
}

getJaspToolsDir <- function() {
  require(jaspTools)
  return(path.package("jaspTools", quiet = FALSE))
}

getSetupCompleteFileName <- function() {
  jaspToolsDir <- getJaspToolsDir()
  file <- file.path(jaspToolsDir, "setup_complete.txt")
  return(file)
}

getJaspRequiredFilesLocationFileName <- function() {
  jaspToolsDir <- getJaspToolsDir()
  file <- file.path(jaspToolsDir, "jasp-required-files_location.txt")
  return(file)
}

isSetupComplete <- function() {
  return(file.exists(getSetupCompleteFileName()))
}

isJaspRequiredFilesLocationSet <- function() {
  return(file.exists(getJaspRequiredFilesLocationFileName()))
}

readJaspRequiredFilesLocation <- function() {
  loc <- NULL
  if (isJaspRequiredFilesLocationSet())
    loc <- readLines(getJaspRequiredFilesLocationFileName())

  return(loc)
}

setSetupComplete <- function() {
  file <- getSetupCompleteFileName()
  fileConn <- file(file)
  on.exit(close(fileConn))
  writeLines("", fileConn)

  message("jaspTools setup complete")
}

removeCompletedSetupFiles <- function() {
  removeSetupCompleteFile()
  removeJaspRequiredFilesLocationFile()
}

removeSetupCompleteFile <- function() {
  if (isSetupComplete())
    file.remove(getSetupCompleteFileName())
}

removeJaspRequiredFilesLocationFile <- function() {
  if (isJaspRequiredFilesLocationSet())
    file.remove(getJaspRequiredFilesLocationFileName())
}

setLocationJaspRequiredFiles <- function(pathToRequiredFiles) {
  if (!dir.exists(pathToRequiredFiles))
    stop("jasp-required-files folder does not exist at ", pathToRequiredFiles)

  path <- normalizePath(pathToRequiredFiles)
  libPath <- findRequiredPkgs(path)
  if (libPath == "")
    stop("Could not locate the R packages within ", pathToRequiredFiles)

  file <- getJaspRequiredFilesLocationFileName()
  fileConn <- file(file)
  on.exit(close(fileConn))
  writeLines(libPath, fileConn)

  message(sprintf("Created %s", file))
}
