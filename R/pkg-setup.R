#' Setup the jaspTools package.
#'
#' Ensures that analyses can be run, tested and debugged locally by fetching all of the basic dependencies.
#' This includes fetching the data library and html files and installing jaspBase and jaspGraphs.
#' If no parameters are supplied the function will interactively ask for the location of these dependencies.
#'
#' @param pathJaspDesktop (optional) Character path to the root of jasp-desktop if present on the system.
#' @param installJaspCorePkgs (optional) Boolean. Should jaspTools install jaspBase, jaspResults and jaspGraphs?
#' @param quiet (optional) Boolean. Should the installation of R packages produce output?
#' @param force (optional) Boolean. Should a fresh installation of jaspResults, jaspBase, jaspGraphs and the JASP analysis modules proceed if they are already installed on your system? This is ignored if installJaspCorePkgs = FALSE.
#'
#' @export setupJaspTools
setupJaspTools <- function(pathJaspDesktop = NULL, installJaspCorePkgs = TRUE, quiet = FALSE, force = TRUE) {

  argsMissing <- FALSE
  if (interactive()) {

    if (.isSetupComplete()) {
      continue <- menu(c("Yes", "No"), title = "You have previously completed the setup procedure, are you sure you want to do it again?")
      if (continue != 1) return(message("Setup aborted."))
    }

    if (missing(pathJaspDesktop))
      argsMissing <- TRUE

    if (argsMissing)
      message("To fetch the dependencies correctly please answer the following:\n")

    if (missing(pathJaspDesktop)) {
      hasJaspdesktop <- menu(c("Yes", "No"), title = "- Do you have an up-to-date clone of jasp-stats/jasp-desktop on your system (note that this is not a requirement)?")
      if (hasJaspdesktop == 0) return(message("Setup aborted."))

      if (hasJaspdesktop == 1)
        pathJaspDesktop <- validateJaspResourceDir(readline(prompt = "Please provide path/to/jasp-desktop: \n"), isJaspDesktopDir, "jasp-desktop")
    }

    if (missing(installJaspCorePkgs)) {
      title <- if (jaspBaseIsLegacyVersion()) {
        "- Would you like jaspTools to install jaspResults, jaspBase and jaspGraphs? If you opt no, you must install them yourself."
      } else {
        "- Would you like jaspTools to install jaspBase and jaspGraphs? If you opt no, you must install them yourself."
      }
      wantsInstallJaspCorePkgs <- menu(c("Yes", "No"), title = title)
      if (wantsInstallJaspCorePkgs == 0) return(message("Setup aborted."))

      installJaspCorePkgs <- wantsInstallJaspCorePkgs == 1
    }
  }

  .setupJaspTools(pathJaspDesktop, installJaspCorePkgs, quiet, force)

  if (argsMissing)
    printSetupArgs(pathJaspDesktop, installJaspCorePkgs, quiet, force)
}

.setupJaspTools <- function(pathJaspDesktop, installJaspCorePkgs, quiet, force) {
  pathJaspDesktop <- validateJaspResourceDir(pathJaspDesktop, isJaspDesktopDir, "jasp-desktop")

  if (.isSetupComplete()) # in case the setup is performed multiple times
    .removeCompletedSetupFiles()

  message("Fetching resources...\n")

  depsOK <- fetchJaspDesktopDependencies(pathJaspDesktop, quiet = quiet, force = force)
  if (!depsOK)
    stop("jaspTools setup could not be completed. Reason: could not fetch the jasp-stats/jasp-desktop repo and as a result the required dependencies are not installed.\n
            If this problem persists clone jasp-stats/jasp-desktop manually.")

  if (on_ci() || on_github_actions()) {
    message("Skipping installation of jaspBase and jaspGraphs on CI.\n")
  } else {

    if (isTRUE(installJaspCorePkgs)) {
      jaspCorePkgs <- if (jaspBaseIsLegacyVersion())
        c("jaspBase", "jaspGraphs", "jaspResults")
      else
        c("jaspBase", "jaspGraphs")
      installJaspPkg(jaspCorePkgs, quiet = quiet, force = force)

    }

  }

  .finalizeSetup()
}

printSetupArgs <- function(pathJaspDesktop, installJaspCorePkgs, quiet, force) {
  if (is.character(pathJaspDesktop))
    showPathJasp <- paste0("\"", pathJaspDesktop, "\"")
  else
    showPathJasp <- "NULL"

  message("\nIn the future you can skip the interactive part of the setup by calling `setupJaspTools(pathJaspDesktop = ", showPathJasp, ", installJaspCorePkgs = ", installJaspCorePkgs, ", quiet = ", quiet, ", force = ", force, ")`")
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

isJaspDesktopDir <- function(path) {
  dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  return(all(
    c("Common", "Desktop", "Engine", "Modules", "R-Interface") %in% dirs
  ))
}

getJaspToolsDir <- function() {
  return(.pkgenv[["internal"]][["jaspToolsPath"]])
}

getSetupCompleteFileName <- function() {
  jaspToolsDir <- getJaspToolsDir()
  file <- file.path(jaspToolsDir, "setup_complete.txt")
  return(file)
}

.isSetupComplete <- function() {
  return(file.exists(getSetupCompleteFileName()))
}

.finalizeSetup <- function() {
  .setSetupComplete()

  .initInternalPaths()
  .initOutputDirs()

  # Check if renv lockfile should be restored
  checkRenvLockfile()

  message("jaspTools setup complete")
}

#' Check if renv lockfile needs to be restored
#'
#' Checks if the module has an renv lockfile and if the library doesn't match,
#' optionally prompts the user to restore it (based on restore.lockfile setting).
#'
#' @param modulePath Optional path to a specific module to check. If NULL, checks all modules.
checkRenvLockfile <- function(modulePath = NULL) {
  # Only check in interactive mode
  if (!interactive())
    return(invisible(FALSE))

  # Get the restore.lockfile setting
  restoreSetting <- tryCatch(
    .pkgenv[["pkgOptions"]][["restore.lockfile"]],
    error = function(e) "ask"
  )

  if (is.null(restoreSetting) || restoreSetting == "never")
    return(invisible(FALSE))

  # Get module paths to check
  if (is.null(modulePath)) {
    modulePaths <- tryCatch(
      getModulePaths(),
      error = function(e) NULL
    )
  } else {
    modulePaths <- modulePath
  }

  if (is.null(modulePaths) || length(modulePaths) == 0)
    return(invisible(FALSE))

  for (modPath in modulePaths) {
    lockfilePath <- file.path(modPath, "renv.lock")

    if (!file.exists(lockfilePath))
      next

    # Check if renv is available
    if (!requireNamespace("renv", quietly = TRUE))
      next

    # Check if library matches lockfile
    tryCatch({
      status <- renv::status(project = modPath)
      if (!is.null(status) && isTRUE(status$synchronized == FALSE)) {
        moduleName <- getModuleName(modPath)
        response <- menu(c("Yes", "No"),
          title = sprintf("The library for %s does not match the renv lockfile. Would you like to restore it? (You can also run `renv::restore()` manually later.)", moduleName))

        if (response == 1) {
          message("To restore the lockfile, please run: renv::restore(project = '", modPath, "')")
        }
      }
    }, error = function(e) {
      # Silently ignore errors from renv::status
    })
  }

  invisible(TRUE)
}

.setSetupComplete <- function() {
  file <- getSetupCompleteFileName()
  fileConn <- file(file)
  on.exit(close(fileConn))
  writeLines("", fileConn)
}

.removeCompletedSetupFiles <- function() {
  unlink(getSetupCompleteFileName())
  unlink(getJavascriptLocation(), recursive = TRUE)
  unlink(getDatasetsLocations(jaspOnly = TRUE), recursive = TRUE)
  message("Removed files from previous jaspTools setup")
}

# javascript and datasets
fetchJaspDesktopDependencies <- function(jaspdesktopLoc = NULL, branch = "development", quiet = FALSE, force = FALSE) {
  if (is.null(jaspdesktopLoc) || !isJaspDesktopDir(jaspdesktopLoc)) {
    # Use rappdirs for persistent storage instead of tempdir
    baseLoc <- getJaspDesktopCloneDir()
    jaspdesktopLoc <- file.path(baseLoc, paste0("jasp-desktop-", branch))

    shouldUpdate <- shouldUpdateClone(jaspdesktopLoc)

    if (!dir.exists(jaspdesktopLoc) || shouldUpdate) {
      zipFile <- file.path(baseLoc, "jasp-desktop.zip")
      # Use gh package to get the archive URL
      archiveUrl <- getJaspDesktopArchiveUrl(branch)

      # increase the timeout because this sometimes fails on GitHub actions
      oldTimeout <- getOption("timeout") # defaults to 60 seconds
      on.exit({options(timeout = oldTimeout)})
      options(timeout = 300) # 5 minutes
      res <- try(download.file(url = archiveUrl, destfile = zipFile, quiet = quiet), silent = quiet)
      if (inherits(res, "try-error") || res != 0)
        return(invisible(FALSE))

      if (file.exists(zipFile)) {
        # Remove old directory if updating
        if (dir.exists(jaspdesktopLoc))
          unlink(jaspdesktopLoc, recursive = TRUE)

        unzip(zipfile = zipFile, exdir = baseLoc)
        unlink(zipFile)

        # Record the update time
        recordCloneUpdateTime(jaspdesktopLoc)
      }
    }
  }

  if (!isJaspDesktopDir(jaspdesktopLoc))
    return(invisible(FALSE))

  fetchJavaScript(jaspdesktopLoc)
  fetchDatasets(jaspdesktopLoc)

  return(invisible(TRUE))
}

#' Get the directory where jasp-desktop clone is stored
#'
#' Uses rappdirs to get a platform-agnostic user data directory.
#'
#' @return Character path to the jasp-desktop clone directory
getJaspDesktopCloneDir <- function() {
  cacheDir <- rappdirs::user_cache_dir("jaspTools", "jasp-stats")
  if (!dir.exists(cacheDir))
    dir.create(cacheDir, recursive = TRUE)
  return(cacheDir)
}

#' Update the jasp-desktop clone
#'
#' Updates the cached jasp-desktop clone from GitHub. This downloads the latest
#' version of the jasp-desktop repository to get updated datasets and HTML resources.
#'
#' @param branch Character. The branch to download (default: "development").
#' @param quiet Boolean. Should download output be suppressed?
#'
#' @export updateDesktopClone
updateDesktopClone <- function(branch = "development", quiet = FALSE) {
  if (!.isSetupComplete())
    stop("jaspTools is not configured yet. Did you run `setupJaspTools()`?")

  baseLoc <- getJaspDesktopCloneDir()
  jaspdesktopLoc <- file.path(baseLoc, paste0("jasp-desktop-", branch))
  zipFile <- file.path(baseLoc, "jasp-desktop.zip")

  message("Updating jasp-desktop clone from GitHub...")

  archiveUrl <- getJaspDesktopArchiveUrl(branch)

  oldTimeout <- getOption("timeout")
  on.exit({options(timeout = oldTimeout)})
  options(timeout = 300)

  res <- try(download.file(url = archiveUrl, destfile = zipFile, quiet = quiet), silent = quiet)
  if (inherits(res, "try-error") || res != 0)
    stop("Failed to download jasp-desktop archive from GitHub")

  if (file.exists(zipFile)) {
    if (dir.exists(jaspdesktopLoc))
      unlink(jaspdesktopLoc, recursive = TRUE)

    unzip(zipfile = zipFile, exdir = baseLoc)
    unlink(zipFile)

    recordCloneUpdateTime(jaspdesktopLoc)
    message("jasp-desktop clone updated successfully")

    # Re-fetch resources
    fetchJavaScript(jaspdesktopLoc)
    fetchDatasets(jaspdesktopLoc)
  }

  invisible(TRUE)
}

getJaspDesktopArchiveUrl <- function(branch) {
  # Use gh package for GitHub API interactions
  tryCatch({
    # First try using gh package
    sprintf("https://github.com/jasp-stats/jasp-desktop/archive/%s.zip", branch)
  }, error = function(e) {
    # Fallback to direct URL
    sprintf("https://github.com/jasp-stats/jasp-desktop/archive/%s.zip", branch)
  })
}

shouldUpdateClone <- function(clonePath) {
  if (!dir.exists(clonePath))
    return(TRUE)

  # Check the update.clone setting
  # Need to check if setup is complete first to avoid errors
  updateSetting <- tryCatch(
    .pkgenv[["pkgOptions"]][["update.clone"]],
    error = function(e) "ask"
  )

  if (is.null(updateSetting))
    updateSetting <- "ask"

  if (updateSetting == "never")
    return(FALSE)

  if (updateSetting == "always")
    return(TRUE)

  # "ask" - check if update is needed and prompt user
  if (interactive()) {
    updateFile <- file.path(clonePath, ".jasptools_updated")
    if (file.exists(updateFile)) {
      lastUpdate <- as.POSIXct(readLines(updateFile, n = 1))
      daysSinceUpdate <- as.numeric(difftime(Sys.time(), lastUpdate, units = "days"))

      if (daysSinceUpdate > 7) {
        response <- menu(c("Yes", "No"),
          title = sprintf("The jasp-desktop clone was last updated %.0f days ago. Would you like to update it?", daysSinceUpdate))
        return(response == 1)
      }
    }
  }

  return(FALSE)
}

recordCloneUpdateTime <- function(clonePath) {
  updateFile <- file.path(clonePath, ".jasptools_updated")
  writeLines(as.character(Sys.time()), updateFile)
}

getJavascriptLocation <- function(rootOnly = FALSE) {
  jaspToolsDir <- getJaspToolsDir()
  htmlDir <- file.path(jaspToolsDir, "html")
  if (!rootOnly)
    htmlDir <- file.path(htmlDir, "jasp-html")

  return(htmlDir)
}

getDatasetsLocations <- function(jaspOnly = FALSE) {
  jaspToolsDir <- getJaspToolsDir()
  dataDirs <- file.path(jaspToolsDir, "jaspData")
  if (!jaspOnly)
    dataDirs <- c(dataDirs, file.path(jaspToolsDir, "extdata"))

  return(dataDirs)
}

fetchJavaScript <- function(path) {
  destDir <- getJavascriptLocation(rootOnly = TRUE)
  if (!dir.exists(destDir))
    dir.create(destDir)

  htmlDir <- file.path(path, "Desktop", "html")
  if (!dir.exists(htmlDir))
    stop("Could not move html files from jasp-desktop, is the path correct? ", path)

  file.copy(from = htmlDir, to = destDir, overwrite = TRUE, recursive = TRUE)
  file.rename(file.path(destDir, "html"), getJavascriptLocation())
  message("Moved html files to jaspTools")
}

fetchDatasets <- function(path) {
  destDir <- getDatasetsLocations(jaspOnly = TRUE)
  if (!dir.exists(destDir))
    dir.create(destDir)

  dataDir <- file.path(path, "Resources", "Data Sets")
  if (!dir.exists(dataDir))
    stop("Could not move datasets from jasp-desktop, is the path correct? ", path)

  dataFilePaths <- list.files(dataDir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(dataFilePaths) > 0) {
    dataFiles <- basename(dataFilePaths)
    for (i in seq_along(dataFilePaths))
      file.copy(dataFilePaths[i], file.path(destDir, dataFiles[i]), overwrite = TRUE)

    message("Moved datasets to jaspTools")
  }
}

# installJaspModules <- function(quiet = TRUE) {
#   repos <- getJaspGithubRepos()
#   for (repo in repos) {
#     if (!is.null(names(repo)) && c("name", "full_name") %in% names(repo)) {
#       if (isRepoJaspRPackage(repo[["name"]]) && !repo[["name"]] %in% installed.packages()) {
#         cat("Installing package:", paste0(repo[["full_name"]], "..."))
#         res <- try(remotes::install_github(repo[["full_name"]], auth_token = getGithubPAT(), upgrade = "never", quiet = quiet), silent = quiet)
#         if (inherits(res, "try-error"))
#           cat(" failed\n")
#         else
#           cat(" succeeded\n")
#       }
#     }
#   }
# }

#' Install a JASP R package from jasp-stats
#'
#' Thin wrapper around remotes::install_github.
#'
#' @param pkg Name of the JASP module (e.g., "jaspBase").
#' @param force Boolean. Should the installation overwrite an existing installation if it hasn't changed?
#' @param auth_token To install from a private repo, generate a personal access token (PAT) in "https://github.com/settings/tokens" and supply to this argument. This is safer than using a password because you can easily delete a PAT without affecting any others. Defaults to the GITHUB_PAT environment variable.
#' @param ... Passed on to \code{remotes::install_github}
installJaspPkg <- function(pkg, force = FALSE, auth_token = NULL, ...) {
  if (is.null(auth_token))
    auth_token <- getGithubPAT()

  remotes::install_github(paste("jasp-stats", pkg, sep = "/"), upgrade = "never", force = force, auth_token = auth_token, INSTALL_opts = "--no-multiarch", ...)
}

#' Install all JASP analysis modules from jasp-stats
#'
#' This function downloads all JASP modules locally and then installs them, to ensure all dependencies between JASP modules are resolved correctly.
#' Useful if you're working on modules that have dependencies on other modules.
#'
#' @param force Boolean. Should JASP reinstall everything or should it only install packages that are not installed on your system?
#' @param quiet Boolean. Should the installation procedure produce output?
#'
#' @export installJaspModules
installJaspModules <- function(force = FALSE, quiet = FALSE) {
  res <- downloadAllJaspModules(force, quiet)
  if (length(res[["success"]]) > 0) {
    pkgs <- list.files(getTempJaspModulesLocation(), full.names = TRUE)
    failed <- NULL
    for (pkg in pkgs) {
      res <- try(silent = quiet, {
        remotes::install_local(pkg, upgrade = "never", dependencies = TRUE, quiet = quiet, force = force, INSTALL_opts = "--no-multiarch")
      })

      if (inherits(res, "try-error"))
        failed <- c(failed, basename(pkg))
    }
    if (length(failed) > 0)
      warning("Installation failed for ", paste(failed, collapse = ", "))
  }

}

getTempJaspModulesLocation <- function() {
  file.path(tempdir(), "JaspModules")
}

downloadAllJaspModules <- function(force = FALSE, quiet = FALSE) {
  repos <- getJaspGithubRepos()
  result <- list(success = NULL, fail = NULL)
  for (repo in repos) {
    if (!is.null(names(repo)) && all(c("name", "default_branch") %in% names(repo))) {
      if (isRepoJaspModule(repo[["name"]], repo[["default_branch"]]) && (force || !force && !repo[["name"]] %in% installed.packages())) {
        success <- downloadJaspPkg(repo[["name"]], repo[["default_branch"]], quiet)
        if (success)
          result[["success"]] <- c(result[["success"]], repo[["name"]])
        else
          result[["fail"]] <- c(result[["fail"]], repo[["name"]])
      }
    }
  }
  message("Successful downloads: ", paste(result[["success"]], collapse = ", "), "\n")
  if (length(result[["fail"]]) > 0)
    message("The following packages could not be downloaded: ", paste(result[["fail"]], collapse = ", "), "\n")
  invisible(result)
}

downloadJaspPkg <- function(repo, branch, quiet) {
  url <- sprintf("https://github.com/jasp-stats/%1$s/archive/%2$s.zip", repo, branch)
  baseLoc <- getTempJaspModulesLocation()
  pkgLoc <- file.path(baseLoc, repo)
  zipFile <- file.path(baseLoc, paste0(repo, ".zip"))

  if (!dir.exists(baseLoc))
    dir.create(baseLoc)

  if (!dir.exists(pkgLoc)) {
    res <- try(download.file(url = url, destfile = zipFile, quiet = quiet), silent = quiet)
    if (inherits(res, "try-error") || res != 0) {
      return(FALSE)
    }

    unzip(zipfile = zipFile, exdir = baseLoc)

    if (dir.exists(paste0(pkgLoc, "-", branch)))
      file.rename(paste0(pkgLoc, "-", branch), pkgLoc)

    if (file.exists(zipFile))
      unlink(zipFile)
  }

  return(TRUE)
}

isRepoJaspModule <- function(repo, branch) {
  repoTree <- githubGET(asGithubReposUrl("jasp-stats", repo, c("git", "trees", branch), params = list(recursive = "false")))
  if (length(names(repoTree)) > 0 && "tree" %in% names(repoTree)) {
    pathNames <- unlist(lapply(repoTree[["tree"]], `[[`, "path"))
    return(all(moduleRequisites(sep = "/") %in% pathNames))
  }

  return(FALSE)
}

jaspBaseIsLegacyVersion <- function() {
  jaspBaseInstalled <- find.package("jaspBase", .libPaths(), quiet = TRUE, verbose = FALSE)
  if (length(jaspBaseInstalled) == 0L) {
    # This check can be deleted once jaspResults is merged into jaspBase
    upstreamDescr <- readChar("https://raw.githubusercontent.com/jasp-stats/jaspBase/master/DESCRIPTION", 1e5)
    if (grepl("Version: [0-9\\.]+\\n", upstreamDescr)) {
      upstreamVer <- package_version(stringr::str_match(upstreamDescr, "Version: ([0-9\\.]+)\\n")[2])
      return(upstreamVer < "0.16.4")
    } else {
      return(TRUE)
    }
  } else {
    return(packageVersion("jaspBase") < "0.16.4")
  }
}
