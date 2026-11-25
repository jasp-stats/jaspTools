#' Setup the jaspTools package.
#'
#' Ensures that analyses can be run, tested and debugged locally by fetching all of the basic dependencies.
#' This includes fetching the data library and html files and installing jaspBase and jaspGraphs.
#' If no parameters are supplied the function will interactively ask for the location of these dependencies.
#'
#' @param pathJaspDesktop (optional) Character path to the root of jasp-desktop if present on the system.
#' @param installJaspModules (optional) Boolean. Should jaspTools install all the JASP analysis modules as R packages (e.g., jaspAnova, jaspFrequencies)?
#' @param installJaspCorePkgs (optional) Boolean. Should jaspTools install jaspBase and jaspGraphs?
#' @param quiet (optional) Boolean. Should the installation of R packages produce output?
#' @param force (optional) Boolean. Should a fresh installation of jaspBase, jaspGraphs and the JASP analysis modules proceed if they are already installed on your system? This is ignored if installJaspCorePkgs = FALSE.
#' @param jaspVersion (optional) Character. The JASP version to run the analyses with: \code{"development"} (the default), or
#'   the tag of a jaspSyntax release built from a JASP version, e.g. \code{"v0.99.1"}. Defaults to the environment variable
#'   \code{JASPTOOLS_JASP_VERSION} when that is set, so CI can choose it.
#'
#' @details
#' With \code{jaspVersion = "development"} jaspTools uses jasp-desktop's development branch and the jaspSyntax and jaspBase
#' that are installed (on CI, those of the module's lockfile).
#'
#' With the tag of a JASP version, jaspTools installs, also on CI, jaspSyntax at that tag with the SyntaxInterface library
#' of that release. The \code{jasp-version.json} of that release gives the jasp-desktop commit of that JASP version and
#' the jaspBase it pins: that jaspBase is installed, and the html files and data sets are taken from that commit.
#' Call it in a fresh R session: an installed package that is already loaded keeps running its old code.
#'
#' @export setupJaspTools
setupJaspTools <- function(pathJaspDesktop = NULL, installJaspModules = FALSE, installJaspCorePkgs = TRUE, quiet = FALSE, force = TRUE, jaspVersion = defaultJaspVersion()) {

  argsMissing <- FALSE
  if (interactive()) {

    if (.isSetupComplete()) {
      continue <- menu(c("Yes", "No"), title = "You have previously completed the setup procedure, are you sure you want to do it again?")
      if (continue != 1) return(message("Setup aborted."))
    }

    if (missing(installJaspModules) || missing(pathJaspDesktop))
      argsMissing <- TRUE

    if (argsMissing)
      message("To fetch the dependencies correctly please answer the following:\n")

    if (missing(pathJaspDesktop)) {
      hasJaspdesktop <- menu(c("Yes", "No"), title = "- Do you have an up-to-date clone of jasp-stats/jasp-desktop on your system (note that this is not a requirement)?")
      if (hasJaspdesktop == 0) return(message("Setup aborted."))

      if (hasJaspdesktop == 1)
        pathJaspDesktop <- validateJaspResourceDir(readline(prompt = "Please provide path/to/jasp-desktop: \n"), isJaspDesktopDir, "jasp-desktop")
    }

    if (missing(installJaspModules)) {
      wantsInstallJaspModules  <- menu(c("Yes", "No"), title = "- Would you like jaspTools to install all the JASP analysis modules located at github.com/jasp-stats? This is useful if the module(s) you are working on requires functions from other JASP analysis modules.")
      if (wantsInstallJaspModules == 0) return(message("Setup aborted."))

      installJaspModules <- wantsInstallJaspModules == 1
    }

    if (missing(installJaspCorePkgs)) {
      wantsInstallJaspCorePkgs <- menu(c("Yes", "No"), title = "- Would you like jaspTools to install jaspBase and jaspGraphs? If you opt no, you must install them yourself.")
      if (wantsInstallJaspCorePkgs == 0) return(message("Setup aborted."))

      installJaspCorePkgs <- wantsInstallJaspCorePkgs == 1
    }
  }

  .setupJaspTools(pathJaspDesktop, installJaspModules, installJaspCorePkgs, quiet, force, jaspVersion)

  if (argsMissing)
    printSetupArgs(pathJaspDesktop, installJaspModules, installJaspCorePkgs, quiet, force, jaspVersion)
}

.setupJaspTools <- function(pathJaspDesktop, installJaspModules, installJaspCorePkgs, quiet, force, jaspVersion = "development") {
  pathJaspDesktop <- validateJaspResourceDir(pathJaspDesktop, isJaspDesktopDir, "jasp-desktop")

  # Before anything is changed, so a JASP version that does not exist leaves the setup as it was.
  version <- readJaspVersion(jaspVersion)

  if (.isSetupComplete()) # in case the setup is performed multiple times
    .removeCompletedSetupFiles()

  if (!isDevelopmentJaspVersion(version)) {
    message("Installing the packages of JASP ", version[["jaspVersion"]], "...\n")
    installJaspVersionPkgs(version, quiet = quiet, force = force)
  }

  message("Fetching resources...\n")

  depsOK <- fetchJaspDesktopDependencies(pathJaspDesktop, branch = version[["jaspDesktop"]][["ref"]], quiet = quiet, force = force)
  if (!depsOK)
    stop("jaspTools setup could not be completed. Reason: could not fetch the jasp-stats/jasp-desktop repo and as a result the required dependencies are not installed.\n
            If this problem persists clone jasp-stats/jasp-desktop manually.")

  if (on_ci() || on_github_actions()) {
    message("Skipping installation of jaspBase and jaspGraphs on CI.\n")
  } else {

    if (isTRUE(installJaspCorePkgs)) {
      # A JASP version comes with its own jaspBase (installed above).
      corePkgs <- if (isDevelopmentJaspVersion(version)) c("jaspBase", "jaspGraphs") else "jaspGraphs"
      installJaspPkg(corePkgs, quiet = quiet, force = force)

    }

    if (isTRUE(installJaspModules))
      installJaspModules(force = force, quiet = quiet)

  }

  .finalizeSetup()
}

printSetupArgs <- function(pathJaspDesktop, installJaspModules, installJaspCorePkgs, quiet, force, jaspVersion = "development") {
  if (is.character(pathJaspDesktop))
    showPathJasp <- paste0("\"", pathJaspDesktop, "\"")
  else
    showPathJasp <- "NULL"

  message("\nIn the future you can skip the interactive part of the setup by calling `setupJaspTools(pathJaspDesktop = ", showPathJasp, ", installJaspModules = ", installJaspModules, ", installJaspCorePkgs = ", installJaspCorePkgs, ", quiet = ", quiet, ", force = ", force, ", jaspVersion = \"", jaspVersion, "\")`")
}

# ---- JASP versions ----
#
# A JASP version is a release of jaspSyntax built from a jasp-desktop tag (its Build SyntaxInterface Libraries workflow):
# jaspSyntax is installed at that tag, with the SyntaxInterface library of that release. The jasp-version.json of the
# release gives what the tag does not tell, the jasp-desktop and jaspBase commits of that version:
#   { "schema": 1, "jaspVersion": "v0.99.1",
#     "jaspDesktop": { "repo": "jasp-stats/jasp-desktop", "ref": "v0.99.1", "sha": "<sha>" },
#     "jaspBase":    { "repo": "jasp-stats/jaspBase",     "sha": "<sha>" }, ... }
# "development" is no release: jasp-desktop's development branch with the jaspSyntax and jaspBase that are installed.

jaspVersionSchema    <- 1L
jaspSyntaxReleaseRepo <- "jasp-stats/jaspSyntax"

defaultJaspVersion <- function() {
  version <- Sys.getenv("JASPTOOLS_JASP_VERSION", unset = "")
  if (nzchar(version)) version else "development"
}

isDevelopmentJaspVersion <- function(version) {
  identical(version[["jaspVersion"]], "development")
}

readJaspVersion <- function(jaspVersion) {
  if (!is.character(jaspVersion) || length(jaspVersion) != 1L || is.na(jaspVersion) || !nzchar(jaspVersion))
    stop("`jaspVersion` must be \"development\" or the tag of a JASP version, e.g. \"v0.99.1\".", call. = FALSE)

  if (identical(jaspVersion, "development"))
    return(list(jaspVersion = "development", jaspDesktop = list(ref = "development")))

  # The tag ends up in URLs and in an environment variable of the configure script of jaspSyntax.
  if (!grepl("^[A-Za-z0-9._-]+$", jaspVersion))
    stop("`jaspVersion` \"", jaspVersion, "\" is not a tag: only letters, digits, '.', '_' and '-' are allowed.", call. = FALSE)

  version <- downloadJaspVersionFile(jaspVersion)
  if (is.null(version))
    stop("Release ", jaspVersion, " of ", jaspSyntaxReleaseRepo, " has no jasp-version.json, so jaspTools does not know which ",
         "packages belong to JASP ", jaspVersion, ". Use \"development\" or a JASP version whose release has that file ",
         "(see https://github.com/", jaspSyntaxReleaseRepo, "/releases).", call. = FALSE)

  validateJaspVersion(version, jaspVersion)
}

# NULL when the release or the file does not exist.
downloadJaspVersionFile <- function(jaspVersion) {
  url  <- sprintf("https://github.com/%s/releases/download/%s/jasp-version.json", jaspSyntaxReleaseRepo, jaspVersion)
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  res <- suppressWarnings(try(utils::download.file(url, destfile = file, quiet = TRUE, mode = "wb"), silent = TRUE))
  if (inherits(res, "try-error") || !identical(as.integer(res), 0L) || !file.exists(file))
    return(NULL)

  jsonlite::fromJSON(file, simplifyVector = FALSE)
}

validateJaspVersion <- function(version, jaspVersion) {
  bad <- function(what) stop("The jasp-version.json of JASP ", jaspVersion, " ", what, call. = FALSE)

  if (!is.list(version))
    bad("is not a JSON object.")
  if (!identical(as.integer(version[["schema"]]), jaspVersionSchema))
    bad(paste0("has schema ", format(version[["schema"]]), ", this jaspTools reads schema ", jaspVersionSchema, ": update jaspTools."))

  isString <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  for (field in list(c("jaspDesktop", "sha"), c("jaspBase", "repo"), c("jaspBase", "sha")))
    if (!isString(version[[field[1L]]][[field[2L]]]))
      bad(paste0("has no ", paste(field, collapse = "."), "."))

  # jasp-desktop is downloaded by commit: its archive of a tag drops the "v" from the directory name.
  version[["jaspDesktop"]][["ref"]] <- version[["jaspDesktop"]][["sha"]]
  version[["jaspVersion"]]          <- jaspVersion

  version
}

installJaspVersionPkgs <- function(version, quiet = FALSE, force = TRUE) {
  pkgs <- c("jaspSyntax", "jaspBase")
  loaded <- pkgs[vapply(pkgs, isPkgLoaded, logical(1L))]
  if (length(loaded) > 0L)
    stop(paste(loaded, collapse = " and "), if (length(loaded) == 1L) " is" else " are", " already loaded in this R session, ",
         "which would keep running the code that is installed now. Restart R and call setupJaspTools(jaspVersion = \"",
         version[["jaspVersion"]], "\") before anything else.", call. = FALSE)

  tag <- version[["jaspVersion"]]
  # configure downloads the SyntaxInterface library of this release, also when the configure at the tag names another one.
  withEnvVars(c(JASPSYNTAX_RELEASE_TAG = tag),
    installGithubPkg(paste0(jaspSyntaxReleaseRepo, "@", tag), quiet = quiet, force = force)
  )

  jaspBase <- version[["jaspBase"]]
  installGithubPkg(paste0(jaspBase[["repo"]], "@", jaspBase[["sha"]]), quiet = quiet, force = force)

  invisible(TRUE)
}

isPkgLoaded <- function(pkg) isNamespaceLoaded(pkg)

installGithubPkg <- function(repo, quiet = FALSE, force = TRUE) {
  remotes::install_github(repo, upgrade = "never", force = force, quiet = quiet, auth_token = getGithubPAT(), INSTALL_opts = "--no-multiarch")
}

# Sets the environment variables while `code` runs, and restores them afterwards.
withEnvVars <- function(vars, code) {
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  on.exit({
    unset <- is.na(old)
    if (any(unset))  Sys.unsetenv(names(old)[unset])
    if (any(!unset)) do.call(Sys.setenv, as.list(old[!unset]))
  })
  do.call(Sys.setenv, as.list(vars))
  force(code)
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

  message("jaspTools setup complete")
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
    baseLoc <- tempdir()
    jaspdesktopLoc <- file.path(baseLoc, paste0("jasp-desktop-", branch))
    if (!dir.exists(jaspdesktopLoc)) {
      zipFile <- file.path(baseLoc, "jasp-desktop.zip")
      url <- sprintf("https://github.com/jasp-stats/jasp-desktop/archive/%s.zip", branch)

      # increase the timeout because this sometimes fails on GitHub actions
      oldTimeout <- getOption("timeout") # defaults to 60 seconds
      on.exit({options(timeout = oldTimeout)})
      options(timeout = 300) # 5 minutes
      res <- try(download.file(url = url, destfile = zipFile, quiet = quiet), silent = quiet)
      if (inherits(res, "try-error") || res != 0)
        return(invisible(FALSE))

      if (file.exists(zipFile)) {
        unzip(zipfile = zipFile, exdir = baseLoc)
        unlink(zipFile)
      }
    }
  }

  if (!isJaspDesktopDir(jaspdesktopLoc))
    return(invisible(FALSE))

  fetchJavaScript(jaspdesktopLoc)
  fetchDatasets(jaspdesktopLoc)

  return(invisible(TRUE))
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
