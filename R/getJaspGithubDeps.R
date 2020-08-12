# javascript, datasets, jaspResults
fetchJaspDesktopDependencies <- function(jaspdesktopLoc = NULL, branch = "stable", quiet = FALSE) {
  if (is.null(jaspdesktopLoc) || !isJaspDesktopDir(jaspdesktopLoc)) {
    baseLoc <- tempdir()
    jaspdesktopLoc <- file.path(baseLoc, paste0("jasp-desktop-", branch))
    if (!dir.exists(jaspdesktopLoc)) {
      zipFile <- file.path(baseLoc, "jasp-desktop.zip")
      url <- sprintf("https://github.com/jasp-stats/jasp-desktop/archive/%s.zip", branch)

      res <- try(download.file(url = url, destfile = zipFile, quiet = quiet), silent = quiet)
      if (inherits(res, "try-error") || res != 0)
        return(invisible(FALSE))

      if (file.exists(zipFile)) {
        unzip(zipfile = zipFile, exdir = baseLoc)
        file.remove(zipFile)
      }
    }
  }

  if (!isJaspDesktopDir(jaspdesktopLoc))
    return(invisible(FALSE))

  fetchJavaScript(jaspdesktopLoc)
  fetchDatasets(jaspdesktopLoc)
  installJaspResults(jaspdesktopLoc)

  return(invisible(TRUE))
}

getJavascriptLocation <- function(rootOnly = FALSE) {
  jaspToolsDir <- getJaspToolsDir()
  htmlDir <- file.path(jaspToolsDir, "html")
  if (!rootOnly)
    htmlDir <- file.path(htmlDir, "jasp-html")

  return(htmlDir)
}

getDatasetsLocation <- function() {
  jaspToolsDir <- getJaspToolsDir()
  dataDir <- file.path(jaspToolsDir, "data")
  return(dataDir)
}

fetchJavaScript <- function(path) {
  destDir <- getJavascriptLocation(rootOnly = TRUE)
  if (!dir.exists(destDir))
    dir.create(destDir)

  htmlDir <- file.path(path, "JASP-Desktop", "html")
  if (!dir.exists(htmlDir))
    stop("Could not move html files from jasp-desktop, is the path correct? ", path)

  file.copy(from = htmlDir, to = destDir, overwrite = TRUE, recursive = TRUE)
  file.rename(file.path(destDir, "html"), getJavascriptLocation())
  message("Moved html files to jaspTools")
}

fetchDatasets <- function(path) {
  destDir <- getDatasetsLocation()
  if (!dir.exists(destDir))
    dir.create(destDir)

  dataDir <- file.path(path, "Resources", "Data Sets")
  testDataDir <- file.path(path, "JASP-Tests", "R", "tests", "datasets")
  if (!dir.exists(dataDir) || !dir.exists(testDataDir))
    stop("Could not move datasets from jasp-desktop, is the path correct? ", path)

  dataFilePaths <- list.files(c(dataDir, testDataDir), pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(dataFilePaths) > 0) {
    dataFiles <- basename(dataFilePaths)
    for (i in seq_along(dataFilePaths))
      file.copy(dataFilePaths[i], file.path(destDir, dataFiles[i]), overwrite = TRUE)

    message("Moved datasets to jaspTools")
  }
}

installJaspResults <- function(path) {
  if (isNamespaceLoaded("jaspResults"))
    unloadNamespace("jaspResults")

  jaspResultsDir <- file.path(path, "JASP-R-Interface", "jaspResults")
  if (!dir.exists(jaspResultsDir))
    stop("Could not locate jaspResults inside ", path)

  install.packages(jaspResultsDir, type = "source", repos = NULL)
}

# installJaspPkgs <- function(quiet = TRUE) {
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

#' Install a JASP module from jasp-stats
#'
#' Thin wrapper around remotes::install_github.
#'
#' @param pkg Name of the JASP module (e.g., "jaspFrequencies").
#' @param force Boolean. Should the installation overwrite an existing installation if it hasn't changed?
#' @param auth_token To install from a private repo, generate a personal access token (PAT) in "https://github.com/settings/tokens" and supply to this argument. This is safer than using a password because you can easily delete a PAT without affecting any others. Defaults to the GITHUB_PAT environment variable.
#' @param ... Passed on to \code{remotes::install_github}
#'
#' @export installJaspPkg
installJaspPkg <- function(pkg, force = FALSE, auth_token = NULL, ...) {
  if (is.null(auth_token))
    auth_token <- getGithubPAT()

  remotes::install_github(paste("jasp-stats", pkg, sep = "/"), upgrade = "never", force = force, auth_token = auth_token, ...)
}

#' Install all modules and R packages from jasp-stats
#'
#' This function downloads all JASP modules locally and then installs them, to ensure all dependencies between JASP modules are resolved correctly.
#'
#' @param onlyMissingPkgs Boolean. Should JASP reinstall everything or should it only install packages that are not installed on your system?
#' @param quiet Boolean. Should the installation procedure produce output?
#'
#' @export installAllJaspPkgs
installAllJaspPkgs <- function(onlyMissingPkgs = FALSE, quiet = FALSE) {
  if (isJaspRequiredFilesLocationSet()) {
    oldLibPaths <- .libPaths()
    on.exit(.libPaths(oldLibPaths))
    .libPaths(c(oldLibPaths, readJaspRequiredFilesLocation())) # we add it at the end so it doesn't actually write to it
  }

  res <- downloadAllJaspPkgs(onlyMissingPkgs, quiet)
  if (length(res[["success"]]) > 0) {
    numWritten <- tools::write_PACKAGES(getTempJaspPkgsLocation(), type = "source")
    rdsFile <- file.path(getTempJaspPkgsLocation(), "packages.RDS")
    if (numWritten > 0 && file.exists(rdsFile)) {
      localPkgDB <- readRDS(rdsFile)
      installCranDependencies(localPkgDB)
      if (length(localPkgDB[, "Package"]) > 0)
        install.packages(localPkgDB[, "Package"], contriburl = paste0("file:///", getTempJaspPkgsLocation()), type = "source", quiet = quiet)
    }
  }
}

installCranDependencies <- function(localPkgDB) {
  allDeps <- unlist(tools::package_dependencies(localPkgDB[, "Package"], db = localPkgDB))
  internalDeps <- localPkgDB[, "Package"] # these are the JASP modules
  externalDeps <- setdiff(allDeps, internalDeps) # these are CRAN packages
  missingDeps <- setdiff(externalDeps, installed.packages())
  if (length(missingDeps) > 0)
    install.packages(missingDeps)
}

getTempJaspPkgsLocation <- function() {
  file.path(tempdir(), "jaspPkgs")
}

downloadAllJaspPkgs <- function(onlyMissingPkgs = FALSE, quiet = FALSE) {
  repos <- getJaspGithubRepos()
  result <- list(success = NULL, fail = NULL)
  for (repo in repos) {
    if (!is.null(names(repo)) && c("name", "full_name") %in% names(repo)) {
      if (isRepoJaspModule(repo[["name"]]) && (!onlyMissingPkgs || onlyMissingPkgs && !repo[["name"]] %in% installed.packages())) {
        success <- downloadJaspPkg(repo[["name"]], quiet)
        if (success)
          result[["success"]] <- c(result[["success"]], repo[["name"]])
        else
          result[["fail"]] <- c(result[["fail"]], repo[["name"]])
      }
    }
  }
  cat("Successful downloads: ", paste(result[["success"]], collapse = ", "), "\n")
  if (length(result[["fail"]]) > 0)
    cat("The following packages could not be downloaded: ", paste(result[["fail"]], collapse = ", "), "\n")
  invisible(result)
}

downloadJaspPkg <- function(repo, quiet) {
  url <- sprintf("https://github.com/jasp-stats/%1$s/%2$s/%3$s", repo, "archive", "master.zip")
  baseLoc <- getTempJaspPkgsLocation()
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

    if (dir.exists(paste0(pkgLoc, "-master")))
      file.rename(paste0(pkgLoc, "-master"), pkgLoc)

    devtools::build(pkgLoc, clean_doc = FALSE, quiet = quiet)

    if (dir.exists(pkgLoc))
      unlink(pkgLoc, recursive = TRUE)

    if (file.exists(zipFile))
      file.remove(zipFile)
  }

  return(TRUE)
}

isRepoJaspModule <- function(repo) {
  exclude <- c("jaspTools", "TestModule", "ggpol", "callr", "TruncatedSamplingSpeedup")
  isRPackage <- FALSE

  if (!repo %in% exclude) {
    repoTree <- githubGET(asGithubReposUrl("jasp-stats", repo, c("git", "trees", "master"), params = list(recursive = "false")))
    if (length(names(repoTree)) > 0 && "tree" %in% names(repoTree)) {
      pathNames <- unlist(lapply(repoTree[["tree"]], `[[`, "path"))
      if (length(pathNames) > 0 && all(c("NAMESPACE", "DESCRIPTION", "R") %in% pathNames))
        isRPackage <- TRUE
    }
  }

  return(isRPackage)
}

getJaspGithubRepos <- function() {
  githubGET(asGithubOrganizationUrl("jasp-stats", "repos", params = list(type = "public", "per_page" = 1e3)))
}

asGithubOrganizationUrl <- function(owner, urlSegments = NULL, params = list()) {
  asGitHubUrl(sprintf("https://api.github.com/orgs/%s", owner), urlSegments, params)
}

asGithubReposUrl <- function(owner, repo, urlSegments = NULL, params = list()) {
  asGitHubUrl(sprintf("https://api.github.com/repos/%1$s/%2$s", owner, repo), urlSegments, params)
}

asGitHubUrl <- function(url, urlSegments, params) {
  url <- addSegmentsToUrl(url, urlSegments)
  url <- addParamsToUrl(url, params)
  return(url)
}

addSegmentsToUrl <- function(url, segments) {
  if (length(segments) > 0 && is.character(segments))
    url <- paste0(url, "/", paste(segments, collapse = "/"))
  return(url)
}

addParamsToUrl <- function(url, params) {
  if (length(params) > 0 && is.list(params) && !is.null(names(params))) {
    for (i in seq_along(params)) {
      if (i == 1) token <- "?"
      else        token <- "&"
      url <- paste0(url, token, names(params)[i], "=", params[[i]])
    }
  }
  return(url)
}

getGithubPAT <- function() {
  pat <- paste0("5334959d", "c3906be2", "0391aa5d", "cecf1492", "55d38d6f") # default public key
  patEnv <- Sys.getenv("GITHUB_PAT")
  if (nzchar(patEnv))
    pat <- patEnv

  return(pat)
}

githubGET <- function(url) {
  response <- httr::GET(url = url, config = getGithubHeader())

  if (response$status_code == 404)
    stop("Could not locate GitHub repository resource at \"", url, "\" did you specify the owner and repo correctly?")

  if (response$status_code != 200)
    stop("Could not retrieve information from \"", url, "\" at this time")

  suppressMessages(httr::parsed_content(response))
}

getGithubHeader <- function() {
  pat <- getGithubPAT()
  httr::add_headers(Authorization = sprintf("token %s", pat),
                    Accept = "application/vnd.github.golden-comet-preview+json")
}
