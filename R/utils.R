findCorrectFunction <- function(name) {
  modulePath <- getModulePathFromRFunction(name)
  if (is.null(modulePath))
    stop("Could not locate the module location for ", name)

  return(paste(getModuleNameFromPath(modulePath), name, sep = "::"))
}

getModulePaths <- function() {
  modulePaths <- getPkgOption("module.dirs")

  if (length(modulePaths) == 0)
    stop("No module folders were specified through `setPkgOption(\"module.dirs\", ...)`. Please add the ones you are working on and want to run or test.")

  if (all(is.character(modulePaths)))
    modulePaths <- normalizePath(modulePaths)

  validModulePaths <- NULL
  for (modulePath in modulePaths) {
    rPkg <- try(devtools::as.package(modulePath), silent = TRUE)
    if (!inherits(rPkg, "try-error") && is.list(rPkg) && "path" %in% names(rPkg))
      validModulePaths <- c(validModulePaths, rPkg[["path"]])
  }

  if (length(validModulePaths) == 0)
    stop("None of the module folders specified through `setPkgOption(\"module.dirs\", ...)` are valid R packages. All JASP modules should be R packages.")

  return(validModulePaths)
}

getModuleNameFromPath <- function(modulePaths) {
  names <- character(length(modulePaths))
  for (i in seq_along(modulePaths)) {
    descr <- devtools:::load_pkg_description(modulePaths[i], create = FALSE)
    names[i] <- descr[["package"]]
  }
  return(names)
}

getModulePathFromRFunction <- function(name) {
  modulePath <- NULL

  modulePaths <- getModulePaths()
  for (i in seq_along(modulePaths))
    if (rFunctionExistsInModule(name, modulePaths[[i]]))
      modulePath <- modulePaths[i]

  if (is.null(modulePath))
    stop("Could not locate R function ", name, " in any module. Did you specify the R function correctly (it's case sensitive)? Also make sure the `module.dirs` is complete (see `viewPkgOptions()`).")

  return(modulePath)
}

rFunctionExistsInModule <- function(name, modulePath) {
  env <- new.env()
  rFiles <- list.files(file.path(modulePath, "R"), pattern = "\\.[RrSsQq]$", recursive = TRUE, full.names = TRUE)
  if (length(rFiles) == 0)
    return(FALSE)

  for (rFile in rFiles)
    source(rFile, local = env)

  if (name %in% names(env))
    return(TRUE)

  return(FALSE)
}

getModulePathsForTesting <- function(name = NULL) {
  if (!is.null(name))
    modulePaths <- getModulePathFromRFunction(name)
  else
    modulePaths <- getModulePaths()

  modulesWithTests <- NULL
  for (modulePath in modulePaths) {
    testDir <- file.path(modulePath, "tests", "testthat")
    if (dir.exists(testDir) && length(list.files(testDir)) > 0)
      modulesWithTests <- c(modulesWithTests, modulePath)
  }

  if (length(modulesWithTests) == 0)
    stop("None of the module folders specified in `module.dirs` (see `viewPkgOptions()`) have any tests. Note that the tests should be in `moduleDir/tests/testthat` and named `test-analysisName.R`.")

  return(modulesWithTests)
}

insideTestEnvironment <- function() {
  testthat <- vapply(sys.frames(),
    function(frame)
      methods::getPackageName(frame) == "testthat",
    logical(1))
  if (any(testthat)) {
    return(TRUE)
  }
  return(FALSE)
}

replaceFn <- function(fnName, fn, pkgName) {
  reAssign <- function(env) {
    unlockBinding(fnName, env)
    assign(fnName, fn, env)
    lockBinding(fnName, env)
  }

  try(silent=TRUE, {
    reAssign(getNamespace(pkgName)) # if not attached
    reAssign(as.environment(paste0("package:", pkgName))) # if attached
  })
}

getErrorMsgFromLastResults <- function() {
  lastResults <- .getInternal("lastResults")
  if (jsonlite::validate(lastResults))
    lastResults <- jsonlite::fromJSON(lastResults)

  if (is.null(lastResults) || !is.list(lastResults) || is.null(names(lastResults)))
    return(NULL)

  if ((lastResults[["status"]] == "validationError" || lastResults[["status"]] == "fatalError") && is.list(lastResults[["results"]]))
    return(errorMsgFromHtml(lastResults$results$errorMessage))

  return(NULL)
}

errorMsgFromHtml <- function(html) {
  indexStackTrace <- unlist(gregexpr("<div class=stack-trace", html, fixed=TRUE))[1]
  if (indexStackTrace > -1) {
    error <- substr(html, 1, indexStackTrace - 1)
    error <- gsub("<br>", " ", error, fixed=TRUE)
    stackTrace <- stringr::str_match(html, "<div class=stack-trace>(.*)<\\/div>")[1, 2]
    html <- paste0(error, "\n\nStacktrace within JASP:\n----------\n", stackTrace, "\n----------\n")
  }
  parsedMsg <- gsub("<br>", "\n", html, fixed=TRUE)
  return(parsedMsg)
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
