#' @importFrom utils install.packages menu download.file unzip capture.output installed.packages packageVersion
#' @importFrom testthat expect skip

findCorrectFunction <- function(funName) {
  modulePath <- getModulePathFromRFunction(funName)
  if (is.null(modulePath))
    stop("Could not locate the module location for `", funName, "`")
  
  if (!endsWith(funName, "Internal")) {
    funNameInternal <- paste0(funName, "Internal")
    if (rFunctionExistsInModule(funNameInternal, modulePath))
      funName <- funNameInternal
  }
  
  return(paste(getModuleName(modulePath), funName, sep = "::"))
}

getModulePaths <- function() {
  validModules <- NULL

  modulePaths <- getPkgOption("module.dirs")
  if (length(modulePaths) > 0 && any(modulePaths != "")) {

    for (modulePath in modulePaths) {
      validModuleRoot <- getValidModuleRoot(modulePath)
      if (!is.null(validModuleRoot))
        validModules <- c(validModules, validModuleRoot)
    }

  } else {

    wdAsValidModule <- getValidModuleRoot(getwd())
    if (!is.null(wdAsValidModule)) {
      message("Current working directory is a JASP module, using that because `module.dirs` is empty.")
      setPkgOption("module.dirs", wdAsValidModule)
      validModules <- wdAsValidModule
    } else {
      stop("jaspTools needs to know what module to obtain resources from. Please set the current working directory to your JASP module, or specify it through `setPkgOption(\"module.dirs\", \"path/to/module\")`")
    }

  }

  if (length(validModules) == 0)
    stop("None of the modules specified through `setPkgOption(\"module.dirs\", ...)` are valid JASP modules. All JASP modules should be valid R packages and have these files: DESCRIPTION, NAMESPACE and inst/Description.qml.")

  return(validModules)
}

getModulePathFromRFunction <- function(funName) {
  modulePath <- NULL

  modulePaths <- getModulePaths()
  for (i in seq_along(modulePaths))
    if (rFunctionExistsInModule(funName, modulePaths[[i]]))
      modulePath <- modulePaths[i]

  if (is.null(modulePath))
    stop("Could not locate R function `", funName, "` in any module. Did you specify the R function correctly (it's case sensitive)? Also make sure the `module.dirs` is complete (see `viewPkgOptions()`).")

  return(modulePath)
}

isBinaryPackage <- function(modulePath) {
  # check if a JASP module is a binary package. The main difference is that in an installed binary package module/inst/* is moved to module/*

  dir.exists(file.path(modulePath, "qml")) &&
    dir.exists(file.path(modulePath, "Meta")) &&
    length(list.files(file.path(modulePath, "R"))) == 3L
}

rFunctionExistsInModule <- function(funName, modulePath) {

  if (isBinaryPackage(modulePath)) {

    # this is how `::` looks up functions
    moduleName <- getModuleName(modulePath)
    ns <- asNamespace(moduleName)
    return(!is.null(.getNamespaceInfo(ns, "exports")[[funName]]))

  } else {

    env <- new.env()
    rFiles <- list.files(file.path(modulePath, "R"), pattern = "\\.[RrSsQq]$", recursive = TRUE, full.names = TRUE)
    if (length(rFiles) == 0)
      return(FALSE)

    for (rFile in rFiles)
      source(rFile, local = env)

    if (funName %in% names(env))
      return(TRUE)

    return(FALSE)
  }
}

getModulePathsForTesting <- function() {
  modulesWithTests <- NULL
  modulePaths <- getModulePaths()
  for (modulePath in modulePaths) {
    testDir <- file.path(modulePath, "tests", "testthat")
    if (dir.exists(testDir) && length(list.files(testDir)) > 0)
      modulesWithTests <- c(modulesWithTests, modulePath)
  }

  if (length(modulesWithTests) == 0)
    message("No tests were found. Note that the tests should be in `moduleDir/tests/testthat` and named `test-analysisName.R`.")

  return(modulesWithTests)
}

getModuleName <- function(moduleRoot) {
  descrFile <- file.path(moduleRoot, "DESCRIPTION")
  pkgName <- as.vector(read.dcf(descrFile, fields = "Package"))
  if (is.na(pkgName))
    stop("Could not obtain package name from `Package` field in ", descrFile)

  return(pkgName)
}

getValidModuleRoot <- function(path) {
  while (!hasJaspModuleRequisites(path)) {
    parentDir <- dirname(path)
    if (identical(parentDir, dirname(parentDir))) # we're at the root of the filesystem
      return(NULL)
    path <- parentDir
  }
  return(path)
}

moduleRequisites <- function(sep = .Platform$file.sep) {
  return(c("NAMESPACE", "DESCRIPTION", paste("inst", "Description.qml", sep = sep)))
}

binaryModuleRequisites <- function() {
  return(c("NAMESPACE", "DESCRIPTION", "Description.qml", "qml", "Meta"))
}

hasJaspModuleRequisites <- function(path) {
  all(file.exists(file.path(path, moduleRequisites()))) ||
    all(file.exists(file.path(path, binaryModuleRequisites())))
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

getTempOutputLocation <- function(dir = NULL) {
  loc <- file.path(tempdir(), "jaspTools")
  if (!is.null(dir)) {
    if (!dir %in% c("state", "html"))
      stop("Unknown output directory requested ", dir)

    loc <- file.path(loc, dir)
  }
  return(loc)
}

getErrorMsgFromLastResults <- function() {
  error <- list(type = NULL, message = NULL)

  lastResults <- .getInternal("lastResults")
  if (jsonlite::validate(lastResults))
    lastResults <- jsonlite::fromJSON(lastResults)

  if (is.null(lastResults) || !is.list(lastResults) || is.null(names(lastResults)))
    return(NULL)

  if ((lastResults[["status"]] == "validationError" || lastResults[["status"]] == "fatalError") && is.list(lastResults[["results"]])) {
    error[["type"]] <- lastResults[["status"]]
    error[["message"]] <- errorMsgFromHtml(lastResults$results$errorMessage)
  }

  if (is.null(error[["type"]])) {
    flatResults <- unlist(lastResults)
    localErrors <- endsWith(names(flatResults), ".error.errorMessage")
    if (sum(localErrors) > 0) {
      posInResults <- gsub(".error.errorMessage", "", names(flatResults)[localErrors], fixed = TRUE)
      msgs <- flatResults[localErrors]
      error[["type"]] <- "localError"
      error[["message"]] <- paste(posInResults, "-->", msgs, collapse = "\n")
    }
  }

  return(error)
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
  githubGET(asGithubOrganizationUrl("jasp-stats", "repos", params = list(type = "public", per_page = 1e3)))
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
