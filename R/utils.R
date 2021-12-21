#' @importFrom utils install.packages menu download.file unzip capture.output installed.packages packageVersion
#' @importFrom testthat expect skip

isBinaryPackage <- function(modulePath) {
  # check if a JASP module is a binary package. The main difference is that in an installed binary package module/inst/* is moved to module/*

  dir.exists(file.path(modulePath, "qml")) &&
    dir.exists(file.path(modulePath, "Meta")) &&
    length(list.files(file.path(modulePath, "R"))) == 3L
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

tidyPath <- function(path) {
  gsub("[\\/]$", "", normalizePath(path)) # normalize path and strip trailing slashes
}
