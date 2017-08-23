.pkgOptions <- list2env(list(
  r.dir = file.path("..", "JASP-Engine", "JASP", "R"),
  html.dir = file.path("..", "JASP-Desktop", "html"),
  json.dir = file.path("..", "Resources", "Library"),
  data.dir = file.path("..", "Resources", "Data Sets"),
  .ppi = 96
))

viewPkgOptions <- function() {
  for (i in 1:length(names(.pkgOptions))) {
    name <- names(.pkgOptions)[i]
    if (i == 1) {
      value <- .getPkgOption(name, run = FALSE)
    } else { # one warning is enough.
      value <- suppressWarnings(.getPkgOption(name, run = FALSE))
    }
    cat(name, "=", value, "\n")
  }
}

setPkgOption <- function(name, value) {
  assign(name, value, envir = .pkgOptions)
}

.getPkgOption <- function(name, run = TRUE) {
  if (.JASPToolsReady() == FALSE) {
    if (run) {
      stop("JASPTools is not configured correctly. Please ensure the paths in viewPkgOptions() are correct.
      If the paths are relative, your working directory must be correctly specified.")
    } else {
      warning("JASPTools is not configured correctly. It will not find the needed resources.
      Please set your working directory to %path%/to%jasp%jasp-desktop/Tools or specify absolute paths to the resources.")
    }
  }
  return(get(name, envir = .pkgOptions))
}

.JASPToolsReady <- function() {
  pathsToSet <- get("pathsToResources", envir = as.environment("package:JASPTools"))
  if (is.null(pathsToSet)) { # paths were initialized previously and emptied
    return(TRUE)
  } else if (is.list(pathsToSet)) { # paths were specified during .onAttach
    .initResourcePaths(pathsToSet)
    return(TRUE)
  } else if (pathsToSet == FALSE && endsWith(getwd(), file.path("jasp-desktop", "Tools"))) { # user manually set wd
    return(TRUE)
  }
  return(FALSE) # paths were not found during initialization
}

.initResourcePaths <- function(paths) {
  for (pathName in names(paths)) {
    setPkgOption(pathName, paths[[pathName]])
  }
  unlockBinding("pathsToResources", env = as.environment("package:JASPTools"))
  assign("pathsToResources", NULL, envir = as.environment("package:JASPTools"))
}

.initRunEnvironment <- function(...) {
  do.call(.setRCPPMasks, list(...))
  .sourceDir(.getPkgOption("r.dir"))
}

.setRCPPMasks <- function(...) {
  setFromRun <- list(...)
  for (mask in .masks) {
    unlockBinding(mask, env = as.environment("package:JASPTools"))
    if (mask %in% names(setFromRun)) {
      value <- setFromRun[[mask]]
    } else {
      value <- .getPkgOption(mask)
    }
    assign(mask, value, envir = as.environment("package:JASPTools"))
  }
}

.sourceDir <- function(path, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    source(file.path(path, nm), ...)
  }
}

.requestTempFileNameNative <- function(...) {
  root <- file.path(tempdir(), "JASPTools", "html")
  list(
    root = root,
    relativePath = paste0(length(list.files(root)), ".png")
  )
}
