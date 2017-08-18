
#' @export
viewPkgOptions <- function() {
  for (i in 1:length(names(.pkgOptions))) {
    name <- names(.pkgOptions)[i]
    value <- .getPkgOption(name)
    cat(name, "=", value, "\n")
  }
}

#' @export
setPkgOption <- function(name, value) {
  assign(name, value, envir = .pkgOptions)
}

.getPkgOption <- function(name) {
  return(get(name, envir = .pkgOptions))
}

.setRCPPMasks <- function(...) {
  setFromRun <- list(...)
  for (mask in .masks) {
    unlockBinding(mask, as.environment("package:JASPTools"))
    if (mask %in% names(setFromRun)) {
      value <- setFromRun[[mask]]
    } else {
      value <- .getPkgOption(mask)
    }
    assign(mask, value, envir=as.environment("package:JASPTools"))
  }
}

.sourceDir <- function(path, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    source(file.path(path, nm), ...)
  }
}

.initEnvironment <- function(...) {
  do.call(.setRCPPMasks, list(...))
  .sourceDir(.getPkgOption("r.dir"))
}

.requestTempFileNameNative <- function(...) {
  root <- paste0(tempdir(), "/html/")
  list(root=root,
       relativePath=paste0(length(list.files(root)), ".png"))
}
