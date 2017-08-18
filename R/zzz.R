.onAttach <- function(libname, pkgname) {

  # set the working directory
  dir1 <- file.path(libname, pkgname, "R") # attempt 1
  dir2 <- getSrcDirectory(function(x) {x}) # attempt 2
  correctDir <- endsWith(c(dir1, dir2), "Tools/JASPTools/R")
  if (any(correctDir)) {
    dirIndex <- min(which(correctDir))
    dir <- c(dir1, dir2)[dirIndex]
    setwd(dir)
    message(paste("Changing working directory to", dir))
  } else {
    message("Cannot set wd to required location. Did you set the argument lib.loc to path/to/jasp/jasp-desktop/Tools?")
  }

  # set the libpath to JASP R packages
  pathToPackages <- NULL
  os <- NULL
  if (! is.null(Sys.info())) {
    os <- Sys.info()['sysname']
    if (os == "Darwin")
      os <- "OSX"
  } else {
    if (grepl("^darwin", R.version$os))
      os <- "OSX"
  }

  if (! is.null(os) && os == "OSX") {

    rVersions <- list.files("../../../../Frameworks/R.framework/Versions/")
    if (! identical(rVersions, character(0))) {
      rVersions <- suppressWarnings(as.numeric(rVersions))
      r <- sort(rVersions, decreasing=TRUE)[1]
      pathToPackages <- paste0("../../../../Frameworks/R.framework/Versions/", r, "/Resources/library/")
    }

  } else if (! is.null(os) && os == "Windows") {

    findDirPackages <- function(path, needle) {
      dirs <- list.files(path)
      locations <- NULL
      if (! identical(dirs, character(0))) {
        for (dirName in dirs) {
          name <- unlist(strsplit(dirName, "[\\W_]", perl=TRUE))
          if (all(needle %in% tolower(name))) {
            location <- paste0(path, dirName, "/R/library")
            locations <- c(locations, location)
          }
        }
      }
      return(locations)
    }

    if (.Machine$sizeof.pointer == 8) { # 64 bits
      pathToPackages <- findDirPackages("../../../../", c("jasp", "64"))
    } else { # 32 bits
      pathToPackages <- findDirPackages("../../../../", c("jasp", "32"))
    }

  }

  libPathSet <- FALSE
  if (! is.null(pathToPackages)) {
    for (path in pathToPackages) {
      packages <- list.files(path)
      if (! identical(packages, character(0)) && "base" %in% packages) {
        message("Redirecting search path to installed JASP packages")
        .libPaths(path)
        libPathSet <- TRUE
        break
      }
    }
  }

  if (! libPathSet) {
    message("Unable to set search path to installed JASP packages. Required packages will have to be installed manually.")
  }

  # create the temp html directory for the output
  if (! dir.exists(paste0(tempdir(), "/html"))) {
    dir.create(paste0(tempdir(), "/html"))
  }

  # create empty globals for JASP to find
  assign("dataset", NULL, envir=as.environment("package:JASPTools"))
  assign("perform", NULL, envir=as.environment("package:JASPTools"))
  assign(".ppi", NULL, envir=as.environment("package:JASPTools"))
  assign(".baseCitation", NULL, envir=as.environment("package:JASPTools"))
  assign(".masks", c("dataset", "perform", ".ppi"), envir=as.environment("package:JASPTools"))

}
