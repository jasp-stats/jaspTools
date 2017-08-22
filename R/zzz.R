.onAttach <- function(libname, pkgname) {

  # attempt to find the JASP install on the disk
  foundJASP <- FALSE
  pkgPath <- file.path(libname, pkgname)
  if (endsWith(pkgPath, file.path("Tools", "JASPTools"))) {
    foundJASP <- TRUE
    message("Found the root location of JASPTools.")
  } else {
    message("Cannot find the install location of JASPTools.
    Did you set the argument lib.loc to %path%/%to%/%jasp%/jasp-desktop/Tools?
    To continue working with JASPTools you will have to either:
    (1) set your working directory to %path%/%to%/%jasp%/jasp-desktop/Tools
    or
    (2) set the absolute paths through JASPTools::setPkgOption()")
  }

  pathsToResources <- FALSE
  if (foundJASP) {

    findDirPackages <- function(pathToBuild, needle) {
      dirs <- list.files(pathToBuild)
      locations <- NULL
      if (! identical(dirs, character(0))) {
        for (dirName in dirs) {
          name <- unlist(strsplit(dirName, "[\\W_]", perl = TRUE))
          if (all(needle %in% tolower(name))) {
            location <- file.path(pathToBuild, dirName, "R", "library")
            locations <- c(locations, location)
          }
        }
      }
      return(locations)
    }

    # get location of jasp-desktop
    explode <- unlist(strsplit(pkgPath, .Platform$file.sep)) # php habits..
    basePath <- paste(head(explode, length(explode) - 2), collapse = .Platform$file.sep)

    # temporarily change wd
    oldwd <- getwd()
    setwd(basePath)
    on.exit(setwd(oldwd))

    # get locations of required resources (json, analyses, html)
    relativePaths <- list(
      r.dir = file.path("JASP-Engine", "JASP", "R"),
      html.dir = file.path("JASP-Desktop", "html"),
      json.dir = file.path("Resources", "Library"),
      data.dir = file.path("Resources", "Data Sets")
    )
    absolutePaths <- lapply(relativePaths, normalizePath)
    pathsToResources <- absolutePaths

    # set the libpath to JASP R packages so users do not need to install any additional packages
    pathToPackages <- NULL
    os <- NULL
    if (! is.null(Sys.info())) {
      os <- Sys.info()["sysname"]
      if (os == "Darwin")
        os <- "OSX"
    } else {
      if (grepl("^darwin", R.version$os))
        os <- "OSX"
    }

    if (! is.null(os)) {

      if (os == "OSX") {

        basePathPackages <- file.path("..", "Frameworks", "R.framework", "Versions")
        rVersions <- list.files(basePathPackages)
        if (! identical(rVersions, character(0))) {
          rVersions <- suppressWarnings(as.numeric(rVersions))
          r <- sort(rVersions, decreasing = TRUE)[1]
          pathToPackages <- file.path(basePathPackages, r, "Resources", "library")
        }

      } else if (os == "Windows") {

        if (.Machine$sizeof.pointer == 8) { # 64 bits
          pathToPackages <- findDirPackages(file.path(".."), c("jasp", "64"))
        } else { # 32 bits
          pathToPackages <- findDirPackages(file.path(".."), c("jasp", "32"))
        }

      }

    }

    libPathSet <- FALSE
    if (! is.null(pathToPackages)) {
      for (path in pathToPackages) {
        packages <- list.files(path)
        if (! identical(packages, character(0)) && "base" %in% packages) {
          message("Found the bundled R packages, redirecting the search path.")
          .libPaths(path)
          libPathSet <- TRUE
          break
        }
      }
    }

    if (! libPathSet) {
      message("Unable to find the bundled R packages.
      Required packages will have to be installed manually.")
    }

  }

  # create the temp html directory for the output
  pathToHtml <- file.path(tempdir(), "JASPTools", "html")
  if (! dir.exists(pathToHtml)) {
    dir.create(pathToHtml, recursive = TRUE)
    message(paste("Created temporary html output folder at", pathToHtml))
  }

  # create globals for setup / JASP to find
  assign("pathsToResources", pathsToResources, envir = as.environment("package:JASPTools"))
  assign("dataset", NULL, envir = as.environment("package:JASPTools"))
  assign("perform", NULL, envir = as.environment("package:JASPTools"))
  assign(".ppi", NULL, envir = as.environment("package:JASPTools"))
  assign(".baseCitation", "x", envir = as.environment("package:JASPTools"))
  assign(".masks", c("dataset", "perform", ".ppi"), envir = as.environment("package:JASPTools"))

}
