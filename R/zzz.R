.onAttach <- function(libname, pkgname) {
  message(sprintf("jasptools version: %s", packageVersion("jasptools")))

  # attempt to find the JASP install on the disk
  foundJASP <- FALSE
  jasptoolsPath <- file.path(libname, pkgname)
  if (endsWith(jasptoolsPath, file.path("Tools", "jasptools"))) {
    foundJASP <- TRUE
    message("Successfully found the root location of jasptools.")
  } else {
    message(paste(
    "Error: Cannot find the install location of jasptools.
    Location provided:", libname, "
    Did you set the argument lib.loc to %path%/%to%/%jasp%/jasp-desktop/Tools?
    If you did, please execute unloadNamespace('jasptools') and try again.

    If the problem persists you will have to set your working directory to %path%/%to%/%jasp%/jasp-desktop/Tools

    Setup will continue; please follow the steps above to ensure correct functioning of jasptools."
    ))
  }

  pathsToResources <- FALSE
  if (foundJASP) {
    # install dependencies (jasptools comes pre-installed with JASP so install.packages() is never called)
    pkgDescr <- packageDescription("jasptools", lib.loc = libname)
    imports <- gsub("\\s", "", pkgDescr$Imports)
    pkgs <- unlist(strsplit(imports, ",", fixed = TRUE))
    for (pkg in pkgs) {
      if (! pkg %in% installed.packages())
        try(install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = NA), silent = TRUE)
    }

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
    explode <- unlist(strsplit(jasptoolsPath, .Platform$file.sep)) # php habits..
    basePath <- paste(head(explode, length(explode) - 2), collapse = .Platform$file.sep)

    # temporarily change wd
    oldwd <- getwd()
    setwd(basePath)
    on.exit(setwd(oldwd))

    # get the path to JASP R packages so users do not need to install any additional packages
    # retrieving os bit: http://conjugateprior.org/2015/06/identifying-the-os-from-r/
    pathsToPackages <- NULL
    os <- NULL
    if (! is.null(Sys.info())) {
      os <- Sys.info()["sysname"]
      if (os == "Darwin")
        os <- "osx"
    } else {
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }

    if (! is.null(os)) {
      os <- tolower(os)

      if (os == "osx") {

        basePathPackages <- file.path("..", "Frameworks", "R.framework", "Versions")
        rVersions <- list.files(basePathPackages)
        if (! identical(rVersions, character(0))) {
          rVersions <- suppressWarnings(as.numeric(rVersions))
          r <- sort(rVersions, decreasing = TRUE)[1]
          pathsToPackages <- file.path(basePathPackages, r, "Resources", "library")
        }

      } else if (os == "windows") {

        if (.Machine$sizeof.pointer == 8) { # 64 bits
          pathsToPackages <- findDirPackages(file.path(".."), c("jasp", "64"))
        } else { # 32 bits
          pathsToPackages <- findDirPackages(file.path(".."), c("jasp", "32"))
        }

        if (is.null(pathsToPackages)) {
          pathsToPackages <- findDirPackages(file.path(".."), c("build", "jasp"))
        }

      } else if (os == "linux") {

        message("Identified OS as Linux. Assuming R packages required for JASP were installed manually.")

      }

    }

    pathToPackages <- NULL
    if (! is.null(pathsToPackages)) {
      for (path in pathsToPackages) {
        packages <- list.files(path)
        if (! identical(packages, character(0)) && "base" %in% packages) {
          message("Successfully found the bundled R packages.")
          pathToPackages <- path
          break
        }
      }
    }

    if (is.null(pathToPackages) && (is.null(os) || os != "linux")) {
      message("Unable to find the bundled R packages.
      Required packages will have to be installed manually, or 'pkgs.dir' must be set.")
    }

    # set locations of all required resources (json, analyses, html, packages)
    relativePaths <- list(
      r.dir = file.path("JASP-Engine", "JASP", "R"),
      html.dir = file.path("JASP-Desktop", "html"),
      json.dir = file.path("Resources", "Library"),
      data.dir = file.path("Resources", "Data Sets"),
      tests.dir = file.path("JASP-Tests", "R", "tests", "testthat"),
      tests.data.dir = file.path("JASP-Tests", "R", "tests", "datasets")
    )

    if (! is.null(pathToPackages)) {
      relativePaths[["pkgs.dir"]] <- pathToPackages
    }

    absolutePaths <- lapply(relativePaths, normalizePath)
    pathsToResources <- absolutePaths
  }

  # create the temp (html) directory for the output
  pathToTools <- file.path(tempdir(), "jasptools")
  if (! dir.exists(pathToTools)) {
    dir.create(file.path(pathToTools, "html", "plots"), recursive = TRUE)
    dir.create(file.path(pathToTools, "state"))
    message(paste("Note: temp output files may be found at", pathToTools))
  }

  initPaths <- FALSE
  if (! is.null(pathsToResources))
    initPaths <- pathsToResources

  .internal <- list2env(list(
    initPaths=initPaths,
    envir = .GlobalEnv,
    dataset = NULL,
    state = NULL
  ))
  # create globals for setup / JASP to find
  assign(".internal", .internal, envir = as.environment("package:jasptools"))
  assign("perform", NULL, envir = as.environment("package:jasptools"))
  assign(".ppi", NULL, envir = as.environment("package:jasptools"))
  assign(".baseCitation", "x", envir = as.environment("package:jasptools"))
  assign(".masks", c("perform", ".ppi"), envir = as.environment("package:jasptools"))
}
