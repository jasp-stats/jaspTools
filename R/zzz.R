.pkgenv <- list2env(list(
  internal   = list(jaspToolsPath     = "",
                    dataset           = "",
                    state             = list(),
                    modulePaths       = "",
                    modulesMd5Sums    = list()
               ),
  pkgOptions = list(reinstall.modules = TRUE,
                    install.deps      = TRUE,
                    view.in.rstudio   = TRUE,
                    html.dir          = "",
                    data.dirs         = "",
                    language          = "en"
               )
  ), parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .setInternal("jaspToolsPath", normalizePath(file.path(libname, "jaspTools")))
  .insertRbridgeIntoEnv(.GlobalEnv)

  if (.isSetupComplete()) {
    .initInternalPaths()
    .initOutputDirs()
  }
}

.onAttach <- function(libname, pkgname) {
  if (.isSetupComplete())
    .checkUpdatesJaspCorePkgs()
  else
    packageStartupMessage("jaspTools needs to be setup, so it can find all the resources it needs. Please use `setupJaspTools()` (you don't have to provide args if you're not sure what they mean).")
}

.initInternalPaths <- function() {
  suppressMessages({
    setPkgOption("html.dir",  getJaspDesktopJSLocation())
    setPkgOption("data.dirs", c(getJaspDesktopDatasetLocation(), getJaspToolsDatasetLocation()))
  })
}

.initOutputDirs <- function() {
  htmlDir <- getTempOutputLocation("html")
  if (!dir.exists(htmlDir))
    dir.create(file.path(htmlDir, "plots"), recursive = TRUE)

  stateDir <- getTempOutputLocation("state")
  if (!dir.exists(stateDir))
    dir.create(stateDir, recursive = TRUE)
}

.checkUpdatesJaspCorePkgs <- function() {
  hasUpdates <- NULL
  corePkgs <- c("jaspGraphs", "jaspBase", "jaspTools")
  for (pkg in corePkgs) {
    suppressWarnings(try(silent = TRUE, {
      localVer <- packageVersion(pkg)
      upstreamDescr <- readChar(sprintf("https://raw.githubusercontent.com/jasp-stats/%s/master/DESCRIPTION", pkg), 1e5)
      if (grepl("Version: [0-9\\.]+\\n", upstreamDescr)) {
        upstreamVer <- package_version(stringr::str_match(upstreamDescr, "Version: ([0-9\\.]+)\\n")[2])
        if (upstreamVer > localVer)
          hasUpdates <- c(hasUpdates, pkg)
      }
    }))
  }

  if (length(hasUpdates) > 0)
    packageStartupMessage(
      sprintf("%1$s available for %2$s. It is recommended that you use `remotes::install_github()` to get the latest %3$s.",
              ifelse(length(hasUpdates) == 1, "An update is", "Updates are"),
              paste(hasUpdates, collapse = ", "),
              ifelse(length(hasUpdates) == 1, "version", "versions")))
}
