.pkgenv <- list2env(list(
  internal   = list(jaspToolsPath     = "",
                    dataset           = "",
                    state             = list(),
                    modulesMd5Sums    = list()
               ),
  pkgOptions = list(module.dirs       = "",
                    reinstall.modules = TRUE,
                    view.in.rstudio   = TRUE,
                    html.dir          = "",
                    data.dirs         = "",
                    pkgs.dir          = "",
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
  if (!.isSetupComplete())
    packageStartupMessage("jaspTools needs to be setup, so it can find all the resources it needs. Please use `setupJaspTools()` (you don't have to provide args if you're not sure what they mean).")
}

.initInternalPaths <- function() {
  setPkgOption("html.dir",  getJavascriptLocation())
  setPkgOption("data.dirs", getDatasetsLocations())
  setPkgOption("pkgs.dir",  readJaspRequiredFilesLocation())
}

.initOutputDirs <- function() {
  htmlDir <- getTempOutputLocation("html")
  if (!dir.exists(htmlDir))
    dir.create(file.path(htmlDir, "plots"), recursive = TRUE)

  stateDir <- getTempOutputLocation("state")
  if (!dir.exists(stateDir))
    dir.create(stateDir, recursive = TRUE)
}
