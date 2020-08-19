.pkgenv <- list2env(
  list(internal = list(jaspToolsPath  = "",
                       dataset        = "",
                       state          = list(),
                       modulesMd5Sums = list()),
       pkgOptions = list()),
  parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkgenv[["internal"]][["jaspToolsPath"]] <- normalizePath(file.path(libname, "jaspTools"))
  .initJaspToolsInternals()
}

.onAttach <- function(libname, pkgname) {
  if (!.isSetupComplete())
    packageStartupMessage("jaspTools needs to be setup, so it can find all the resources it needs. Please use `setupJaspTools()` (you don't have to provide args if you're not sure what they mean).")
}

.initJaspToolsInternals <- function() {
  if (.isSetupComplete()) {
    .pkgenv[["pkgOptions"]] <- list(
      module.dirs       = NULL,
      reinstall.modules = TRUE,
      html.dir          = getJavascriptLocation(),
      data.dirs         = getDatasetsLocations(),
      pkgs.dir          = readJaspRequiredFilesLocation(),
      language          = "en"
    )

    # create the temp (html) directory for the output
    tempOutputDir <- file.path(tempdir(), "jaspTools")
    if (!dir.exists(tempOutputDir)) {
      dir.create(file.path(tempOutputDir, "html", "plots"), recursive = TRUE)
      dir.create(file.path(tempOutputDir, "state"))
      packageStartupMessage(paste("Note: temp output files may be found at", tempOutputDir))
    }
  }

  .insertRbridgeIntoEnv(.GlobalEnv)
}
