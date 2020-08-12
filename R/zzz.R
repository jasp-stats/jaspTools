.onAttach <- function(libname, pkgname) {
  if (!isSetupComplete()) {
    message("jaspTools needs to be setup, so it can find all the resources it needs. Please use `initJaspToolsInteractive()` (or `initJaspTools(args)` if you know how this works).")
    setupJaspToolsInternals(FALSE)
  } else {
    setupJaspToolsInternals(TRUE)
  }
  invisible(NULL)
}
