# externally accessible options
.pkgOptions <- list2env(list(
  r.dir = file.path("..", "JASP-Engine", "JASP", "R"),
  html.dir = file.path("..", "JASP-Desktop", "html"),
  json.dir = file.path("..", "Resources", "Library"),
  data.dir = file.path("..", "Resources", "Data Sets"),
  pkgs.dir = "",
  tests.dir = file.path("..", "JASP-Tests", "R", "tests", "testthat"),
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
              Please set your working directory to %path%/to%jasp%jasp-desktop/Tools.")
    }
    }
  return(get(name, envir = .pkgOptions))
}

# internally accessible options
.setInternal <- function(name, value) {
  .internal <- get(".internal", envir = as.environment("package:JASPTools"))
  .internal[[name]] <- value
}

.getInternal <- function(name) {
  .internal <- get(".internal", envir = as.environment("package:JASPTools"))
  if (! name %in% names(.internal))
    stop(paste("Could not locate internal variable", name))
  return(.internal[[name]])
}

.resetInternals <- function() {
  .setInternal("state", NULL)
  .setInternal("dataset", NULL)
}
