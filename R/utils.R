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

.initRunEnvironment <- function(envir, ...) {
  unlockBinding("envir", env = as.environment("package:JASPTools"))
  assign("envir", envir, envir = as.environment("package:JASPTools"))
  .sourceDir(.getPkgOption("r.dir"), envir)
  .setRCPPMasks(...)
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

.sourceDir <- function(path, envir) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    source(file.path(path, nm), local=envir)
  }
}

.requestTempFileNameNative <- function(...) {
  root <- file.path(tempdir(), "JASPTools", "html")
  numPlots <- length(list.files(file.path(root, "plots")))
  list(
    root = root,
    relativePath = file.path("plots", paste0(numPlots + 1, ".png"))
  )
}

.parseUnicode <- function(str) {
  if (! is.character(str) || length(str) == 0)
    stop(paste("Invalid str provided, received", str))

  # used unicode chars in JASP as of 17/10/17.
  # unfortunately I have not found a way to do this more elegantly.
  lookup <- list(
    "\\u002a" = "*",
    "\\u0042" = "B",
    "\\u0046" = "F",
    "\\u00b2" = "²",
    "\\u00f4" = "ô",
    "\\u03a7" = "χ",
    "\\u03b1" = "α",
    "\\u03b5" = "ε",
    "\\u03b7" = "η",
    "\\u03bb" = "λ",
    "\\u03c3" = "σ",
    "\\u03c7" = "χ",
    "\\u03c9" = "ω",
    "\\u2009" = "	",
    "\\u2013" = "–",
    "\\u2014" = "—",
    "\\u2019" = "’",
    "\\u207a" = "⁺",
    "\\u207b" = "⁻",
    "\\u2080" = "₀",
    "\\u2081" = "₁",
    "\\u2082" = "₂",
    "\\u208a" = "₊",
    "\\u208b" = "₋",
    "\\u209a" = "ᵨ", # close enough
    "\\u221e" = "∞",
    "\\u2260" = "≠",
    "\\u2264" = "≤",
    "\\u273b" = "✻"
  )

  for (unicode in names(lookup)) {
    str <- gsub(unicode, lookup[[unicode]], str, ignore.case=TRUE)
  }

  return(str)
}

.restoreOptions <- function(opts) {
  options(opts) # overwrite changed options
  addedOpts <- setdiff(names(options()), names(opts))
  options(Map(function(x) NULL, addedOpts)) # remove added options
}
