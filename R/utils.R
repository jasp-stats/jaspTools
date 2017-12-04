.JASPToolsReady <- function() {
  initPaths <- .getInternal("initPaths")
  if (is.list(initPaths)) { # paths specified during .onAttach and still need to be inited
    .initResourcePaths(initPaths)
    return(TRUE)
  } else if (initPaths == TRUE) { # paths were initialized previously
    return(TRUE)
  } else if (endsWith(getwd(), file.path("Tools"))) { # user manually set wd
    return(TRUE)
  }
  return(FALSE) # paths were not found during initialization
}

.initResourcePaths <- function(paths) {
  for (pathName in names(paths)) {
    setPkgOption(pathName, paths[[pathName]])
  }
  .setInternal("initPaths", TRUE)
}

.initRunEnvironment <- function(envir, dataset, ...) {
  .setInternal("envir", envir)
  .setInternal("dataset", dataset)
  .libPaths(c(.getPkgOption("pkgs.dir"), .libPaths()))
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

.getJSON <- function(analysis, ...) {
  file <- file.path(.getPkgOption("json.dir"), paste0(analysis, ".json"))
  analysisJSON <- try(readLines(file), silent=TRUE)
  if (inherits(analysisJSON, "try-error")) {
    stop("The JSON file for the analysis you supplied could not be found.
         Please ensure that (1) its name matches the main R function
         and (2) your working directory is set properly.")
  }

  args <- list(...)
  if (length(args) == 0)
   return(analysisJSON)

  result <- list()
  optsList <- jsonlite::read_json(file)
  for (arg in args) {
    keys <- unlist(strsplit(arg, "=>", fixed=TRUE))
    value <- optsList
    for (key in keys) {
      value <- value[[key]]
    }
    if (is.null(value))
      result[[key]] <- "null"
    else
      result[[key]] <- jsonlite::toJSON(value)
  }
  return(result)
}

.parseUnicode <- function(str) {
  if (! is.character(str) || length(str) == 0)
    stop(paste("Invalid str provided, received", str))

  # used unicode chars in JASP as of 3/11/17.
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
  if (length(addedOpts) > 0) {
    options(Map(function(x) NULL, addedOpts)) # remove added options
  }
}

.restoreNamespaces <- function(nms) {
  addedNamespaces <- setdiff(loadedNamespaces(), nms)
  if (length(addedNamespaces) > 0) {
    addedNamespaces <- rev(addedNamespaces) # assuming new pkgs (i.e. dependents) get added later
    for (namespace in addedNamespaces) {
      try(unloadNamespace(namespace), silent=TRUE) # this will fail if the pkg is a dependent
    }
  }
}

# not used. Could possibly make pkg unloading more targeted, but does not include pkgs used in other (linked) analyses
.getAnalysisPkgs <- function(analysis, base=FALSE) {
  analysis <- .validateAnalysis(analysis)
  file <- file.path(.getPkgOption("r.dir"), paste0(analysis, ".R"))
  content <- suppressWarnings(readLines(file))
  content <- gsub('#.*', "", content) # remove comments
  matches <- stringr::str_match_all(content, '([a-zA-Z0-9.]{2,}(?<![.]))(?:::|:::)[a-zA-Z0-9._]+')
  analysisPkgs <- unique(unlist(lapply(matches, function(match) match[, 2])))

  if (! base) {
    basePkgs <- installed.packages(priority="high")
    basePkgs <- basePkgs[basePkgs[, "Priority"] == "base", 1]
    if (length(analysisPkgs) > 0)
      analysisPkgs <- analysisPkgs[! analysisPkgs %in% basePkgs]
  }

  if (length(analysisPkgs) > 0)
    return(analysisPkgs)
  return(NULL)
}

.charVec2MixedList <- function(x) {
  x <- stringi::stri_escape_unicode(x)
  x <- gsub("\\\\u.{4}", "<unicode>", x)
  x <- stringi::stri_unescape_unicode(x)
  lapply(x, function(element) {
    res <- element
    if (is.character(element)) {
      num <- suppressWarnings(as.numeric(element))
      if (! is.na(num)) {
        res <- num
      }
    }
    return(res)
  })
}

collapseTable <- function(rows) {
  if (! is.list(rows) || length(rows) == 0) {
    stop("expecting input to be a list with table rows")
  }

  x <- unname(unlist(rows))
  x <- .charVec2MixedList(x)

  return(x)
}

.validateAnalysis <- function(analysis) {
  if (! is.character(analysis) || length(analysis) != 1) {
    stop("expecting non-vectorized character input")
  }

  analysis <- tolower(analysis)
  analyses <- list.files(.getPkgOption("r.dir"), pattern = "\\.[RrSsQq]$")
  analyses <- gsub("\\.[RrSsQq]$", "", analyses)
  if (! analysis %in% analyses) {
    stop("Could not find the analysis. Please ensure that its name matches the main R function.")
  }

  return(analysis)
}
