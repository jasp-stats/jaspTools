findCorrectFunction <- function(name) {
  modulePath <- getModulePathFromRFunction(name)
  if (is.null(modulePath))
    stop("Could not locate the module location for ", name)

  return(paste(getModuleNameFromPath(modulePath), name, sep = "::"))
}


fetchRunArgs <- function(name, options) {
  possibleArgs <- list(
    name = name,
    functionCall = findCorrectFunction(name),
    title = "",
    requiresInit = TRUE,
    options = jsonlite::toJSON(options),
    dataKey = "null",
    resultsMeta = "null",
    stateKey = "null"
  )

  runArgs <- formals(jaspBase::runJaspResults)
  argNames <- intersect(names(possibleArgs), names(runArgs))
  return(possibleArgs[argNames])
}


initAnalysisRuntime <- function(dataset, makeTests, ...) {
  # source all the R analysis files
  reinstallChangedModules()
  .setInternal("dataset", dataset) # dataset to be found later when it needs to be read
  .libPaths(c(getPkgOption("pkgs.dir"), .libPaths())) # location of JASP's R packages
  refreshRCPPMasks(...) # set the rbridge globals to the value run is called with, or take it from .pkgOptions (for now only .ppi)
  localeRes <- suppressWarnings(Sys.setlocale(category = "LC_ALL", locale = getPkgOption("locale"))) # ensure it defaults to English unless specified otherwise
  initializeCoreJaspPackages()
  if (makeTests)
    set.seed(1)
}

refreshRCPPMasks <- function(...) {
  setFromRun <- list(...)
  for (mask in .masks) { # .masks is a global
    unlockBinding(mask, env = as.environment("package:jaspTools"))
    if (mask %in% names(setFromRun)) {
      value <- setFromRun[[mask]]
    } else {
      value <- getPkgOption(mask)
    }
    assign(mask, value, envir = as.environment("package:jaspTools"))
  }
}

getModulePaths <- function() {
  modulePaths <- getPkgOption("module.dirs")

  if (length(modulePaths) == 0)
    stop("No module folders were specified through `setPkgOption(\"module.dirs\", ...)`. Please add the ones you are working on and want to run or test.")

  if (all(is.character(modulePaths)))
    modulePaths <- normalizePath(modulePaths)

  validModulePaths <- NULL
  for (modulePath in modulePaths) {
    rPkg <- try(devtools::as.package(modulePath), silent = TRUE)
    if (!inherits(rPkg, "try-error") && is.list(rPkg) && "path" %in% names(rPkg))
      validModulePaths <- c(validModulePaths, rPkg[["path"]])
  }

  if (length(validModulePaths) == 0)
    stop("None of the module folders specified through `setPkgOption(\"module.dirs\", ...)` are valid R packages. All JASP modules should be R packages.")

  return(validModulePaths)
}

getModuleNameFromPath <- function(modulePaths) {
  names <- character(length(modulePaths))
  for (i in seq_along(modulePaths)) {
    descr <- devtools:::load_pkg_description(modulePaths[i], create = FALSE)
    names[i] <- descr[["package"]]
  }
  return(names)
}

getModulePathFromRFunction <- function(name) {
  modulePath <- NULL

  modulePaths <- getModulePaths()
  for (i in seq_along(modulePaths))
    if (rFunctionExistsInModule(name, modulePaths[[i]]))
      modulePath <- modulePaths[i]

  if (is.null(modulePath))
    stop("Could not locate R function ", name, " in any module. Did you specify the R function correctly (it's case sensitive)? Also make sure the `module.dirs` is complete (see `viewPkgOptions()`).")

  return(modulePath)
}

rFunctionExistsInModule <- function(name, modulePath) {
  env <- new.env()
  rFiles <- list.files(file.path(modulePath, "R"), pattern = "\\.[RrSsQq]$", recursive = TRUE, full.names = TRUE)
  if (length(rFiles) == 0)
    return(FALSE)

  for (rFile in rFiles)
    source(rFile, local = env)

  if (name %in% names(env))
    return(TRUE)

  return(FALSE)
}


reinstallChangedModules <- function() {
  if (!"jaspBase" %in% installed.packages())
    stop("Cannot find the basic module `jaspBase`; please run `installJaspPkg(\"jaspBase\")`")

  modules   <- getModulePaths()
  reinstall <- getPkgOption("reinstall.modules")
  if (!reinstall || length(modules) == 0)
    return()

  md5Sums <- .getInternal("modulesMd5Sums")
  for (module in modules) {
    files <- list.files(module, include.dirs = FALSE, full.names = TRUE, recursive = TRUE, pattern="(NAMESPACE$)|(DESCRIPTION$)|(\\.R$)")
    if (length(files) > 0) {
      newMd5Sums <- tools::md5sum(files)
      if (length(md5Sums) == 0 || !module %in% names(md5Sums) || !all(newMd5Sums %in% md5Sums[[module]])) {
        remotes::install_local(module, force = TRUE, upgrade = "never", quiet = TRUE)
        devtools::reload(module)
        md5Sums[[module]] <- newMd5Sums
      }
    }
  }

  .setInternal("modulesMd5Sums", md5Sums)
}

initializeCoreJaspPackages <- function() {
  require(jaspResults)
  require(jaspBase)
  jaspResults::initJaspResults()

  assign("jaspResultsModule", list(create_cpp_jaspResults = function(name, state) get("jaspResults", envir = .GlobalEnv)$.__enclos_env__$private$jaspObject), envir = .GlobalEnv)
}

convertResultsListToJson <- function(lst) {
  json <- try(jsonlite::toJSON(lst, null="null", auto_unbox=TRUE, digits=NA))
  if (inherits(json, "try-error"))
    json <- paste0("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"Unable to jsonify\" } }")

  json <- parseUnicode(json)
  json <- gsub("<div class=stack-trace>", "<div>", json, fixed=TRUE) # this makes sure the stacktrace is not hidden
  json <- gsub("\\\"", "\\\\\"", json, fixed=TRUE) # double escape all escaped quotes (otherwise the printed json is invalid)

  return(json)
}

insertJsonInHtml <- function(json, htmlFile) {
  html <- readChar(file.path(getPkgOption("html.dir"), "index.html"), 1000000)
  insertedJS <- paste0(
    "<script>
      var jasp = {}
      jQuery(function($) {
        $(document).ready(function() {
          window.analysisChanged(", json, ")
        })
      })
    </script></body>")
  html <- gsub("</body>", insertedJS, html)
  html <- changeJsIncludeForAdblockers(html)

  writeChar(html, htmlFile)
}

initializeOutputFolder <- function(folder) {
  if (!dir.exists(folder))
    dir.create(folder, recursive=TRUE)

  if (! "js" %in% list.dirs(folder, full.names=FALSE))
    file.copy(list.files(getPkgOption("html.dir"), full.names = TRUE), folder, recursive = TRUE)

  renameJsFileForAdblockers(folder)
}

changeJsIncludeForAdblockers <- function(html) {
  gsub("analysis.js", "jaspanalysis.js", html, fixed = TRUE)
}

renameJsFileForAdblockers <- function(folder) {
  if (file.exists(file.path(folder, "js", "analysis.js")))
    file.rename(file.path(folder, "js", "analysis.js"), file.path(folder, "js", "jaspanalysis.js"))
}

getJsonResultsFromJaspResults <- function() {
  return(jaspResults$.__enclos_env__$private$getResults())
}

processJsonResults <- function(jsonResults) {
  if (jsonlite::validate(jsonResults))
    results <- jsonlite::fromJSON(jsonResults, simplifyVector=FALSE)
  else
    stop("Could not process json result from jaspResults")

  results[["state"]] <- .getInternal("state")

  figures <- results$state$figures
  if (length(figures) > 1 && !is.null(names(figures)))
    results$state$figures <- figures[order(as.numeric(tools::file_path_sans_ext(basename(names(figures)))))]

  return(results)
}

transferPlotsFromjaspResults <- function() {
  pathPlotsjaspResults <- file.path(tempdir(), "jaspResults", "plots")
  pathPlotsjaspTools <- file.path(tempdir(), "jaspTools", "html")
  if (dir.exists(pathPlotsjaspResults)) {
    plots <- list.files(pathPlotsjaspResults)
    if (length(plots) > 0) {
      file.copy(file.path(pathPlotsjaspResults, plots), pathPlotsjaspTools, overwrite=TRUE)
    }
  }
}

parseUnicode <- function(str) {
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

getInstallLocationDep <- function(dep) {
  pkgs <- installed.packages()
  index <- min(which(row.names(pkgs) == dep))
  return(pkgs[index, "LibPath"])
}

charVec2MixedList <- function(x) {
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

#
collapseTestTable <- function(rows) {
  if (! is.list(rows) || length(rows) == 0)
    stop("expecting input to be a list (with a list for each JASP table row)")

  x <- unname(unlist(rows))
  x <- charVec2MixedList(x)

  return(x)
}

insideTestEnvironment <- function() {
  testthat <- vapply(sys.frames(),
    function(frame)
      methods::getPackageName(frame) == "testthat",
    logical(1))
  if (any(testthat)) {
    return(TRUE)
  }
  return(FALSE)
}

replaceFn <- function(fnName, fn, pkgName) {
  reAssign <- function(env) {
    unlockBinding(fnName, env)
    assign(fnName, fn, env)
    lockBinding(fnName, env)
  }

  try(silent=TRUE, {
    reAssign(getNamespace(pkgName)) # if not attached
    reAssign(as.environment(paste0("package:", pkgName))) # if attached
  })
}

getErrorMsgFromLastResults <- function() {
  lastResults <- .getInternal("lastResults")
  if (jsonlite::validate(lastResults))
    lastResults <- jsonlite::fromJSON(lastResults)

  if (is.null(lastResults) || !is.list(lastResults) || is.null(names(lastResults)))
    return(NULL)

  if ((lastResults[["status"]] == "validationError" || lastResults[["status"]] == "fatalError") && is.list(lastResults[["results"]]))
    return(errorMsgFromHtml(lastResults$results$errorMessage))

  return(NULL)
}

errorMsgFromHtml <- function(html) {
  indexStackTrace <- unlist(gregexpr("<div class=stack-trace", html, fixed=TRUE))[1]
  if (indexStackTrace > -1) {
    error <- substr(html, 1, indexStackTrace - 1)
    error <- gsub("<br>", " ", error, fixed=TRUE)
    stackTrace <- stringr::str_match(html, "<div class=stack-trace>(.*)<\\/div>")[1, 2]
    html <- paste0(error, "\n\nStacktrace within JASP:\n----------\n", stackTrace, "\n----------\n")
  }
  parsedMsg <- gsub("<br>", "\n", html, fixed=TRUE)
  return(parsedMsg)
}
