#' Run the R syntax of an analysis
#'
#' Runs a call of the R wrapper of an analysis (the R syntax JASP shows for it, e.g.
#' `jaspTTests::TTestOneSample(data = NULL, version = "0.96.5", dependent = ~ contNormal)`) and
#' gives its results as [runAnalysis()] gives them.
#'
#' @param analysis The call of the R wrapper of an analysis. It is run by `runAnalysisSyntax()`,
#'   after the data and the modules are ready, so write the call itself, not its result.
#' @param jaspFile Optional path to a .jasp file. Its data is loaded as JASP opens it (with the
#'   types, labels and order of the levels of its columns), for a call with `data = NULL`.
#'   Without `jaspFile`, the call gives its data itself (its `data` argument).
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the analysis.
#'
#' @details The R wrapper runs the analysis with `jaspBase::runWrappedAnalysis()`: its QML form
#' checks the options, and upgrades them when the `version` of the call is older than the module.
#' jaspTools first reinstalls the modules of `module.dirs` that changed.
#'
#' No seed is set before the analysis runs: call `set.seed()` first when its results depend on
#' random numbers.
#'
#' [makeTestsFromExamples()] writes tests that run the analyses of a .jasp file this way.
#'
#' @return The results, as [runAnalysis()] gives them.
#' @examples
#' \dontrun{
#' set.seed(1)
#' results <- runAnalysisSyntax(
#'   jaspTTests::TTestIndependentSamples(
#'     data = NULL,
#'     version = "0.96.5",
#'     formula = ~ mean_NEO,
#'     group = ~ Rotation),
#'   jaspFile = "tests/testthat/jaspfiles/library/Kitchen Rolls.jasp")
#'
#' # the wrapper loads a data.frame given as its data
#' results <- runAnalysisSyntax(jaspTTests::TTestOneSample(data = mtcars, dependent = ~ mpg))
#' }
#' @export
runAnalysisSyntax <- function(analysis, jaspFile = NULL, view = TRUE, quiet = FALSE) {
  if (!is.null(jaspFile))
    jaspFile <- normalizePath(jaspFile, winslash = "/", mustWork = TRUE)

  display <- runDisplay(view, quiet)
  restoreRunState <- saveRunState()
  on.exit(restoreRunState(), add = TRUE)

  # Before `analysis` is evaluated: the modules of module.dirs that changed are reinstalled, and the data of the
  # .jasp file is loaded (or the data of an earlier run cleared, for a call that gives its own data).
  initAnalysisRuntime(dataset = NULL, options = NULL, makeTests = FALSE, jaspFile = jaspFile, reinstall = hasModuleDirs())

  returnVal <- runQuietly(function() analysis, list(), display$quiet)

  if (!inherits(returnVal, "jaspResultsR"))
    stop("`analysis` must be a call of the R wrapper of an analysis, which gives the results of jaspBase::runWrappedAnalysis()",
         call. = FALSE)

  # jaspBase before 0.20.6 gives the results with the encoded column names
  invisible(collectRunResults(returnVal, display$view, decode = !jaspBaseRunsWrappedAnalyses()))
}

# The R syntax of analysis `index` of a .jasp file (the call of its R wrapper, as JASP shows it), with the options
# saved in the file upgraded to the version of the installed module. NULL when the module has no R wrapper for it.
# The form drops the variables that are not in the data, so the data of the file is loaded first.
# Only the generator needs jaspSyntax 1.4.7 (analysisSyntax), so jaspTools does not require it: the module tests may run
# with an older jaspSyntax.
jaspFileAnalysisSyntax <- function(jaspFile, index) {
  if (utils::packageVersion("jaspSyntax") < "1.4.7")
    stop("the R syntax of an analysis needs jaspSyntax 1.4.7 or later (installed: ", utils::packageVersion("jaspSyntax"), ")", call. = FALSE)

  jaspFile <- normalizePath(jaspFile, winslash = "/", mustWork = TRUE)
  record   <- selectJaspFileAnalysis(jaspSyntax::readAnalysisOptionsFromJaspFile(jaspFile, includeMeta = FALSE, isolated = FALSE), index)

  moduleName <- record$moduleName
  if (is.null(moduleName) || !nzchar(moduleName) || !nzchar(system.file(package = moduleName)))
    return(NULL)

  if (!record$name %in% getNamespaceExports(moduleName))
    return(NULL)

  info    <- jaspSyntax::resolveAnalysisQml(find.package(moduleName), record$name)
  version <- if (is.null(record$moduleVersion) || !nzchar(record$moduleVersion)) info$version else record$moduleVersion

  jaspSyntax::clearDatasetState()
  on.exit(jaspSyntax::clearDatasetState(), add = TRUE)
  invisible(jaspSyntax::loadDataSetFromJaspFile(jaspFile))

  jaspSyntax::analysisSyntax(info$qmlFile, record$options, moduleName = moduleName, analysisName = record$name,
                             version = version, preloadData = info$preloadData)
}

# Runs `syntax` (from jaspFileAnalysisSyntax) on the data of `jaspFile`, as the test generated from it does.
runJaspFileAnalysisSyntax <- function(syntax, jaspFile) {
  call <- str2lang(syntax)
  eval(bquote(runAnalysisSyntax(.(call), jaspFile = .(jaspFile), view = FALSE, quiet = TRUE)))
}
