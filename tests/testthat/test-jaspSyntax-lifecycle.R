context("jaspSyntax lifecycle")

localJaspToolsBinding <- function(name, value) {
  namespace <- asNamespace("jaspTools")
  oldValue <- get(name, envir = namespace, inherits = FALSE)
  wasLocked <- bindingIsLocked(name, namespace)

  if (wasLocked)
    unlockBinding(name, namespace)
  assign(name, value, envir = namespace)
  if (wasLocked)
    lockBinding(name, namespace)

  function() {
    if (bindingIsLocked(name, namespace))
      unlockBinding(name, namespace)
    assign(name, oldValue, envir = namespace)
    if (wasLocked)
      lockBinding(name, namespace)
  }
}

localJaspToolsBindings <- function(...) {
  replacements <- list(...)
  restores <- Map(localJaspToolsBinding, names(replacements), replacements)

  function() {
    lapply(rev(restores), function(restoreBinding) restoreBinding())
    invisible(NULL)
  }
}

localJaspToolsOptions <- function(values) {
  oldValues <- options(values)

  function() {
    options(oldValues)
    invisible(NULL)
  }
}

localJaspToolsPkgOptions <- function(values) {
  pkgEnv <- get(".pkgenv", envir = asNamespace("jaspTools"), inherits = FALSE)
  oldValues <- pkgEnv[["pkgOptions"]][names(values)]
  pkgEnv[["pkgOptions"]][names(values)] <- values

  function() {
    pkgEnv[["pkgOptions"]][names(oldValues)] <- oldValues
    invisible(NULL)
  }
}

fakeRunWrappedAnalysis <- function(moduleName, analysisName, qmlFileName,
                                   options, version, preloadData,
                                   modulePath = NULL, qmlFile = NULL) {
  list(
    moduleName = moduleName,
    analysisName = analysisName,
    qmlFileName = qmlFileName,
    options = options,
    version = version,
    preloadData = preloadData,
    modulePath = modulePath,
    qmlFile = qmlFile
  )
}

localDescriptivesModuleOptions <- function(modulePath) {
  localJaspToolsPkgOptions(list(
    "module.dirs" = modulePath,
    "reinstall.modules" = FALSE,
    "language" = "en",
    "data.dirs" = character(0),
    "html.dir" = tempdir(),
    "view.in.rstudio" = FALSE
  ))
}

localRealDescriptivesModulePath <- function() {
  candidates <- c(
    Sys.getenv("JASPTOOLS_REAL_DESCRIPTIVES_MODULE", unset = ""),
    file.path(testthat::test_path(), "..", "..", "..", "jaspDescriptives")
  )
  candidates <- candidates[nzchar(candidates)]
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  candidates[dir.exists(candidates)][1L]
}

disableDescriptivesPlotOptions <- function(options) {
  plotFlags <- c(
    "boxPlot",
    "boxPlotBoxPlot",
    "boxPlotColourPalette",
    "boxPlotJitter",
    "boxPlotOutlierLabel",
    "boxPlotViolin",
    "correlationPlots",
    "densityPlot",
    "distributionAndCorrelationPlotDensity",
    "distributionAndCorrelationPlotRugMarks",
    "distributionPlots",
    "dotPlot",
    "heatmapDisplayValue",
    "heatmapLegend",
    "heatmapPlot",
    "intervalPlot",
    "likertPlot",
    "likertPlotAssumeVariablesSameLevel",
    "paretoPlot",
    "paretoPlotRule",
    "paretoPlotTurnXAxisLabels",
    "pieChart",
    "qqPlot",
    "scatterPlot",
    "scatterPlotLegend",
    "scatterPlotRegressionLine",
    "scatterPlotRegressionLineCi",
    "stemAndLeaf"
  )

  for (name in intersect(plotFlags, names(options))) {
    if (is.logical(options[[name]]) && length(options[[name]]) == 1L)
      options[[name]] <- FALSE
  }

  options
}

skip_if_descriptives_qml_has_known_pareto_bug <- function(modulePath) {
  qmlFile <- file.path(modulePath, "inst", "qml", "Descriptives.qml")
  if (!file.exists(qmlFile))
    return(invisible(FALSE))

  qml <- readLines(qmlFile, warn = FALSE)
  testthat::skip_if(
    any(grepl("visible:\\s*paretoAddCountVariable\\.checked", qml)),
    "local jaspDescriptives QML has unresolved paretoAddCountVariable visibility binding"
  )
}

skip_if_no_jaspSyntax_dataset_api <- function() {
  namespace <- asNamespace("jaspSyntax")
  required <- c(
    "loadAnalysisDataset",
    "readLoadedDataset",
    "readRequestedDataset",
    "clearDatasetState"
  )
  missing <- required[!vapply(required, exists, logical(1L), envir = namespace, inherits = FALSE)]
  testthat::skip_if(
    length(missing) > 0L,
    paste("jaspSyntax native dataset API is unavailable:", paste(missing, collapse = ", "))
  )
}

test_that("analysisOptionsFromJASPFile requests saved jaspSyntax option records for runAnalysis", {
  observed <- NULL

  restore <- localJaspToolsBinding(
    ".jaspSyntaxReadAnalysisOptionsNative",
    function(file, modulePath, runtime,
             includeMeta,
             includeTypeOptions,
             isolated) {
      observed <<- list(
        file = file,
        modulePath = modulePath,
        runtime = runtime,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions,
        isolated = isolated
      )
      list(
        list(name = "analysisA", options = list(option = 0L, `option.types` = "scale", `.meta` = list())),
        list(name = "analysisB", options = list(option = 1L, `option.types` = "nominal", `.meta` = list()))
      )
    }
  )
  on.exit(restore(), add = TRUE)

  opts <- jaspTools:::analysisOptionsFromJASPFile("dummy.jasp", modulePath = "C:/fake/module")

  expect_equal(observed$file, "dummy.jasp")
  expect_equal(observed$modulePath, "C:/fake/module")
  expect_false(observed$runtime)
  expect_true(observed$includeMeta)
  expect_true(observed$includeTypeOptions)
  expect_true(observed$isolated)
  expect_length(opts, 2)
  expect_equal(attr(opts[[1]], "analysisName"), "analysisA")
  expect_equal(attr(opts[[2]], "analysisName"), "analysisB")
  expect_equal(attr(opts[[1]], "jaspTools.optionShape"), "saved")
  expect_equal(attr(opts[[1]], "modulePath"), "C:/fake/module")
  expect_equal(attr(opts[[2]], "modulePath"), "C:/fake/module")
  expect_false(jaspTools:::isPreparedOptions(opts[[1]]))
  expect_equal(opts[[1]]$option, 0L)
  expect_equal(opts[[2]]$option, 1L)
  expect_true(".meta" %in% names(opts[[1]]))
})

test_that("analysisRuntimeOptions requests and marks runtime jaspSyntax option records", {
  observed <- NULL

  restore <- localJaspToolsBinding(
    ".jaspSyntaxReadAnalysisOptionsNative",
    function(file, modulePath, runtime,
             includeMeta,
             includeTypeOptions,
             isolated) {
      observed <<- list(
        file = file,
        modulePath = modulePath,
        runtime = runtime,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions,
        isolated = isolated
      )
      list(
        list(name = "analysisA", moduleName = "jaspFake", moduleVersion = "1.2.3",
             options = list(option = 0L, `option.types` = "scale")),
        list(name = "analysisB", moduleName = "jaspFake", moduleVersion = "1.2.3",
             options = list(option = 1L, `option.types` = "nominal"))
      )
    }
  )
  on.exit(restore(), add = TRUE)

  jaspFile <- tempfile(fileext = ".jasp")
  file.create(jaspFile)

  opts <- jaspTools::analysisRuntimeOptions(jaspFile, modulePath = "C:/fake/module")

  expect_equal(observed$file, normalizePath(jaspFile, winslash = "/", mustWork = FALSE))
  expect_equal(observed$modulePath, "C:/fake/module")
  expect_true(observed$runtime)
  expect_false(observed$includeMeta)
  expect_true(observed$includeTypeOptions)
  expect_true(observed$isolated)
  expect_length(opts, 2)
  expect_true(jaspTools:::isPreparedOptions(opts[[1]]))
  expect_true(jaspTools:::isPreparedOptions(opts[[2]]))
  expect_equal(attr(opts[[1]], "analysisName"), "analysisA")
  expect_equal(attr(opts[[1]], "moduleName"), "jaspFake")
  expect_equal(attr(opts[[1]], "moduleVersion"), "1.2.3")
  expect_equal(attr(opts[[1]], "jaspTools.optionShape"), "runtime")
  expect_equal(attr(opts[[1]], "modulePath"), "C:/fake/module")
  expect_false(".meta" %in% names(opts[[1]]))
})

test_that("named modulePath mismatches fail instead of losing provenance", {
  records <- list(
    list(
      name = "analysisA",
      moduleName = "jaspFake",
      options = list(option = TRUE)
    )
  )

  expect_error(
    jaspTools:::.optionsFromJaspRecords(
      records,
      preparedOptions = FALSE,
      optionShape = "saved",
      modulePath = list(jaspOther = "C:/other/module")
    ),
    "do not match",
    fixed = TRUE
  )
})

test_that(".jaspSyntaxReadAnalysisOptionsFromJaspFile uses configured module paths", {
  observed <- NULL

  restore <- list(
    localJaspToolsBinding(".jaspSyntaxRuntimeModulePaths", function() list(jaspFake = "C:/fake/module")),
    localJaspToolsBinding(
      ".jaspSyntaxReadAnalysisOptionsNative",
      function(file, modulePath, runtime,
               includeMeta,
               includeTypeOptions,
               isolated) {
        observed <<- list(
          file = file,
          modulePath = modulePath,
          runtime = runtime,
          includeMeta = includeMeta,
          includeTypeOptions = includeTypeOptions,
          isolated = isolated
        )
        list(list(name = "analysisA", options = structure(list(), analysisName = "analysisA")))
      }
    )
  )
  on.exit(lapply(rev(restore), function(restoreBinding) restoreBinding()), add = TRUE)

  records <- jaspTools:::.jaspSyntaxReadAnalysisOptionsFromJaspFile("dummy.jasp")

  expect_length(records, 1)
  expect_equal(observed$file, "dummy.jasp")
  expect_equal(observed$modulePath, list(jaspFake = "C:/fake/module"))
  expect_false(observed$runtime)
  expect_true(observed$includeMeta)
  expect_true(observed$includeTypeOptions)
  expect_true(observed$isolated)
})

test_that("prepared option state helper only trusts namespaced attributes", {
  opts <- list(option = TRUE)
  attr(opts, "preparedOptions") <- TRUE
  attr(opts, "syntax") <- TRUE
  expect_false(jaspTools:::isPreparedOptions(opts))

  prepared <- jaspTools:::markPreparedOptions(opts)
  expect_true(jaspTools:::isPreparedOptions(prepared))
})

test_that(".jaspSyntaxNamedModulePaths names local module paths for jaspSyntax", {
  moduleDir <- tempfile("jaspFakeModule_")
  dir.create(moduleDir)
  writeLines(
    c(
      "Package: jaspFake",
      "Version: 0.0.1",
      "Title: Fake JASP Module",
      "Description: Fake module for tests.",
      "License: GPL-2"
    ),
    file.path(moduleDir, "DESCRIPTION")
  )

  paths <- jaspTools:::.jaspSyntaxNamedModulePaths(moduleDir)

  expect_equal(names(paths), "jaspFake")
  expect_equal(paths[[1]], normalizePath(moduleDir, winslash = "/", mustWork = FALSE))
})

test_that("preloadDataset delegates raw dataset preparation to jaspSyntax", {
  clearDatasetCalls <- 0L
  observed <- NULL
  requested <- data.frame(left = 1:2)
  resultDecodingDataset <- data.frame(left = factor(c("control", "treatment")))
  columnMapping <- c(JaspColumn_1_Encoded = "left")

  restore <- localJaspToolsBindings(
    .jaspSyntaxClearDatasetState = function(required) {
      expect_true(required)
      clearDatasetCalls <<- clearDatasetCalls + 1L
      invisible(NULL)
    },
    .jaspSyntaxLoadAnalysisDataset = function(dataset, modulePath, analysisName, options) {
      observed <<- list(
        dataset = dataset,
        modulePath = modulePath,
        analysisName = analysisName,
        options = options
      )
      list(
        loadedDataset = dataset,
        requestedDataset = requested,
        resultDecodingDataset = resultDecodingDataset,
        columnMapping = columnMapping
      )
    }
  )
  on.exit(restore(), add = TRUE)

  dataset <- data.frame(left = 1:2, right = c("a", "b"), stringsAsFactors = FALSE)
  options <- list(variables = list(value = "left", types = "scale"))
  jaspTools:::preloadDataset(
    dataset,
    options = options,
    modulePath = "C:/fake/module",
    analysisName = "FakeAnalysis"
  )

  expect_equal(clearDatasetCalls, 1L)
  expect_identical(observed$dataset, dataset)
  expect_identical(observed$modulePath, "C:/fake/module")
  expect_identical(observed$analysisName, "FakeAnalysis")
  expect_identical(observed$options, options)
  expect_equal(jaspTools:::.getInternal("preloadedDataset"), resultDecodingDataset)
  expect_equal(jaspTools:::.getInternal("preloadedColumnMapping"), columnMapping)
})

test_that("preloadDataset clears stale native state for null datasets", {
  clearDatasetCalls <- 0L

  restore <- localJaspToolsBindings(
    .jaspSyntaxClearDatasetState = function(required) {
      expect_true(required)
      clearDatasetCalls <<- clearDatasetCalls + 1L
      invisible(NULL)
    }
  )
  on.exit(restore(), add = TRUE)

  jaspTools:::preloadDataset(NULL, options = list())

  expect_equal(clearDatasetCalls, 1L)
  expect_equal(jaspTools:::.getInternal("preloadedDataset"), data.frame())
  expect_equal(jaspTools:::.getInternal("preloadedColumnMapping"), character(0))
})

test_that("analysis result decoding delegates to jaspSyntax", {
  observed <- NULL
  requestedDataset <- data.frame(encoded = factor("a"))
  columnMapping <- c(JaspColumn_1_Encoded = "encoded")
  oldPreloadedDataset <- tryCatch(
    jaspTools:::.getInternal("preloadedDataset"),
    error = function(e) NULL
  )
  oldPreloadedColumnMapping <- tryCatch(
    jaspTools:::.getInternal("preloadedColumnMapping"),
    error = function(e) character(0)
  )
  jaspTools:::.setInternal("preloadedDataset", requestedDataset)
  jaspTools:::.setInternal("preloadedColumnMapping", columnMapping)
  on.exit(jaspTools:::.setInternal("preloadedDataset", oldPreloadedDataset), add = TRUE)
  on.exit(jaspTools:::.setInternal("preloadedColumnMapping", oldPreloadedColumnMapping), add = TRUE)

  restore <- localJaspToolsBindings(
    .jaspSyntaxCall = function(names, args = list(), required = TRUE,
                               feature = "jaspSyntax bridge API",
                               requiredArgs = names(args),
                               requiredArgGroups = list()) {
      observed <<- list(
        names = names,
        args = args,
        required = required,
        feature = feature,
        requiredArgs = requiredArgs,
        requiredArgGroups = requiredArgGroups
      )
      args$results$decoded <- TRUE
      args$results
    }
  )
  on.exit(restore(), add = TRUE)

  results <- list(status = "complete", table = list(column = "JaspColumn_1_Encoded"))
  decoded <- jaspTools:::.jaspSyntaxDecodeAnalysisResults(results)

  expect_true(decoded$decoded)
  expect_equal(observed$names, "decodeAnalysisResults")
  expect_equal(observed$args$results, results)
  expect_identical(observed$args$requestedDataset, requestedDataset)
  expect_identical(observed$args$columnMapping, columnMapping)
  expect_true(observed$required)
  expect_equal(observed$requiredArgs, "results")
})

test_that("analysis result decoding delegates plain results to jaspSyntax", {
  observed <- NULL
  restore <- localJaspToolsBindings(
    .jaspSyntaxCall = function(names, args = list(), required = TRUE,
                               feature = "jaspSyntax bridge API",
                               requiredArgs = names(args),
                               requiredArgGroups = list()) {
      observed <<- list(
        names = names,
        args = args,
        required = required,
        feature = feature,
        requiredArgs = requiredArgs,
        requiredArgGroups = requiredArgGroups
      )
      args$results
    }
  )
  on.exit(restore(), add = TRUE)

  results <- list(status = "complete", table = list(column = "plain"))

  expect_identical(
    jaspTools:::.jaspSyntaxDecodeAnalysisResults(results),
    results
  )
  expect_equal(observed$names, "decodeAnalysisResults")
  expect_equal(observed$args$results, results)
  expect_true(observed$required)
  expect_equal(observed$requiredArgs, "results")
})

test_that("jaspSyntax bridge wrapper rejects missing required arguments", {
  oldLoadAnalysisDataset <- function(dataset) dataset

  expect_error(
    jaspTools:::.callWithSupportedArgs(
      oldLoadAnalysisDataset,
      args = list(
        dataset = data.frame(x = 1),
        modulePath = "C:/fake/module",
        analysisName = "FakeAnalysis",
        options = list(variable = "x")
      ),
      requiredArgs = c("dataset", "modulePath", "analysisName", "options"),
      functionName = "jaspSyntax::loadAnalysisDataset()",
      feature = "native analysis dataset API"
    ),
    "modulePath",
    fixed = TRUE
  )
})

test_that("fetchRunArgs targets jaspBase wrapped native QML path", {
  restore <- localJaspToolsBindings(
    .jaspBaseRunWrappedAnalysis = function() fakeRunWrappedAnalysis,
    getModulePathFromRFunction = function(funName) {
      expect_equal(funName, "WrappedAnalysis")
      "C:/fake/module"
    },
    .jaspSyntaxResolveAnalysisQml = function(modulePath, analysisName) {
      expect_equal(modulePath, "C:/fake/module")
      expect_equal(analysisName, "WrappedAnalysis")
      list(
        moduleName = "jaspFake",
        analysisName = "WrappedAnalysis",
        qmlFileName = "WrappedAnalysis.qml",
        qmlFile = "C:/fake/module/inst/qml/WrappedAnalysis.qml",
        version = "1.0.0",
        preloadData = TRUE
      )
    }
  )
  on.exit(restore(), add = TRUE)

  opts <- list(flag = TRUE)
  args <- jaspTools:::fetchRunArgs("WrappedAnalysis", opts)

  expect_identical(attr(args, "runner"), fakeRunWrappedAnalysis)
  expect_equal(attr(args, "modulePath", exact = TRUE), "C:/fake/module")
  attr(args, "runner") <- NULL
  attr(args, "modulePath") <- NULL
  expect_equal(args$moduleName, "jaspFake")
  expect_equal(args$analysisName, "WrappedAnalysis")
  expect_equal(args$qmlFileName, "WrappedAnalysis.qml")
  expect_equal(args$qmlFile, "C:/fake/module/inst/qml/WrappedAnalysis.qml")
  expect_equal(args$modulePath, "C:/fake/module")
  expect_equal(args$options, opts)
  expect_true(args$preloadData)
  expect_false("functionCall" %in% names(args))
})

test_that("fetchRunArgs honors modulePath carried by options or explicit calls", {
  observedModulePaths <- character(0)
  restore <- localJaspToolsBindings(
    .jaspBaseRunWrappedAnalysis = function() fakeRunWrappedAnalysis,
    getModulePathFromRFunction = function(funName) {
      stop("ambient module resolution should not be used")
    },
    .jaspSyntaxResolveAnalysisQml = function(modulePath, analysisName) {
      observedModulePaths <<- c(observedModulePaths, modulePath)
      list(
        moduleName = "jaspFake",
        analysisName = analysisName,
        qmlFileName = paste0(analysisName, ".qml"),
        qmlFile = file.path(modulePath, "inst", "qml", paste0(analysisName, ".qml")),
        version = "1.0.0",
        preloadData = TRUE
      )
    }
  )
  on.exit(restore(), add = TRUE)

  opts <- list(flag = TRUE)
  attr(opts, "modulePath") <- "C:/attr/module"
  jaspTools:::fetchRunArgs("WrappedAnalysis", opts)
  jaspTools:::fetchRunArgs("WrappedAnalysis", opts, modulePath = "C:/explicit/module")

  expect_equal(observedModulePaths, c("C:/attr/module", "C:/explicit/module"))
})

test_that("fetchRunArgs preserves preloadData = FALSE from jaspSyntax for wrapped options", {
  restore <- localJaspToolsBindings(
    .jaspBaseRunWrappedAnalysis = function() fakeRunWrappedAnalysis,
    getModulePathFromRFunction = function(funName) "C:/fake/module",
    .jaspSyntaxResolveAnalysisQml = function(modulePath, analysisName) {
      list(
        moduleName = "jaspFake",
        analysisName = analysisName,
        qmlFileName = paste0(analysisName, ".qml"),
        version = "1.0.0",
        preloadData = FALSE
      )
    }
  )
  on.exit(restore(), add = TRUE)

  args <- jaspTools:::fetchRunArgs("NoDataAnalysis", list(flag = TRUE))

  expect_identical(attr(args, "runner"), fakeRunWrappedAnalysis)
  expect_false(args$preloadData)
})

test_that("fetchRunArgs requires jaspBase modulePath and qmlFile provenance support", {
  oldRunner <- function(moduleName, analysisName, qmlFileName, options, version, preloadData) NULL

  restore <- localJaspToolsBindings(
    .jaspBaseRunWrappedAnalysis = function() oldRunner
  )
  on.exit(restore(), add = TRUE)

  expect_error(
    jaspTools:::fetchRunArgs("WrappedAnalysis", list(flag = TRUE)),
    "modulePath",
    fixed = TRUE
  )
})

test_that("quiet runAnalysis uses subprocess containment outside testthat", {
  restoreOption <- localJaspToolsOptions(list(jaspTools.runAnalysis.subprocess = TRUE))
  on.exit(restoreOption(), add = TRUE)

  expect_true(jaspTools:::runAnalysisShouldUseSubprocess(
    quiet = TRUE,
    testEnvironment = FALSE
  ))
  expect_false(jaspTools:::runAnalysisShouldUseSubprocess(
    quiet = FALSE,
    testEnvironment = FALSE
  ))
  expect_true(jaspTools:::runAnalysisShouldUseSubprocess(
    quiet = FALSE,
    testEnvironment = TRUE
  ))
})

test_that("subprocess html files can be collected and restored", {
  sourceRoot <- tempfile("source-html-")
  targetRoot <- tempfile("target-html-")
  dir.create(file.path(sourceRoot, "plots"), recursive = TRUE)
  writeBin(as.raw(c(1, 2, 3, 4)), file.path(sourceRoot, "plots", "plot.png"))
  writeLines("<html></html>", file.path(sourceRoot, "tmp-index.html"))

  files <- jaspTools:::collectSubprocessHtmlFiles(sourceRoot)
  restored <- jaspTools:::restoreSubprocessHtmlFiles(files, targetRoot)

  expect_true(restored)
  expect_equal(
    readBin(file.path(targetRoot, "plots", "plot.png"), what = "raw", n = 4L),
    as.raw(c(1, 2, 3, 4))
  )
  expect_true(file.exists(file.path(targetRoot, "tmp-index.html")))
})

test_that("subprocess warnings are replayed in the parent session", {
  expect_warning(
    jaspTools:::replaySubprocessWarnings("first warning"),
    "first warning",
    fixed = TRUE
  )
})

test_that("runAnalysis verbosity separates replayed subprocess streams", {
  expect_identical(jaspTools:::normalizeRunAnalysisVerbose(NULL, quiet = TRUE), "analysis")
  expect_identical(jaspTools:::normalizeRunAnalysisVerbose(NULL, quiet = FALSE), "all")
  expect_identical(jaspTools:::normalizeRunAnalysisVerbose(TRUE), "all")
  expect_identical(jaspTools:::normalizeRunAnalysisVerbose(FALSE), "none")
  expect_identical(jaspTools:::normalizeRunAnalysisVerbose("jasp"), "jasp")
  expect_error(
    jaspTools:::normalizeRunAnalysisVerbose("loud"),
    "`verbose` must be one of"
  )

  expect_message(
    jaspTools:::replaySubprocessMessages("analysis message", verbose = "analysis"),
    "analysis message"
  )
  expect_silent(jaspTools:::replaySubprocessMessages("analysis message", verbose = "jasp"))

  expect_warning(
    jaspTools:::replaySubprocessWarnings("analysis warning", verbose = "analysis"),
    "analysis warning"
  )
  expect_silent(jaspTools:::replaySubprocessWarnings("analysis warning", verbose = "jasp"))

  expect_output(
    jaspTools:::replaySubprocessOutput("Desktop: native output", verbose = "jasp"),
    "Desktop: native output"
  )
  expect_silent(jaspTools:::replaySubprocessOutput("Desktop: native output", verbose = "analysis"))
})

test_that("subprocess env only carries requested variables", {
  env <- jaspTools:::.jaspToolsSubprocessEnv("JASPTOOLS_FAKE_CHILD")

  expect_named(env, "JASPTOOLS_FAKE_CHILD")
  expect_identical(env$JASPTOOLS_FAKE_CHILD, "true")
})

test_that("subprocess payload carries selected R options", {
  restoreOption <- localJaspToolsOptions(list(jaspLegacyRngKind = FALSE))
  on.exit(restoreOption(), add = TRUE)

  payload <- jaspTools:::.jaspToolsSubprocessPayload()

  expect_identical(payload$rOptions$jaspLegacyRngKind, FALSE)
})

test_that("runAnalysis sends processed results to the viewer", {
  rawJson <- '{"status":"complete","results":{"table":{"data":[{"name":"JaspColumn_1_Encoded"}]}}}'
  processed <- list(
    status = "complete",
    results = list(table = list(data = list(list(name = "decoded name"))))
  )
  viewed <- NULL
  observedOrder <- character()

  restore <- localJaspToolsBindings(
    insideTestEnvironment = function() FALSE,
    fetchRunArgs = function(name, options, modulePath = NULL) {
      observedOrder <<- c(observedOrder, "fetch")
      args <- list()
      attr(args, "runner") <- function() structure(list(), class = c("jaspResultsR", "R6"))
      attr(args, "modulePath") <- modulePath
      args
    },
    initAnalysisRuntime = function(...) {
      observedOrder <<- c(observedOrder, "init")
      invisible(NULL)
    },
    .resetRunTimeInternals = function() invisible(NULL),
    getJsonResultsFromJaspResults = function(results) rawJson,
    transferPlotsFromjaspResults = function() invisible(NULL),
    processJsonResults = function(jsonResults) {
      expect_identical(jsonResults, rawJson)
      processed
    },
    view = function(results) {
      viewed <<- results
      invisible("fake.html")
    }
  )
  on.exit(restore(), add = TRUE)

  result <- jaspTools::runAnalysis(
    "FakeAnalysis",
    dataset = data.frame(x = 1),
    options = list(variable = "x"),
    view = TRUE,
    quiet = FALSE,
    modulePath = "C:/fake/module"
  )

  expect_identical(result, processed)
  expect_identical(viewed, processed)
  expect_identical(jaspTools:::.getInternal("lastResults"), rawJson)
  expect_equal(observedOrder, c("fetch", "init"))
})

test_that("subprocess runAnalysis parent views returned processed results", {
  rawJson <- '{"status":"complete","results":{"table":{"data":[{"name":"JaspColumn_1_Encoded"}]}}}'
  processed <- list(
    status = "complete",
    results = list(table = list(data = list(list(name = "decoded name"))))
  )
  viewed <- NULL
  observedPayload <- NULL

  restore <- localJaspToolsBindings(
    .jaspToolsRunSubprocess = function(task, payload, failureMessage, isError) {
      observedPayload <<- payload
      expect_equal(task, "runAnalysis")
      expect_match(failureMessage, "runAnalysis", fixed = TRUE)
      expect_false(isError(list(result = processed)))
      list(
        result = processed,
        lastResults = rawJson,
        htmlFiles = list(files = list()),
        messages = character(0),
        output = character(0),
        warnings = character(0)
      )
    },
    view = function(results) {
      viewed <<- results
      invisible("fake.html")
    }
  )
  on.exit(restore(), add = TRUE)

  result <- jaspTools:::runAnalysisInSubprocess(
    name = "FakeAnalysis",
    dataset = data.frame(x = 1),
    options = list(variable = "x"),
    view = TRUE,
    quiet = TRUE,
    makeTests = FALSE
  )

  expect_false(observedPayload$args$view)
  expect_false(observedPayload$args$makeTests)
  expect_identical(observedPayload$args$verbose, "analysis")
  expect_identical(observedPayload$env$JASPTOOLS_RUNANALYSIS_CHILD, "true")
  expect_true(all(c("NOT_CRAN", "LANG", "LANGUAGE") %in% names(observedPayload$env)))
  expect_identical(result, processed)
  expect_identical(viewed, processed)
  expect_identical(jaspTools:::.getInternal("lastResults"), rawJson)
})

test_that("native QML replay accepts saved bound scalar options", {
  fixtureModule <- normalizePath(
    file.path(testthat::test_path(), "fixtures", "minimalModule"),
    winslash = "/",
    mustWork = FALSE
  )
  testthat::skip_if_not(dir.exists(fixtureModule), "minimal module fixture is unavailable")

  opts <- jaspSyntax::readAnalysisOptionsFromQml(
    fixtureModule,
    "MinimalAnalysis",
    options = list(
      choice = "one",
      flag = FALSE,
      threshold = 2.5
    ),
    fresh = TRUE,
    includeMeta = FALSE
  )

  expect_equal(opts$choice, "one")
  expect_false(opts$flag)
  expect_equal(opts$threshold, 2.5)
})

test_that("run argument construction rejects analysis and module metadata mismatch", {
  restore <- localJaspToolsBindings(
    .jaspBaseRunWrappedAnalysis = function() fakeRunWrappedAnalysis,
    getModulePathFromRFunction = function(funName) "C:/fake/module",
    .jaspSyntaxResolveAnalysisQml = function(modulePath, analysisName) {
      list(
        moduleName = "jaspFake",
        analysisName = analysisName,
        qmlFileName = paste0(analysisName, ".qml"),
        version = "1.0.0",
        preloadData = TRUE
      )
    }
  )
  on.exit(restore(), add = TRUE)

  wrongAnalysis <- list(flag = TRUE)
  attr(wrongAnalysis, "analysisName") <- "OtherAnalysis"
  expect_error(
    jaspTools:::fetchRunArgs("PreparedAnalysis", wrongAnalysis),
    "tagged for analysis"
  )

  wrongModule <- list(flag = TRUE)
  attr(wrongModule, "analysisName") <- "PreparedAnalysis"
  attr(wrongModule, "moduleName") <- "jaspOther"
  expect_error(
    jaspTools:::fetchRunArgs("PreparedAnalysis", wrongModule),
    "tagged for module"
  )
})

test_that("runAnalysis rejects prepared runtime options", {
  observed <- list(initCalled = FALSE, fetchCalled = FALSE)

  restore <- localJaspToolsBindings(
    initAnalysisRuntime = function(...) {
      observed$initCalled <<- TRUE
      invisible(NULL)
    },
    fetchRunArgs = function(...) {
      observed$fetchCalled <<- TRUE
      stop("fetchRunArgs should not be called")
    },
    .resetRunTimeInternals = function() invisible(NULL)
  )
  on.exit(restore(), add = TRUE)

  opts <- jaspTools:::markPreparedOptions(list(flag = TRUE))

  expect_error(
    jaspTools::runAnalysis("PreparedAnalysis", dataset = data.frame(x = 1), options = opts),
    "inspection only",
    fixed = TRUE
  )
  expect_false(observed$initCalled)
  expect_false(observed$fetchCalled)
})

test_that("real saved .jasp options replay once through runAnalysis with extracted data", {
  testthat::skip_if_not(
    identical(Sys.getenv("JASPTOOLS_RUN_REAL_DESCRIPTIVES"), "true"),
    "set JASPTOOLS_RUN_REAL_DESCRIPTIVES=true to run local jaspDescriptives native integration tests"
  )
  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  testthat::skip_if_not(file.exists(jaspFile), "debug descriptives .jasp fixture is unavailable")
  testthat::skip_if_not_installed("jaspDescriptives")
  skip_if_no_jaspSyntax_dataset_api()

  modulePath <- localRealDescriptivesModulePath()
  testthat::skip_if_not(
    !is.na(modulePath),
    "set JASPTOOLS_REAL_DESCRIPTIVES_MODULE or keep jaspDescriptives beside jaspTools"
  )
  skip_if_descriptives_qml_has_known_pareto_bug(modulePath)

  restoreRngOption <- localJaspToolsOptions(list(jaspLegacyRngKind = FALSE))
  on.exit(restoreRngOption(), add = TRUE)

  restorePkgOptions <- localDescriptivesModuleOptions(modulePath)
  on.exit(restorePkgOptions(), add = TRUE)

  restore <- localJaspToolsBindings(
    getModulePathFromRFunction = function(funName) {
      expect_equal(funName, "Descriptives")
      modulePath
    },
    getModulePaths = function() modulePath,
    getPkgOption = function(name) {
      switch(
        name,
        reinstall.modules = FALSE,
        language = "en",
        data.dirs = character(0),
        html.dir = tempdir(),
        view.in.rstudio = FALSE,
        ""
      )
    }
  )
  on.exit(restore(), add = TRUE)

  opts <- jaspTools::analysisOptions(jaspFile)
  expect_equal(attr(opts, "analysisName"), "Descriptives")
  expect_equal(attr(opts, "moduleName"), "jaspDescriptives")
  expect_equal(attr(opts, "jaspTools.optionShape"), "saved")
  expect_false(jaspTools:::isPreparedOptions(opts))
  expect_true(is.list(opts$variables))
  expect_equal(unlist(opts$variables$value, use.names = FALSE), "contNormal")
  expect_equal(unlist(opts$variables$types, use.names = FALSE), "scale")
  opts <- disableDescriptivesPlotOptions(opts)

  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)
  expect_s3_class(dataset, "data.frame")
  expect_equal(dim(dataset), c(100L, 31L))
  expect_true("contNormal" %in% names(dataset))

  results <- jaspTools::runAnalysis("Descriptives", dataset, opts, view = FALSE, quiet = TRUE)
  stats <- results$results$stats
  firstRow <- stats$data[[1L]]

  expect_equal(results$status, "complete")
  expect_equal(stats$status, "complete")
  expect_equal(firstRow$Valid, 100L)
  expect_equal(firstRow$Missing, 0L)
  expect_equal(firstRow$MeanArithmetic, mean(dataset$contNormal), tolerance = 1e-6)
  expect_equal(firstRow[["Std. Deviation"]], stats::sd(dataset$contNormal), tolerance = 1e-6)
})

test_that("real QML defaults can be edited and replayed through QML once", {
  testthat::skip_if_not(
    identical(Sys.getenv("JASPTOOLS_RUN_REAL_DESCRIPTIVES"), "true"),
    "set JASPTOOLS_RUN_REAL_DESCRIPTIVES=true to run local jaspDescriptives native integration tests"
  )
  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  testthat::skip_if_not(file.exists(jaspFile), "debug descriptives .jasp fixture is unavailable")
  testthat::skip_if_not_installed("jaspDescriptives")
  skip_if_no_jaspSyntax_dataset_api()

  modulePath <- localRealDescriptivesModulePath()
  testthat::skip_if_not(
    !is.na(modulePath),
    "set JASPTOOLS_REAL_DESCRIPTIVES_MODULE or keep jaspDescriptives beside jaspTools"
  )
  skip_if_descriptives_qml_has_known_pareto_bug(modulePath)

  restoreRngOption <- localJaspToolsOptions(list(jaspLegacyRngKind = FALSE))
  on.exit(restoreRngOption(), add = TRUE)

  restorePkgOptions <- localDescriptivesModuleOptions(modulePath)
  on.exit(restorePkgOptions(), add = TRUE)

  restore <- localJaspToolsBindings(
    getModulePathFromRFunction = function(funName) {
      expect_equal(funName, "Descriptives")
      modulePath
    },
    getModulePaths = function() modulePath,
    getPkgOption = function(name) {
      switch(
        name,
        reinstall.modules = FALSE,
        language = "en",
        data.dirs = character(0),
        html.dir = tempdir(),
        view.in.rstudio = FALSE,
        ""
      )
    }
  )
  on.exit(restore(), add = TRUE)

  opts <- jaspTools::analysisOptions("Descriptives")
  expect_equal(attr(opts, "jaspTools.optionShape"), "qml")
  expect_false(any(grepl("\\.types$", names(opts))))
  opts$variables <- "contNormal"
  opts <- disableDescriptivesPlotOptions(opts)

  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)
  results <- jaspTools::runAnalysis("Descriptives", dataset, opts, view = FALSE, quiet = TRUE)
  stats <- results$results$stats
  firstRow <- stats$data[[1L]]

  expect_equal(results$status, "complete")
  expect_equal(stats$status, "complete")
  expect_equal(firstRow$Valid, 100L)
  expect_equal(firstRow$MeanArithmetic, mean(dataset$contNormal), tolerance = 1e-6)
})

test_that("real runtime .jasp options are inspection-only", {
  testthat::skip_if_not(
    identical(Sys.getenv("JASPTOOLS_RUN_REAL_DESCRIPTIVES"), "true"),
    "set JASPTOOLS_RUN_REAL_DESCRIPTIVES=true to run local jaspDescriptives native integration tests"
  )
  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  testthat::skip_if_not(file.exists(jaspFile), "debug descriptives .jasp fixture is unavailable")
  testthat::skip_if_not_installed("jaspDescriptives")

  modulePath <- localRealDescriptivesModulePath()
  testthat::skip_if_not(
    !is.na(modulePath),
    "set JASPTOOLS_REAL_DESCRIPTIVES_MODULE or keep jaspDescriptives beside jaspTools"
  )

  restorePkgOptions <- localDescriptivesModuleOptions(modulePath)
  on.exit(restorePkgOptions(), add = TRUE)

  restore <- localJaspToolsBindings(
    getModulePathFromRFunction = function(funName) {
      expect_equal(funName, "Descriptives")
      modulePath
    },
    getModulePaths = function() modulePath,
    getPkgOption = function(name) {
      switch(
        name,
        reinstall.modules = FALSE,
        language = "en",
        data.dirs = character(0),
        html.dir = tempdir(),
        view.in.rstudio = FALSE,
        ""
      )
    }
  )
  on.exit(restore(), add = TRUE)

  opts <- jaspTools::analysisRuntimeOptions(jaspFile)
  expect_equal(attr(opts, "analysisName"), "Descriptives")
  expect_equal(attr(opts, "moduleName"), "jaspDescriptives")
  expect_true(jaspTools:::isPreparedOptions(opts))
  expect_true("variables.types" %in% names(opts))
  expect_match(unlist(opts$variables, use.names = FALSE)[[1L]], "^JaspColumn_.*_Encoded$")
  expect_equal(unlist(opts$`variables.types`, use.names = FALSE), "scale")

  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)
  expect_error(
    jaspTools::runAnalysis("Descriptives", dataset, opts, view = FALSE, quiet = TRUE),
    "inspection only",
    fixed = TRUE
  )
})
