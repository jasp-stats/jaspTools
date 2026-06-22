context("analysisOptions")

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

localNamespaceBinding <- function(name, value, namespace) {
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

localJaspToolsOptions <- function(values) {
  oldValues <- options(values)

  function() {
    options(oldValues)
    invisible(NULL)
  }
}

test_that("analysisOptions preserves JSON options without native replay", {
  opts <- jaspTools::analysisOptions('{
    "name": "LoggedAnalysis",
    "options": {
      ".meta": {
        "variables": {
          "shouldEncode": true
        }
      },
      "variables": ["x"]
    }
  }')

  expect_equal(attr(opts, "analysisName"), "LoggedAnalysis")
  expect_true(".meta" %in% names(opts))
  expect_equal(opts$variables, "x")
})

test_that("analysisOptions for analysis names requests editable unprepared defaults", {
  observed <- NULL

  restoreTools <- localJaspToolsBinding("getModulePathFromRFunction", function(analysis) {
    expect_equal(analysis, "FakeAnalysis")
    "C:/fake/module"
  })
  restoreSyntax <- localNamespaceBinding(
    "readDefaultAnalysisOptions",
    function(modulePath, analysisName, includeMeta, includeTypeOptions) {
      observed <<- list(
        modulePath = modulePath,
        analysisName = analysisName,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
      list(variables = list(), flag = TRUE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreTools(), add = TRUE)
  on.exit(restoreSyntax(), add = TRUE)

  opts <- jaspTools::analysisOptions("FakeAnalysis")

  expect_equal(observed$modulePath, "C:/fake/module")
  expect_equal(observed$analysisName, "FakeAnalysis")
  expect_false(observed$includeMeta)
  expect_false(observed$includeTypeOptions)
  expect_equal(attr(opts, "analysisName"), "FakeAnalysis")
  expect_equal(attr(opts, "jaspTools.optionShape"), "qml")
  expect_equal(attr(opts, "modulePath"), "C:/fake/module")
  expect_false(jaspTools:::isPreparedOptions(opts))
  expect_false(any(grepl("\\.types$", names(opts))))
})

test_that("analysisOptions for analysis names honors explicit modulePath", {
  observed <- NULL

  restoreTools <- localJaspToolsBinding("getModulePathFromRFunction", function(analysis) {
    stop("ambient module resolution should not be used")
  })
  restoreSyntax <- localNamespaceBinding(
    "readDefaultAnalysisOptions",
    function(modulePath, analysisName, includeMeta, includeTypeOptions) {
      observed <<- list(modulePath = modulePath, analysisName = analysisName)
      list(flag = TRUE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreTools(), add = TRUE)
  on.exit(restoreSyntax(), add = TRUE)

  opts <- jaspTools::analysisOptions("FakeAnalysis", modulePath = "C:/explicit/module")

  expect_equal(observed$modulePath, "C:/explicit/module")
  expect_equal(observed$analysisName, "FakeAnalysis")
  expect_equal(attr(opts, "modulePath"), "C:/explicit/module")
})

test_that("analysisOptions for analysis names accepts module-name keyed modulePath", {
  observed <- NULL

  restoreTools <- localJaspToolsBinding("getModulePathFromRFunction", function(analysis) {
    stop("ambient module resolution should not be used")
  })
  restoreResolve <- localNamespaceBinding(
    "resolveAnalysisQml",
    function(modulePath, analysisName) {
      expect_equal(modulePath, "C:/fake/module")
      expect_equal(analysisName, "FakeAnalysis")
      list()
    },
    asNamespace("jaspSyntax")
  )
  restoreDefaults <- localNamespaceBinding(
    "readDefaultAnalysisOptions",
    function(modulePath, analysisName, includeMeta, includeTypeOptions) {
      observed <<- list(modulePath = modulePath, analysisName = analysisName)
      list(flag = TRUE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreTools(), add = TRUE)
  on.exit(restoreResolve(), add = TRUE)
  on.exit(restoreDefaults(), add = TRUE)

  opts <- jaspTools::analysisOptions(
    "FakeAnalysis",
    modulePath = list(jaspFake = "C:/fake/module")
  )

  expect_equal(observed$modulePath, "C:/fake/module")
  expect_equal(observed$analysisName, "FakeAnalysis")
  expect_equal(attr(opts, "modulePath"), "C:/fake/module")
})

test_that("analysisOptions forwards modulePath for .jasp sources", {
  jaspFile <- tempfile(fileext = ".jasp")
  file.create(jaspFile)
  modulePath <- list(jaspFake = "C:/fake/module")
  observed <- NULL

  restore <- localJaspToolsBinding("analysisOptionsFromJASPFile", function(file, modulePath = NULL) {
    observed <<- list(file = file, modulePath = modulePath)
    opts <- list(variable = list(value = "x", types = "scale"))
    attr(opts, "analysisName") <- "FakeAnalysis"
    opts
  })
  on.exit(restore(), add = TRUE)

  opts <- jaspTools::analysisOptions(jaspFile, modulePath = modulePath)

  expect_equal(normalizePath(observed$file, winslash = "/", mustWork = FALSE),
               normalizePath(jaspFile, winslash = "/", mustWork = FALSE))
  expect_identical(observed$modulePath, modulePath)
  expect_equal(attr(opts, "analysisName"), "FakeAnalysis")
})

test_that("analysisOptions treats missing Windows .jasp paths as files, not JSON", {
  missingJaspFile <- normalizePath(
    file.path(tempdir(), "missing-analysis.jasp"),
    winslash = "/",
    mustWork = FALSE
  )

  err <- tryCatch(jaspTools::analysisOptions(missingJaspFile), error = function(e) e)

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "File not found", fixed = TRUE)
  expect_false(grepl("json", conditionMessage(err), ignore.case = TRUE))
})

test_that("analysisRuntimeOptions validates .jasp file paths before native dispatch", {
  missingJaspFile <- file.path(tempdir(), "missing-runtime.jasp")
  expect_error(
    jaspTools::analysisRuntimeOptions(missingJaspFile),
    "File not found"
  )

  csvFile <- tempfile(fileext = ".csv")
  file.create(csvFile)
  expect_error(
    jaspTools::analysisRuntimeOptions(csvFile),
    ".jasp extension",
    fixed = TRUE
  )
})

test_that("analysisOptions treats missing colon-containing non-jasp paths as files", {
  missingJsonFile <- normalizePath(
    file.path(tempdir(), "missing-options.json"),
    winslash = "/",
    mustWork = FALSE
  )

  expect_error(
    jaspTools::analysisOptions(missingJsonFile),
    "File not found",
    fixed = TRUE
  )
})
