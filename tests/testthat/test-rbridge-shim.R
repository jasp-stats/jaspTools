context("minimal rbridge shim")

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

localJaspToolsOptions <- function(values) {
  oldValues <- options(values)

  function() {
    options(oldValues)
    invisible(NULL)
  }
}

localGlobalSymbol <- function(name, value) {
  hadValue <- exists(name, envir = .GlobalEnv, inherits = FALSE)
  oldValue <- if (hadValue) get(name, envir = .GlobalEnv, inherits = FALSE) else NULL
  wasLocked <- hadValue && bindingIsLocked(name, .GlobalEnv)

  if (wasLocked)
    unlockBinding(name, .GlobalEnv)
  assign(name, value, envir = .GlobalEnv)
  if (wasLocked)
    lockBinding(name, .GlobalEnv)

  function() {
    if (exists(name, envir = .GlobalEnv, inherits = FALSE) &&
        bindingIsLocked(name, .GlobalEnv)) {
      unlockBinding(name, .GlobalEnv)
    }

    if (hadValue) {
      assign(name, oldValue, envir = .GlobalEnv)
      if (wasLocked)
        lockBinding(name, .GlobalEnv)
    } else if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = name, envir = .GlobalEnv)
    }

    invisible(NULL)
  }
}

test_that("rbridge hook injects only local runtime callbacks", {
  env <- new.env(parent = emptyenv())

  result <- jaspTools:::.insertRbridgeIntoEnv(env)

  expect_identical(result, env)
  expect_setequal(ls(env, all.names = TRUE), jaspTools:::.rbridgeNativeSymbols())
})

test_that("rbridge hook leaves native jaspSyntax dataset callbacks alone", {
  env <- new.env(parent = emptyenv())
  nativeRequested <- function() "native"
  environment(nativeRequested) <- emptyenv()
  env$.readDataSetRequestedNative <- nativeRequested

  jaspTools:::.insertRbridgeIntoEnv(env)

  expect_identical(env$.readDataSetRequestedNative, nativeRequested)
  expect_true(identical(environmentName(environment(env$.requestTempFileNameNative)), "jaspTools"))
  expect_false(".readDataSetRequestedNative" %in% jaspTools:::.rbridgeNativeSymbols())
})

test_that("state callback initializes the state file expected by jaspBase", {
  jaspTools:::.resetRunStateFile()
  location <- jaspTools:::.requestStateFileNameNative()
  stateFile <- file.path(location$root, location$relativePath)

  expect_true(file.exists(stateFile))
  loadedNames <- load(stateFile)

  expect_identical(loadedNames, "state")
  expect_null(state)
})

test_that("processed results read standalone jaspBase state from callback file", {
  restoreDecode <- localJaspToolsBinding(".jaspSyntaxDecodeAnalysisResults", function(results) results)
  on.exit(restoreDecode(), add = TRUE)
  on.exit(jaspTools:::.resetRunStateFile(), add = TRUE)

  location <- jaspTools:::.requestStateFileNameNative()
  stateFile <- file.path(location$root, location$relativePath)
  state <- list(figures = list(), other = list(answer = 42))
  save(state, file = stateFile, compress = FALSE)

  results <- jaspTools:::processJsonResults('{"status":"complete","results":{}}')

  expect_equal(results$state, state)
})

test_that("rbridge globals are restored after temporary injection", {
  env <- new.env(parent = emptyenv())
  original <- function() "original"
  env$.baseCitation <- original

  state <- jaspTools:::.snapshotRbridgeEnv(env)
  jaspTools:::.insertRbridgeIntoEnv(env)
  expect_identical(env$.baseCitation, jaspTools:::.baseCitation)

  restored <- jaspTools:::.restoreRbridgeEnv(env, state)

  expect_true(restored)
  expect_identical(env$.baseCitation, original)

  emptyEnv <- new.env(parent = emptyenv())
  emptyState <- jaspTools:::.snapshotRbridgeEnv(emptyEnv)
  jaspTools:::.insertRbridgeIntoEnv(emptyEnv)
  jaspTools:::.restoreRbridgeEnv(emptyEnv, emptyState)

  expect_equal(ls(emptyEnv, all.names = TRUE), character(0))
})

test_that("runAnalysis restores rbridge globals when the runner errors", {
  restoreOption <- localJaspToolsOptions(list(jaspTools.runAnalysis.subprocess = FALSE))
  originalBaseCitation <- function() "original"
  restoreGlobal <- localGlobalSymbol(".baseCitation", originalBaseCitation)

  runner <- function(...) {
    stop("runner failed", call. = FALSE)
  }
  args <- list(
    moduleName = "jaspFake",
    analysisName = "FakeAnalysis",
    qmlFileName = "FakeAnalysis.qml",
    qmlFile = "C:/fake/module/inst/qml/FakeAnalysis.qml",
    modulePath = "C:/fake/module",
    options = list(flag = TRUE),
    version = "1.0.0",
    preloadData = FALSE
  )
  attr(args, "runner") <- runner
  attr(args, "modulePath") <- "C:/fake/module"

  restoreFetch <- localJaspToolsBinding("fetchRunArgs", function(...) args)
  restoreInit <- localJaspToolsBinding("initAnalysisRuntime", function(...) {
    jaspTools:::.insertRbridgeIntoEnv(.GlobalEnv)
  })
  restoreReset <- localJaspToolsBinding(".resetRunTimeInternals", function() invisible(NULL))

  on.exit(restoreOption(), add = TRUE)
  on.exit(restoreGlobal(), add = TRUE)
  on.exit(restoreFetch(), add = TRUE)
  on.exit(restoreInit(), add = TRUE)
  on.exit(restoreReset(), add = TRUE)

  expect_error(
    jaspTools::runAnalysis(
      "FakeAnalysis",
      data.frame(),
      list(flag = TRUE),
      view = FALSE,
      modulePath = "C:/fake/module"
    ),
    "runner failed",
    fixed = TRUE
  )
  expect_identical(get(".baseCitation", envir = .GlobalEnv), originalBaseCitation)
})

test_that("rbridge namespace contract is limited to local runtime objects", {
  expected <- c(
    ".baseCitation",
    ".ppi",
    ".requestTempFileNameNative",
    ".requestTempRootNameNative",
    ".requestStateFileNameNative",
    ".imageBackground"
  )

  expect_setequal(jaspTools:::.rbridgeNativeSymbols(), expected)

  removedPlaceholders <- c(
    ".automaticColumnEncDecoding",
    ".encodeColNamesStrict",
    ".decodeColNamesStrict",
    ".encodeColNamesLax",
    ".decodeColNamesLax",
    ".decodeColTypes",
    ".setColumnDataAsScale",
    ".setColumnDataAsOrdinal",
    ".setColumnDataAsNominal",
    ".setColumnDataAsNominalText",
    ".allColumnNamesDataset",
    ".readDatasetToEndNative",
    ".readDataSetHeaderNative",
    ".readDataSetRequestedNative"
  )

  expect_false(any(removedPlaceholders %in% jaspTools:::.rbridgeNativeSymbols()))

  ns <- asNamespace("jaspTools")
  for (symbol in expected) {
    expect_true(exists(symbol, envir = ns, inherits = FALSE), info = symbol)
    expect_true(length(utils::getAnywhere(symbol)$objs) > 0L, info = symbol)
  }

  for (symbol in c(".readDatasetToEndNative", ".readDataSetHeaderNative", ".readDataSetRequestedNative"))
    expect_false(exists(symbol, envir = ns, inherits = FALSE), info = symbol)
})
