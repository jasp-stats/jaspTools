context("generated example tests")

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

test_that("generated example test blocks use saved options and dataset replay path", {
  block <- jaspTools:::generateExampleTestBlock(
    analysisName = "SelectionModels",
    analysisIndex = 1,
    totalAnalyses = 1,
    jaspFileName = "example.jasp",
    sourceFolder = "library",
    results = list(results = list(), state = list())
  )

  expect_true(grepl('modulePath <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)', block, fixed = TRUE))
  expect_true(grepl("opts <- jaspTools::analysisOptions(jaspFile, modulePath = modulePath)", block, fixed = TRUE))
  expect_true(grepl("dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)", block, fixed = TRUE))
  expect_true(grepl(
    'results <- jaspTools::runAnalysis\\("SelectionModels", dataset, opts, modulePath = modulePath\\)',
    block
  ))
  expect_true(grepl("jaspTools:::.expectNoGeneratedExampleFailureStatus(results)", block, fixed = TRUE))
  expect_false(grepl("encodeOptionsAndDataset", block, fixed = TRUE))
  expect_false(grepl("encodedDataset", block, fixed = TRUE))
  expect_false(grepl("forceEncode", block, fixed = TRUE))
})

test_that("basic generated example test blocks use saved options and dataset replay path", {
  block <- jaspTools:::generateExampleTestBlockBasic(
    analysisName = "SelectionModels",
    analysisIndex = 1,
    totalAnalyses = 1,
    jaspFileName = "example.jasp",
    sourceFolder = "library"
  )

  expect_true(grepl('modulePath <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)', block, fixed = TRUE))
  expect_true(grepl("opts <- jaspTools::analysisOptions(jaspFile, modulePath = modulePath)", block, fixed = TRUE))
  expect_true(grepl("dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)", block, fixed = TRUE))
  expect_true(grepl(
    'results <- jaspTools::runAnalysis\\("SelectionModels", dataset, opts, modulePath = modulePath\\)',
    block
  ))
  expect_true(grepl("jaspTools:::.expectNoGeneratedExampleFailureStatus(results)", block, fixed = TRUE))
  expect_false(grepl("encodeOptionsAndDataset", block, fixed = TRUE))
  expect_false(grepl("encodedDataset", block, fixed = TRUE))
  expect_false(grepl("forceEncode", block, fixed = TRUE))
})

test_that("generated fallback status helper catches JASP failure statuses", {
  for (status in c("error", "validationError", "fatalError")) {
    expect_error(
      jaspTools:::.expectNoGeneratedExampleFailureStatus(
        list(status = status, results = list(errorMessage = "boom"))
      ),
      class = "expectation_failure"
    )
  }

  expect_silent(jaspTools:::.expectNoGeneratedExampleFailureStatus(
    list(status = "complete", results = list())
  ))
})

test_that("generated example test blocks index options for multi-analysis files", {
  block <- jaspTools:::generateExampleTestBlock(
    analysisName = "SelectionModels",
    analysisIndex = 2,
    totalAnalyses = 3,
    jaspFileName = "example.jasp",
    sourceFolder = "library",
    results = list(results = list(), state = list())
  )

  expect_true(grepl("opts <- jaspTools::analysisOptions(jaspFile, modulePath = modulePath)[[2]]", block, fixed = TRUE))
  expect_false(grepl("encodeOptionsAndDataset", block, fixed = TRUE))
  expect_false(grepl("encodedDataset", block, fixed = TRUE))
})

test_that("basic generated example test blocks index options for multi-analysis files", {
  block <- jaspTools:::generateExampleTestBlockBasic(
    analysisName = "SelectionModels",
    analysisIndex = 2,
    totalAnalyses = 3,
    jaspFileName = "example.jasp",
    sourceFolder = "library"
  )

  expect_true(grepl("opts <- jaspTools::analysisOptions(jaspFile, modulePath = modulePath)[[2]]", block, fixed = TRUE))
  expect_false(grepl("encodeOptionsAndDataset", block, fixed = TRUE))
  expect_false(grepl("encodedDataset", block, fixed = TRUE))
})

test_that("test generator reads module analyses through jaspSyntax", {
  restore <- localJaspToolsBinding(
    ".jaspSyntaxReadModuleDescription",
    function(modulePath) {
      expect_equal(modulePath, "C:/fake/module")
      list(
        analyses = list(
          list(name = "AnalysisOne"),
          list(name = "AnalysisTwo")
        )
      )
    }
  )
  on.exit(restore(), add = TRUE)

  expect_equal(
    jaspTools:::readModuleAnalysisNames("C:/fake/module"),
    c("AnalysisOne", "AnalysisTwo")
  )
})

test_that("makeTestsFromSingleJASPFile runs generation with unprepared saved options", {
  moduleDir <- tempfile("jaspModule_")
  dir.create(file.path(moduleDir, "tests", "testthat"), recursive = TRUE)
  jaspFile <- tempfile(fileext = ".jasp")
  file.create(jaspFile)

  observedPrepared <- NULL

  restore <- list(
    localJaspToolsBinding("analysisOptionsFromJASPFile", function(file, modulePath) {
      opts <- list(variable = list(value = "x", types = "scale"), `.meta` = list())
      attr(opts, "analysisName") <- "SelectionModels"
      attr(opts, "jaspTools.optionShape") <- "saved"
      opts
    }),
    localJaspToolsBinding(".jaspSyntaxNamedModulePaths", function(module.dir) {
      expect_equal(module.dir, moduleDir)
      list(jaspFake = moduleDir)
    }),
    localJaspToolsBinding("extractDatasetFromJASPFile", function(file) {
      expect_equal(file, jaspFile)
      data.frame(x = 1:3)
    }),
    localJaspToolsBinding("runAnalysis", function(name, dataset, opts, view, quiet, ...) {
      observedPrepared <<- jaspTools:::isPreparedOptions(opts)
      list(results = list(), state = list())
    }),
    localJaspToolsBinding("generateExampleTestBlock", function(...) "test_that(\"generated\", {})")
  )
  on.exit(lapply(rev(restore), function(restoreBinding) restoreBinding()), add = TRUE)

  created <- jaspTools:::makeTestsFromSingleJASPFile(
    jaspFile,
    module.dir = moduleDir,
    sourceFolder = "other",
    overwrite = TRUE,
    pkgAnalyses = "SelectionModels"
  )

  expect_false(observedPrepared)
  expect_true(file.exists(created))
  expect_true(any(grepl("generated", readLines(created), fixed = TRUE)))
})

test_that("forceEncode transition argument fails loudly", {
  moduleDir <- tempfile("jaspModule_")
  dir.create(file.path(moduleDir, "tests", "testthat"), recursive = TRUE)
  jaspFile <- tempfile(fileext = ".jasp")
  file.create(jaspFile)

  expect_error(
    jaspTools:::makeTestsFromSingleJASPFile(
      jaspFile,
      module.dir = moduleDir,
      sourceFolder = "other",
      forceEncode = "model"
    ),
    "no longer supported",
    fixed = TRUE
  )

  expect_error(
    jaspTools::makeTestsFromExamples(
      module.dir = moduleDir,
      forceEncode = "model"
    ),
    "no longer supported",
    fixed = TRUE
  )
})
