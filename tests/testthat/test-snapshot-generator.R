test_that("generated expectations preserve complete output nodes and stable result paths", {
  empty <- list(title = 'Summary "A"', schema = list(fields = list()), data = list())
  html <- list(rawtext = "warning", text = "<p>warning</p>")
  report <- list(html = "<p>report</p>", topHtml = "<b>notice</b>")
  plot <- list(title = "Same title", data = "resources/12.png", width = 100, height = 100)
  tree <- list(container = list(collection = list(
    'a.b' = empty, 'a/b' = empty, 'a"b' = html, note = report, graph = plot
  )))
  tree$other <- list(collection = list('a.b' = empty, graph = plot))

  tests <- getTests(tree, analysisIndex = 2)
  expect_identical(vapply(tests, "[[", "", "type"),
                   c("table", "table", "html", "html", "plot", "table", "plot"))
  expect_identical(tests[[1L]]$node, empty)
  expect_identical(tests[[3L]]$path, list("container", "collection", 'a"b'))
  names <- vapply(tests, "[[", "", "name")
  expect_false(anyDuplicated(names) > 0L)
  expect_identical(names[c(5, 7)], c("analysis-2_figure-1_same-title", "analysis-2_figure-2_same-title"))
  expect_true(all(grepl("^[a-z0-9][a-z0-9_-]+$", names)))
  extra <- getTests(c(list(first = empty), tree), analysisIndex = 2)
  expect_identical(names, vapply(extra[-1L], "[[", "", "name"))
  expect_false(names[1L] == getTests(tree, analysisIndex = 1)[[1L]]$name)

  block <- generateExampleTestBlock("Example", 2, 3, 'A"file.jasp', "other", list(results = tree))
  expect_length(parse(text = block), 1)
  expect_match(block, 'results[["results"]][["container"]][["collection"]][["a.b"]]', fixed = TRUE)
  expect_match(block, "expect_snapshot_table", fixed = TRUE)
  expect_match(block, "expect_snapshot_html", fixed = TRUE)
  expect_match(block, "expect_equal_plots", fixed = TRUE)
  lines <- strsplit(block, "\n", fixed = TRUE)[[1L]]
  expect_lt(max(grep("expect_snapshot_", lines)), min(grep("expect_equal_plots", lines)))
  expect_false(grepl("expect_equal_tables", block, fixed = TRUE))
  expect_false(grepl("<p>warning", block, fixed = TRUE))

  local_mocked_bindings(addPreambleLines = function(...) "options <- list()\nresults <- list()")
  manual <- makeExpectations(getTests(tree), "Example", list(), "debug.csv")
  expect_length(parse(text = manual), 1)
  statements <- as.list(parse(text = manual)[[1L]][[3L]])
  expect_identical(statements[[2L]], quote(jaspTools::preserveSnapshotReferences()))
  expect_match(manual, "expect_snapshot_html", fixed = TRUE)
  manualPlots <- vapply(Filter(function(test) test$type == "plot", getTests(tree)), "[[", "", "name")
  expect_false(anyDuplicated(manualPlots) > 0L)
  expect_identical(manualPlots[1L], "same-title")
})

test_that("result metadata distinguishes HTML, report and JSON in nested collections", {
  meta <- list(
    list(type = "collection", name = "outer", meta = list(
      list(type = "htmlNode", name = "markup"), list(type = "reportNode", name = "report")
    )),
    list(type = "json", name = "json")
  )
  tree <- list(.meta = meta, outer = list(collection = list(
    markup = list(text = "<p>text</p>"), report = list(html = "<b>report</b>", topHtml = "")
  )), json = list(data = list(), schema = list()))
  expect_identical(vapply(getTests(tree), "[[", "", "type"), c("html", "html"))
  tree$outer$collection$markup <- NULL
  expect_error(getTests(tree), "Missing html result at outer/collection/markup", fixed = TRUE)
})

test_that("numeric collection positions stay distinct from literal numeric names", {
  html <- list(text = "<p>text</p>", rawtext = "text")
  tree <- list(container = list(collection = stats::setNames(list(html, html), c("", "1"))))
  tests <- getTests(tree, analysisIndex = 1)
  expect_length(tests, 2)
  expect_false(identical(tests[[1L]]$name, tests[[2L]]$name))
  expect_match(snapshotExpectationLines(tests[[1L]]), '[["collection"]][[1]]', fixed = TRUE)
  expect_match(snapshotExpectationLines(tests[[2L]]), '[["collection"]][["1"]]', fixed = TRUE)
})

test_that("regeneration recognises literal snapshot names in previous tests", {
  test <- expression(
    test_that("old test", {
      jaspTools::expect_snapshot_table(name = "stats", node = value)
      expect_snapshot_html(html, "details")
      jaspTools::expect_equal_plots(plot, "gg")
      jaspTools::expect_equal_tables(table, list("old", 1))
      expect_snapshot_html(html, dynamicName)
    })
  )
  test <- c(test, parse(text = 'anotherPackage::expect_snapshot_html(html, "elsewhere")'))
  expect_identical(generatedSnapshotIDs(test), c("table stats", "html details", "plot gg"))
})

test_that("example generation seeds from the rerun and stages changed snapshots", {
  moduleDir <- withr::local_tempdir()
  jaspFile <- file.path(moduleDir, "Example File.jasp")
  table <- function(x) list(title = "Results", schema = list(fields = list(list(name = "score"))),
                            data = list(list(score = x)))
  html <- function(x) list(rawtext = x, text = paste0("<p>", x, "</p>"))
  saved <- list(list(name = "Example", status = "complete", results = list(scores = table(10))))
  results <- list(status = "complete", results = list(scores = table(20), html = html("first")))
  local_mocked_bindings(readAnalysisResultsFromJaspFile = function(...) saved, .package = "jaspSyntax")
  local_mocked_bindings(hasModuleDirs = function() FALSE, jaspFileAnalysisSyntax = function(...) NULL,
                        runAnalysisFromJaspFile = function(...) results)
  generate <- function() makeTestsFromSingleJASPFile(jaspFile, moduleDir, "other", overwrite = TRUE)
  expect_warning(file <- generate(), "does not give the results saved")
  expect_true(file.exists(file))
  refDir <- file.path(dirname(file), "_snaps", "other-Example File")
  refTable <- list.files(refDir, "\\.table\\.json$", full.names = TRUE)
  refHtml <- list.files(refDir, "\\.html$", full.names = TRUE)
  expect_length(refTable, 1)
  expect_length(refHtml, 1)
  expect_equal(jsonlite::read_json(refTable)$data[[1]]$score, 20)
  expect_true(any(grepl("<p>first</p>", readLines(refHtml), fixed = TRUE)))
  replay <- as.data.frame(testthat::test_dir(dirname(file), reporter = "silent", stop_on_failure = FALSE))
  expect_equal(replay$passed, 3L)
  expect_equal(replay$failed, 0L)
  expect_false(replay$error)

  results$results$scores <- table(21)
  results$results$html <- html("second")
  expect_warning(generate(), "does not give the results saved")
  expect_equal(jsonlite::read_json(refTable)$data[[1]]$score, 20)
  expect_equal(jsonlite::read_json(snapshotCandidatePath(refTable))$data[[1]]$score, 21)
  expect_true(any(grepl("<p>first</p>", readLines(refHtml), fixed = TRUE)))
  expect_true(any(grepl("<p>second</p>", readLines(snapshotCandidatePath(refHtml)), fixed = TRUE)))

  originalFiles <- tools::md5sum(c(file, list.files(refDir, full.names = TRUE)))
  results$results$html <- NULL
  expect_error(suppressWarnings(generate()), "would remove snapshot expectations: html", fixed = TRUE)
  expect_identical(tools::md5sum(names(originalFiles)), originalFiles)

  # A failed backend run can retain the exact output saved in the references.
  results <- list(status = "fatalError", results = list(
    scores = table(20), html = html("first"), errorMessage = "example failed"))
  replay <- as.data.frame(testthat::test_dir(dirname(file), reporter = "silent", stop_on_failure = FALSE))
  expect_equal(replay$failed, 1L)

  originalFiles <- tools::md5sum(c(file, list.files(refDir, full.names = TRUE)))
  results <- list(status = "fatalError", results = list(errorMessage = "example failed"))
  expect_error(generate(), "Could not regenerate.*example failed")
  expect_identical(tools::md5sum(names(originalFiles)), originalFiles)
})
