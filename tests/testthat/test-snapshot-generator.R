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

test_that("manual replays retain explicit options and single or multiple datasets", {
  name <- "Example"
  options <- list('option "A"' = c("group A", "group B"), fixed = 0L,
                  label = 'quoted "title"', value = 1.2345678901234567, removed = NULL)
  frame <- data.frame(group = factor(c("A", "B")), count = c(1L, NA_integer_))
  many <- list('first "sample"' = frame, second = "debug.csv")
  replay <- function(dataset = NULL, datasets = NULL) {
    runAnalysis <- function(name, dataset = NULL, options, datasets = NULL)
      list(name = name, options = options, dataset = dataset, datasets = datasets)
    code <- addPreambleLines(name, options, dataset, datasets)
    eval(parse(text = code))
  }
  for (dataset in list(frame, 'C:\\data\\example.csv', NULL))
    expect_identical(replay(dataset), list(name = name, options = options, dataset = dataset, datasets = NULL))
  expect_identical(replay(datasets = many), list(name = name, options = options, dataset = NULL, datasets = many))

  table <- data.table::as.data.table(frame)
  data.table::setkey(table, group)
  expect_identical(replay(table)$dataset, frame)
  many[[1L]] <- table
  expect_identical(replay(datasets = many)$datasets, list('first "sample"' = frame, second = "debug.csv"))
  expect_identical(data.table::key(table), "group")
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
  expect_equal(replay$passed, 0L)
  expect_false(replay$error)
  expect_identical(tools::md5sum(names(originalFiles)), originalFiles)

  results <- list(status = "fatalError", results = list(errorMessage = "example failed"))
  expect_error(generate(), "Could not regenerate.*example failed")
  expect_identical(tools::md5sum(names(originalFiles)), originalFiles)
})

test_that("a first failed replay leaves no table, HTML or plot artifacts", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  dir.create(testDir, recursive = TRUE)
  tree <- list(
    stats = list(schema = list(fields = list(list(name = "n"))), data = list(list(n = 12))),
    note = list(text = "<p>stale</p>", rawtext = "stale"),
    plot = list(title = "Figure", data = "resources/1.png", width = 100, height = 100)
  )
  result <- list(status = "validationError", results = c(tree, list(errorMessage = "Invalid input")))
  local_mocked_bindings(
    runAnalysisFromJaspFile = function(...) result,
    addPreambleLines = function(...) 'results <- jaspTools::runAnalysisFromJaspFile("example.jasp", 1)'
  )
  example <- generateExampleTestBlock("Example", 1, 1, "example.jasp", "other", list(results = tree))
  manual <- makeExpectations(getTests(tree), "Manual", list(), "example.csv")
  writeLines(example, file.path(testDir, "test-example.R"))
  writeLines(manual, file.path(testDir, "test-manual.R"))

  replay <- as.data.frame(testthat::test_dir(testDir, reporter = "silent", stop_on_failure = FALSE))
  expect_equal(replay$failed, c(1L, 1L))
  expect_equal(replay$passed, c(0L, 0L))
  expect_false(any(replay$error | replay$skipped))
  expect_length(list.files(file.path(testDir, "_snaps"), recursive = TRUE), 0L)
})

test_that("failed publication restores the generated test, example, references and candidates", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  testFile <- file.path(testDir, "test-other-case.R")
  fixture <- file.path(module, "case.jasp")
  copy <- file.path(testDir, "jaspfiles", "other", "case.jasp")
  snapDir <- file.path(testDir, "_snaps", "other-case")
  writeSnapshotText("old test\n", testFile)
  writeSnapshotText("old fixture\n", copy)
  for (name in c("change", "match")) {
    writeSnapshotText("baseline\n", file.path(snapDir, paste0(name, ".html")))
    writeSnapshotText("previous change\n", file.path(snapDir, paste0(name, ".new.html")))
  }
  nodes <- Map(function(name, node) list(name = name, node = node, type = "html"),
               c("change", "match", "first"), c("changed", "baseline", "first"))
  publish <- function() publishGeneratedTest(testFile, fixture, copy, "new test", nodes)
  hashes <- function() tools::md5sum(list.files(testDir, recursive = TRUE, full.names = TRUE, all.files = TRUE))
  old <- hashes()
  expect_error(suppressWarnings(publish()), "Could not copy the JASP example")
  expect_identical(hashes(), old)

  writeSnapshotText("new fixture\n", fixture)
  realHashes <- snapshotDecisionHashes
  fail <- TRUE
  local_mocked_bindings(snapshotDecisionHashes = function(root, paths) {
    value <- realHashes(root, paths)
    # Fail after all live files, including the last replacement, have been moved.
    if (fail && length(paths) == 1L && identical(paths, file.path(root, basename(testFile))) &&
        identical(snapshotText(paths), "new test\n"))
      stop("simulated final publication failure")
    value
  })
  expect_error(publish(), "simulated final publication failure")
  expect_identical(hashes(), old)

  fail <- FALSE
  publish()
  expect_identical(snapshotText(testFile), "new test\n")
  expect_identical(snapshotText(copy), "new fixture\n")
  expect_identical(snapshotText(file.path(snapDir, "change.new.html")), "changed\n")
  expect_false(file.exists(file.path(snapDir, "match.new.html")))
  expect_identical(snapshotText(file.path(snapDir, "first.html")), "first\n")
})

test_that("generation can copy read-only inputs and preserves read-only reviewed files", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  dir.create(testDir, recursive = TRUE)
  fixture <- file.path(module, "case.jasp")
  testFile <- file.path(testDir, "test-other-case.R")
  copy <- file.path(testDir, "jaspfiles", "other", "case.jasp")
  ref <- file.path(testDir, "_snaps", "other-case", "summary.html")
  writeSnapshotText("fixture\n", fixture)
  withr::defer(Sys.chmod(c(fixture, testFile, ref), "0644"))
  expect_true(Sys.chmod(fixture, "0444"))
  publish <- function(content) publishGeneratedTest(testFile, fixture, copy, content,
    list(list(node = "summary", type = "html", name = "summary")))
  publish("old test")
  expect_identical(snapshotText(copy), "fixture\n")
  expect_false(any(grepl("[.]jaspTools-", list.files(testDir, all.files = TRUE, recursive = TRUE, include.dirs = TRUE))))

  expect_true(all(Sys.chmod(c(testFile, ref), "0444")))
  publish("old test")
  expect_error(publish("new test"), "read-only file .*test-other-case[.]R")
  expect_identical(snapshotText(testFile), "old test\n")
  expect_identical(snapshotText(ref), "summary\n")
  expect_true(all(bitwAnd(file.mode(c(fixture, testFile, ref)), as.octmode("222")) == 0L))
  expect_false(any(grepl("[.]jaspTools-", list.files(testDir, all.files = TRUE, recursive = TRUE, include.dirs = TRUE))))
})
