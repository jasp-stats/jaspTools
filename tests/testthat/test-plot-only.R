test_that("plot-only entry points run mixed blocks and preserve untested snapshots", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  snapDir <- file.path(testDir, "_snaps", "Mixed")
  dir.create(snapDir, recursive = TRUE)
  paths <- file.path(snapDir, c("summary.table.json", "summary.table.new.json",
                                "note.html", "note.new.html", "skipped.svg", "skipped.new.svg"))
  for (path in paths) writeLines(basename(path), path)
  markdown <- file.path(dirname(snapDir), c("Mixed.md", "Mixed.new.md"))
  for (path in markdown)
    writeLines(c("# non-plot block", "", "    Output", "      1"), path)
  figure <- file.path(snapDir, "figure.svg")
  vdiffr::write_svg(function() graphics::barplot(1), figure, "figure")
  paths <- c(paths, markdown, figure)
  hashes <- tools::md5sum(paths)

  writeLines(c(
    'test_that("non-plot block", {',
    '  file.create("ran-nonplot")',
    '  stop("onlyPlots ran a table-only test")',
    '})',
    'plotFigure <- function() graphics::barplot(1)',
    'testthat::test_that("mixed block", {',
    '  jaspTools::expect_equal_tables(stop("legacy table ran"), list())',
    '  jaspTools::expect_snapshot_table(stop("table ran"), "summary")',
    '  jaspTools::expect_snapshot_html(stop("HTML ran"), "note")',
    '  jaspTools::expect_equal_plots(plotFigure, "figure")',
    '  file.create("ran-plot")',
    '})'
  ), file.path(testDir, "test-Mixed.R"))
  local_mocked_bindings(getModulePathsForTesting = function() module,
                        getModulePathFromRFunction = function(name) module)
  withr::local_envvar(c(CI = NA, JASP_PLOT_TEST = "original"))
  withr::local_options(list(testthat.default_reporter = "silent", jaspTools.plotSnapshot.gc = FALSE))
  originalTest <- testthat::test_that
  originalTable <- jaspTools::expect_snapshot_table

  runs <- list(function() testAll(onlyPlots = TRUE), function() testAnalysis("Mixed", onlyPlots = TRUE))
  expected <- list(0, NULL)
  for (i in seq_along(runs)) {
    capture.output(result <- runs[[i]]())
    expect_identical(result, expected[[i]])
    expect_true(file.exists(file.path(testDir, "ran-plot")))
    expect_false(file.exists(file.path(testDir, "ran-nonplot")))
    expect_false(file.exists(file.path(snapDir, "figure.new.svg")))
    expect_identical(tools::md5sum(paths), hashes)
    expect_identical(testthat::test_that, originalTest)
    expect_identical(jaspTools::expect_snapshot_table, originalTable)
    expect_identical(Sys.getenv("JASP_PLOT_TEST"), "original")
    unlink(file.path(testDir, "ran-plot"))
  }

  # An ordinary run should compare the HTML again and clear its stale candidate.
  writeLines(c('test_that("HTML", {', '  jaspTools::expect_snapshot_html("<p>full</p>", "note")',
               '})'), file.path(testDir, "test-Mixed.R"))
  writeLines("<p>full</p>", file.path(snapDir, "note.html"))
  capture.output(suppressMessages(status <- testAll()))
  expect_equal(status, 0)
  expect_false(file.exists(file.path(snapDir, "note.new.html")))
})
