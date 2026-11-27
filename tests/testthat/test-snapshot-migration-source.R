planMigrationText <- function(code) {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path))
  writeLines(enc2utf8(code), path, useBytes = TRUE)
  snapshotMigrationSource(path)
}

test_that("the planner returns BOM-free lines for byte-preserving publication", {
  fixture <- c(paste0("\uFEFF", "# provenance"), 'test_that("bom", {',
               '  expect_equal_tables(node$data, list(1))', '})')
  plan <- planMigrationText(fixture)
  expect_identical(plan$sites$status, "ready")
  expect_identical(plan$text[1L], "# provenance")
  expect_identical(plan$capture[1L], "# provenance")
})

test_that("migration edits parsed calls and preserves text, test cases and existing checks", {
  fixture <- c(
    "# Déjà verified", 'context("original context")',
    'testthat::test_that("first", {',
    '\trows <- results[["résumé"]][["data"]] # provenance',
    '\tjaspTools::expect_equal_tables("ref" = list(-2, "3"), "test" = rows)',
    '})', 'test_that("second", {',
    '  expect_equal_plots(fig, "original")',
    '  expect_snapshot_html(note, "note")',
    '  custom(rows) # outside the first case',
    '  expect_equal_tables(getTable("résumé")$data, list(-2, "3")) # tail',
    '})', "# expect_equal_tables(fake, ignored)",
    'quote("expect_equal_tables")', "")
  plan <- planMigrationText(fixture)
  expect_identical(plan$sites$status, c("ready", "ready"))
  expect_identical(plan$sites$line, c(5L, 11L))
  expect_true(all(fixture[c(1:3, 6:10, 12:15)] %in% plan$text))
  expect_true(any(grepl('\trows <- results[["résumé"]] # provenance', plan$text, fixed = TRUE)))
  expect_true(any(grepl('expect_snapshot_table(getTable("résumé"), ', plan$text, fixed = TRUE)))
  expect_true(any(endsWith(plan$text, ") # tail")))
  expect_identical(plan$text[2L], "jaspTools::preserveSnapshotReferences()")
  expect_silent(parse(text = plan$capture))
  again <- planMigrationText(plan$text)
  expect_identical(again$text, plan$text)
  expect_identical(nrow(again$sites), 0L)
})

test_that("capture verifies the original rows and records the same producer evaluation", {
  fixture <- c('test_that("getters", {',
    '  rows <- getTable()$data',
    '  ref <- list(2)',
    '  expect_equal_tables(ref = ref, test = rows)',
    '  stopifnot(identical(rows, ref))',
    '  expect_equal_tables(getTable()[["data"]], list(3), "other table")',
    '})')
  plan <- planMigrationText(fixture)
  log <- character()
  count <- 1L
  test_that <- function(desc, code) force(code)
  getTable <- function() {
    count <<- count + 1L
    list(title = as.character(count), schema = list(), data = list(as.numeric(count)))
  }
  expect_equal_tables <- jaspTools::expect_equal_tables
  .jaspToolsCaptureTable <- function(node, name) {
    stopifnot(identical(node$title, as.character(count)), !is.null(node$schema))
    log <<- c(log, name)
  }
  # The first expression registers references; execute the test case itself.
  eval(parse(text = plan$capture)[[2L]])
  expect_identical(log, plan$sites$name)
  expect_identical(count, 3L)
  expect_true('  rows <- snapshotTableNode$data' %in% plan$text)
  expect_true('  stopifnot(identical(rows, ref))' %in% plan$text)
})

test_that("final private row aliases fold after analysis setup and remain available for deferred reads", {
  setup <- c('test_that("rank biserial", {',
    '  opts <- initTTestOptions("TTestOneSample")',
    '  opts$dependent <- "score"',
    '  results <- jaspTools::runAnalysis("TTestOneSample", "test.csv", opts)')
  checks <- c('  resultTable <- results$results$ttest$data',
              '  expect_equal_tables("test" = resultTable, "ref" = list(1))', '})')
  plan <- planMigrationText(c(setup, checks))
  expect_identical(plan$sites$status, "ready")
  expect_true('  resultTable <- results$results$ttest' %in% plan$text)
  expect_false(any(grepl("snapshotTableNode", plan$text, fixed = TRUE)))
  for (read in c('  opts$previous <- resultTable', '  withr::defer(check(get("resultTable")))')) {
    plan <- planMigrationText(c(setup, read, checks))
    expect_true(read %in% plan$text)
    expect_true('  resultTable <- snapshotTableNode$data' %in% plan$text)
  }
})

test_that("generated table checks reuse their local variable across tables and plots", {
  fixture <- c('# This test file was auto-generated from a JASP example file.',
    'test_that("Example results match", {',
    '  jaspFile <- testthat::test_path("jaspfiles", "example.jasp")',
    '  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)',
    '  results <- jaspTools::runAnalysis("Example", dataset, list())',
    '  table <- results$first$data',
    '  jaspTools::expect_equal_tables(table, list(1))',
    '  plotName <- results$plot[["data"]]',
    '  testPlot <- results$figures[[plotName]]$obj',
    '  jaspTools::expect_equal_plots(testPlot, "plot")',
    '  table <- results$second[["data"]] # second result',
    '  jaspTools::expect_equal_tables(table, list(2))', '})')
  plan <- planMigrationText(fixture)
  expect_identical(plan$sites$name, c("first", "second"))
  expect_true(all(c('  table <- results$first', '  table <- results$second # second result',
                    '  jaspTools::expect_snapshot_table(table, "first")',
                    '  jaspTools::expect_snapshot_table(table, "second")') %in% plan$text))
  expect_false(any(grepl("snapshotTableNode", plan$text, fixed = TRUE)))
  expect_true(all(fixture[8:10] %in% plan$text))
  expect_true(all(trimws(fixture[c(7L, 12L)]) %in% trimws(plan$capture)))
  expect_true('  table <- (snapshotTableNode <- results$first)$data' %in% plan$capture)

  custom <- list(append(fixture, '  expect_identical(table, list(1))', after = 7L),
                 append(fixture, '  withr::defer(check(get("table")))', after = 5L),
                 append(fixture, '  jaspTools::expect_equal_tables(table, list(1))', after = 7L),
                 append(fixture, c('  runAnalysis <- get', '  results <- runAnalysis("table")'), after = 7L),
                 fixture[-1L])
  for (code in custom) {
    plan <- planMigrationText(code)
    expect_true('  table <- snapshotTableNode$data' %in% plan$text)
  }
})

test_that("capture respects a module's custom legacy expectation or reference constructor", {
  plan <- planMigrationText(c('test_that("overrides", {',
    '  expect_equal_tables(node$data, list(1))', '})'))
  code <- parse(text = plan$capture)[[2L]]
  test_that <- function(desc, code) force(code)
  node <- list(data = list(1), schema = list())
  .jaspToolsCaptureTable <- function(...) stop("reached capture")
  expect_equal_tables <- function(...) stop("reached custom expectation")
  expect_error(eval(code), "Custom expect_equal_tables()", fixed = TRUE)
  expect_equal_tables <- jaspTools::expect_equal_tables
  list <- function(...) stop("reached custom constructor")
  expect_error(eval(code), "Custom list()", fixed = TRUE)
})

test_that("private fixed reference declarations are removed from the rewritten test", {
  fixture <- c('test_that("private baseline", {',
    '  # published reference values',
    '  ref <- list(2,',
    '              3)',
    '  rows <- node$data',
    '  expect_equal_tables(rows, ref)',
    '})')
  plan <- planMigrationText(fixture)
  expect_identical(plan$sites$status, "ready")
  expect_false(any(grepl("ref <-", plan$text, fixed = TRUE)))
  expect_true(all(fixture[3:4] %in% plan$capture))
  expect_true(all(c(fixture[2L], "  rows <- node") %in% plan$text))
  expect_silent(parse(text = plan$text))

  fixture[3L] <- '  ref <- list(2, # provenance for this value'
  withNote <- planMigrationText(fixture)
  expect_identical(withNote$sites$status, "manual")
  expect_identical(withNote$text, fixture)
})

test_that("manual comparisons are retained beside independently migratable checks", {
  manual <- c(
    '  rows <- node$data',
    '  rows[[1]] <- 5',
    '  expect_equal_tables(rows, list(5))',
    '  expect_equal_tables(node$data, modelReference(results))',
    '  expect_equal_tables(node$footnotes, list("note"))',
    '  for (j in 1:2) expect_equal_tables(node$data, list(5))',
    '  expect_equal_tables(node$data, list(5 # explanation of the reference',
    '  ))',
    '  expect_equal_tables(node$data, list(5), label = checkedLabel())',
    '  if (expect_equal_tables(node$data, list(5))) doSomething()',
    '  anotherPackage::expect_equal_tables(rows, 5)')
  fixture <- c('test_that("mixed", {', manual,
               '  if (enabled) { expect_equal_tables(node$data, list(5)) }', '})')
  plan <- planMigrationText(fixture)
  expect_true(all(manual %in% plan$text))
  expect_identical(plan$sites$status, c(rep("manual", 7L), "ready"))
  expect_true(all(nzchar(plan$sites$reason[plan$sites$status == "manual"])))
  expect_silent(parse(text = plan$text))
  expect_silent(parse(text = plan$capture))
})

test_that("snapshot names distinguish repeated cases and coexist with existing names", {
  block <- c('test_that("repeated title", {',
             '  expect_equal_tables(table$data, list(0))', '})')
  one <- planMigrationText(block)
  existing <- sprintf('  jaspTools::expect_snapshot_table(name = "%s", node = other)', one$sites$name)
  plan <- planMigrationText(c('test_that("existing", {', existing, '})', block, block))
  expect_identical(plan$sites$name, paste0(one$sites$name, c("-2", "-3")))
  expect_true(existing %in% plan$text)
  unrelated <- planMigrationText(c("# new header", "", block))
  expect_identical(unrelated$sites$name, one$sites$name)
})

test_that("migration names identify the analysis, test scenario or specific result", {
  fixture <- c('context("One Sample TTest -- Verification project")',
    'test_that("Main table results match R, SPSS, SAS and MiniTab", {',
    '  expect_equal_tables(results$results$ttest$data, list(0))', '})',
    'test_that("Main table results match for Wilcoxon signed rank", {',
    '  expect_equal_tables(results$results$ttest$data, list(0))', '})',
    'test_that("Posterior Estimates per Model table results match", {',
    '  expect_equal_tables(results$bmaTable$data, list(0))', '})',
    'test_that("WaapWls (analysis 1) results match", {',
    '  expect_equal_tables(results$modelSummaryContainer_pooledEstimatesTable$data, list(0))',
    '  expect_equal_tables(results$modelSummaryContainer_testsTable$data, list(0))',
    '  expect_equal_tables(results$extra$data, list(0), label = "Sensitivity to prior width")', '})')
  names <- c("one-sample-t-test", "wilcoxon-signed-rank", "posterior-estimates-per-model",
             "analysis-1-pooled-estimates", "analysis-1-tests", "sensitivity-to-prior-width")
  plan <- planMigrationText(fixture)
  expect_identical(plan$sites$name, names)
  expect_identical(planMigrationText(plan$text)$text, plan$text)
})

test_that("multiple outputs preserve the meaningful part of the description and key", {
  fixture <- c('test_that("Cross-validation", {',
    '  expect_equal_tables(results$weights$data, list(0))',
    '  expect_equal_tables(results$errors$data, list(0))', '})',
    'test_that("I\u00b2 table results match", {',
    '  expect_equal_tables(results$summaryI2Table$data, list(0))', '})',
    'test_that("Analysis (analysis 2) results match", {',
    '  expect_equal_tables(results$randomEstimatesContainer_containerS_tableS$data, list(0))', '})')
  expect_identical(planMigrationText(fixture)$sites$name, c("cross-validation-weights", "cross-validation-errors",
                                                          "i2", "analysis-2-random-estimates-s"))
})
