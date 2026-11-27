test_that("tolerated values survive review and only an explicit refresh stages them", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  dir.create(testDir, recursive = TRUE)
  file <- file.path(testDir, "test-results.R")
  id <- "results/model.table.json"
  root <- snapshotReviewRoot(module)
  ref <- file.path(root, id)
  candidate <- snapshotCandidatePath(ref)
  table <- list(schema = list(fields = list(
    list(name = "estimate", type = "number"), list(name = "se", type = "number")
  )), data = list(list(estimate = 1.23456789012345, se = 0.123456789012345)))
  stageGeneratedSnapshot(table, "model", "table", dirname(ref))
  originalHash <- snapshotFileHash(ref)
  run <- function(args = "", filePolicy = character()) {
    saveRDS(table, file.path(testDir, "observed.rds"))
    writeLines(c(filePolicy, 'testthat::test_that("model", {',
      '  result <- readRDS(testthat::test_path("observed.rds"))',
      paste0('  jaspTools::expect_snapshot_table(result, "model"', args, ')'), '})'), file)
    preserveSnapshotReferences(environment())
    as.data.frame(testthat::test_dir(testDir, reporter = "silent", stop_on_failure = FALSE))
  }
  accept <- function() {
    item <- snapshotReviewItem(root, id)
    row <- item[c("id", "baselineHash", "candidateHash", "structureBaselineHash", "structureCandidateHash")]
    row$action <- "accept"
    row$comment <- ""
    decision <- file.path(module, "decision.json")
    jsonlite::write_json(list(version = 1L, decisions = list(row)), decision,
                         auto_unbox = TRUE, null = "null")
    applySnapshotReview(decision, module)$status
  }

  oldEstimate <- table$data[[1L]]$estimate
  table$data[[1L]]$estimate <- oldEstimate + 1e-12
  stageGeneratedSnapshot(table, "model", "table", dirname(ref))
  expect_false(file.exists(candidate))
  expect_equal(run()$passed, 1L)
  expect_false(file.exists(candidate))
  expect_identical(snapshotFileHash(ref), originalHash)

  table$data[[1L]]$se <- 0.25
  expect_equal(run()$failed, 1L)
  item <- snapshotReviewItem(root, id)
  expect_identical(vapply(item$diff$changes, `[[`, "", "column"), "se")
  expect_identical(item$new$data[[1L]]$estimate, oldEstimate)
  expect_identical(snapshotFileHash(ref), originalHash)
  expect_identical(accept(), "accepted")
  expect_equal(run()$passed, 1L)

  expect_equal(run(", refresh = TRUE")$failed, 1L)
  expect_identical(jsonlite::read_json(candidate)$data[[1L]]$estimate, table$data[[1L]]$estimate)
  expect_identical(jsonlite::read_json(ref)$data[[1L]]$estimate, oldEstimate)
  expect_equal(run()$passed, 1L)
  expect_false(file.exists(candidate))
  expect_equal(run(", refresh = TRUE")$failed, 1L)
  expect_identical(accept(), "accepted")
  expect_identical(jsonlite::read_json(ref)$data[[1L]]$estimate, table$data[[1L]]$estimate)
  expect_equal(run(", refresh = TRUE")$passed, 1L)

  table$data[[1L]]$se <- table$data[[1L]]$se + 0.01
  expect_equal(run()$failed, 1L)
  filePolicy <- "jaspTools::local_table_snapshot_tolerance(rtol = 0, atol = 0.02)"
  expect_equal(run(filePolicy = filePolicy)$passed, 1L)
  expect_false(file.exists(candidate))
  expect_equal(run(", columns = list(se = list(atol = 0))", filePolicy)$failed, 1L)
  expect_equal(run(", rtol = 0, atol = 0", filePolicy)$failed, 1L)
  expect_equal(run(", columns = list(se = list(atol = 0.02))")$passed, 1L)
  expect_false(file.exists(candidate))
  expect_equal(run()$failed, 1L)

  table$schema$fields <- table$schema$fields[1L]
  table$data[[1L]]$se <- NULL
  expect_equal(run(", columns = list(se = list(atol = 0))")$failed, 1L)
  expect_identical(snapshotReviewItem(root, id)$diff$changes[[1L]]$kind, "column_removed")
  expect_true(run(", columns = list(typo = list(atol = 0))")$error)
  expect_true(file.exists(ref))
  priorHashes <- tools::md5sum(c(ref, candidate))
  expect_true(run(filePolicy = "jaspTools::local_table_snapshot_tolerance(atol = -1)")$error)
  expect_identical(tools::md5sum(names(priorHashes)), priorHashes)
})

test_that("regeneration protects file and table tolerance overrides before staging", {
  module <- withr::local_tempdir()
  dir <- file.path(module, "tests", "testthat")
  file <- file.path(dir, "test-other-Example.R")
  ref <- file.path(dir, "_snaps", "other-Example", "model.table.json")
  writeSnapshotText("approved", ref)
  jaspFile <- file.path(module, "Example.jasp")
  local_mocked_bindings(readAnalysisResultsFromJaspFile = function(...) list(), .package = "jaspSyntax")
  local_mocked_bindings(hasModuleDirs = function() stop("Analysis setup should not run"))
  for (custom in c(
    "jaspTools::local_table_snapshot_tolerance(atol = customLimit)",
    "jaspTools::expect_snapshot_table(node, 'model', columns = list(se = list(atol = customLimit)))",
    "expect_snapshot_table(node, 'model', 0, 0)",
    "expect_snapshot_table(node, 'model', refresh = TRUE)"
  )) {
    writeLines(custom, file)
    expect_error(makeTestsFromSingleJASPFile(jaspFile, module, "other", overwrite = TRUE),
                 "would overwrite custom table snapshot settings")
    expect_identical(readLines(file), custom)
    expect_identical(snapshotText(ref), "approved")
    expect_false(file.exists(snapshotCandidatePath(ref)))
  }
})
