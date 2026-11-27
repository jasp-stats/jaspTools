test_that("HTML snapshots stage first runs, keep approved markup and clear resolved changes", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  dir.create(testDir, recursive = TRUE)
  file <- file.path(testDir, "test-notice.R")
  writeTest <- function(text) writeLines(c('testthat::test_that("notice", {',
    paste0('  jaspTools::expect_snapshot_html(', deparse(text), ', "notice")'), '})'), file)
  run <- function() as.data.frame(testthat::test_dir(testDir, reporter = "silent", stop_on_failure = FALSE))
  baseline <- file.path(testDir, "_snaps", "notice", "notice.html")
  candidate <- snapshotCandidatePath(baseline)

  writeTest("<p>Ready</p>")
  expect_equal(run()$failed, 1L)
  expect_false(file.exists(baseline))
  expect_identical(snapshotText(candidate), "<p>Ready</p>\n")
  expect_true(file.rename(candidate, baseline))
  expect_equal(run()$passed, 1L)
  writeTest("<p>Changed</p>")
  expect_equal(run()$failed, 1L)
  expect_identical(snapshotText(baseline), "<p>Ready</p>\n")
  writeTest("<p>Ready</p>")
  expect_equal(run()$passed, 1L)
  expect_false(file.exists(candidate))
})

test_that("HTML snapshots retain visible text, title, presentation and report markup", {
  html <- snapshotHtml(list(title = 'Size & shape', class = "message", elementType = "p",
                            maxWidth = "10cm", text = "x<br>y", rawtext = "x\ny"))
  expect_match(html, "Size &amp; shape", fixed = TRUE)
  expect_match(html, 'class="message"', fixed = TRUE)
  expect_match(html, 'style="max-width:10cm"', fixed = TRUE)
  expect_match(html, "x<br>y", fixed = TRUE)
  expect_identical(snapshotHtml(list(topHtml = "<header/>", html = "<article/>")), "<header/>\n<article/>")
  expect_error(snapshotHtml(NULL), "HTML node")
  expect_error(snapshotFilename("../elsewhere", "html"), "portable")
})

test_that("runners retain accepted snapshots without per-test setup", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  root <- file.path(testDir, "_snaps")
  refs <- file.path(root, c("Example/one.html", "Example/model.table.json",
    "Example/plot.svg", "Example/reference_plotobject/plot.rds", "Retired/old.html"))
  for (path in refs) writeSnapshotText("original\n", path)
  markdown <- file.path(root, "Example.md")
  writeLines(c("# Retired case", "", "    Output", "      1"), markdown)
  hashes <- tools::md5sum(c(refs, markdown))
  originalCleanup <- utils::getFromNamespace("snapshot_cleanup", "testthat")
  script <- file.path(testDir, "test-Example.R")
  log <- file.path(module, "results.json")
  local_mocked_bindings(getModulePathsForTesting = function() module,
                        getModulePathFromRFunction = function(name) module)
  withr::local_envvar(c(CI = "false", TESTTHAT_PARALLEL = "true"))
  withr::local_options(list(testthat.default_reporter = "silent"))

  writeLines('testthat::test_that("sibling", testthat::expect_true(TRUE))',
             file.path(testDir, "test-Sibling.R"))
  writeLines('testthat::test_that("passes", testthat::expect_true(TRUE))', script)
  capture.output(status <- testAll())
  expect_equal(status, 0)
  expect_identical(tools::md5sum(names(hashes)), hashes)

  writeLines('testthat::test_that("skipped", testthat::skip("unavailable"))', script)
  capture.output(testAnalysis("Example"))
  expect_identical(tools::md5sum(names(hashes)), hashes)

  writeLines('stop("analysis failed before first expectation")', script)
  capture.output(result <- agentTestAll(log))
  expect_equal(result$status, 1L)
  expect_identical(tools::md5sum(names(hashes)), hashes)

  writeLines(c('testthat::test_that("changed", {',
    '  jaspTools::expect_snapshot_html("changed", "one")', '})'), script)
  capture.output(result <- agentTestAnalysis("Example", log))
  expect_equal(result$status, 1L)
  expect_identical(snapshotText(snapshotCandidatePath(refs[1L])), "changed\n")
  expect_identical(tools::md5sum(names(hashes)), hashes)
  expect_identical(utils::getFromNamespace("snapshot_cleanup", "testthat"), originalCleanup)

  # A module can select the same policy when calling serial testthat directly.
  withr::local_envvar(c(TESTTHAT_PARALLEL = "false"))
  writeLines("jaspTools::preserveSnapshotReferences()", file.path(testDir, "setup-snapshots.R"))
  writeLines(c('testthat::test_that("duplicate", {',
    '  jaspTools::expect_snapshot_html("original", "one")',
    '  testthat::expect_error(jaspTools::expect_snapshot_html("original", "one"), "used twice")', '})'), script)
  result <- as.data.frame(testthat::test_dir(testDir, reporter = "silent"))
  expect_equal(sum(result$passed), 3L)
  expect_false(file.exists(snapshotCandidatePath(refs[1L])))
  expect_identical(tools::md5sum(names(hashes)), hashes)
  expect_identical(utils::getFromNamespace("snapshot_cleanup", "testthat"), originalCleanup)

  # Native inline snapshots can still add a case and stage changes to it.
  writeInline <- function(value) writeLines(c('testthat::local_edition(3)',
    'testthat::test_that("New case", {',
    paste0('  testthat::expect_snapshot_output(cat("', value, '"))'), '})'), script)
  readSnaps <- utils::getFromNamespace("read_snaps", "testthat")
  old <- readSnaps(markdown)
  writeInline("first")
  testthat::test_dir(testDir, reporter = "silent")
  accepted <- readSnaps(markdown)
  expect_identical(accepted[names(old)], old)
  expect_true("New case" %in% names(accepted))
  writeInline("second")
  result <- as.data.frame(testthat::test_dir(testDir, reporter = "silent", stop_on_failure = FALSE))
  expect_equal(sum(result$failed), 1L)
  expect_true(file.exists(snapshotCandidatePath(markdown)))
  expect_identical(readSnaps(markdown), accepted)
})

test_that("agent tests link the scoped report and preserve HTML input as JSON data", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  dir.create(testDir, recursive = TRUE)
  for (name in c("Example", "Other")) {
    dir <- file.path(testDir, "_snaps", name)
    stageGeneratedSnapshot("<p>Original</p>", "notice", "html", dir)
    writeLines(c('testthat::test_that("notice", {',
      '  jaspTools::expect_snapshot_html("</script><p>Changed</p>", "notice")', '})'),
      file.path(testDir, paste0("test-", name, ".R")))
  }
  writeSnapshotText("Other candidate", file.path(testDir, "_snaps", "Other", "notice.new.html"))
  testthat::local_mocked_bindings(getModulePathFromRFunction = function(name) module)
  log <- tempfile(tmpdir = module, fileext = ".json")
  invisible(capture.output(result <- agentTestAnalysis("Example", logFile = log)))
  expect_equal(result$status, 1L)
  expect_length(result$snapshotReviews, 1L)
  report <- result$snapshotReviews[[1L]]
  withr::defer(unlink(report))
  expect_identical(jsonlite::read_json(log)[[basename(module)]]$snapshotReview, report)
  html <- snapshotText(report)
  expect_false(grepl("</script><p>Changed", html, fixed = TRUE))
  match <- regexec('<script type="application/json" id="review-data">([^<]+)</script>', html)
  manifest <- jsonlite::fromJSON(regmatches(html, match)[[1L]][2L], simplifyVector = FALSE)
  expect_identical(length(manifest$items), 1L)
  expect_identical(manifest$items[[1L]]$id, "Example/notice.html")
  expect_identical(manifest$items[[1L]]$new, "</script><p>Changed</p>\n")
})
