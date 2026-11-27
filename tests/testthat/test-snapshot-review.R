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

test_that("interrupted analyses preserve reviewed files and duplicate snapshot names still fail", {
  module <- withr::local_tempdir()
  testDir <- file.path(module, "tests", "testthat")
  snapDir <- file.path(testDir, "_snaps", "interrupted")
  refs <- c("one.html", "estimate.table.json", "plot.new.svg", "reference_plotobject/plot.rds")
  for (file in refs) writeSnapshotText("original\n", file.path(snapDir, file))
  script <- file.path(testDir, "test-interrupted.R")
  run <- function() as.data.frame(testthat::test_dir(testDir, reporter = "silent", stop_on_failure = FALSE))
  writeLines(c('testthat::test_that("replay", {',
    '  jaspTools::preserveSnapshotReferences()', '  stop("analysis failed")', '})'), script)
  expect_true(run()$error)
  expect_true(all(file.exists(file.path(snapDir, refs))))
  writeLines(c('testthat::test_that("duplicate", {', '  jaspTools::preserveSnapshotReferences()',
    '  jaspTools::expect_snapshot_html("original", "one")',
    '  testthat::expect_error(jaspTools::expect_snapshot_html("original", "one"), "used twice")', '})'), script)
  result <- run()
  expect_false(result$error)
  expect_equal(result$failed, 0L)
  expect_equal(result$passed, 2L)
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
