test_that("the packaged example reproduces clean references and four independent candidates", {
  template <- system.file("examples", "snapshot-review", package = "jaspTools", mustWork = TRUE)
  example <- new.env()
  sys.source(file.path(template, "demo.R"), envir = example)
  capture.output(suppressMessages(clean <- example$snapshotReviewExample(changed = FALSE, open = FALSE)))
  capture.output(suppressMessages(changed <- example$snapshotReviewExample(open = FALSE)))

  expect_true(file.exists(clean$report) && file.exists(changed$report))
  expect_false(identical(clean$modulePath, changed$modulePath))
  sources <- c("analysis-summary.html", "group-descriptives.table.json", "group-means.svg",
               "independent-samples.table.json")
  snapshots <- file.path("tests", "testthat", "_snaps", "cloak")
  expect_setequal(list.files(file.path(template, snapshots)), sources)
  expect_setequal(list.files(file.path(clean$modulePath, snapshots)), sources)
  candidates <- sub("(\\.[^.]+)$", ".new\\1", sources)
  expect_setequal(list.files(file.path(changed$modulePath, snapshots)), c(sources, candidates))
})
