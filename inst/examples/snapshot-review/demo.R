# Source this file, then call snapshotReviewExample() from an R session.
snapshotReviewExample <- function(changed = TRUE, open = interactive()) {
  template <- system.file("examples", "snapshot-review", package = "jaspTools",
                          mustWork = TRUE)
  modulePath <- tempfile("jasp-snapshot-review-")
  dir.create(modulePath)
  if (!file.copy(file.path(template, "tests"), modulePath, recursive = TRUE))
    stop("Could not copy the example tests to ", modulePath)

  testPath <- file.path(modulePath, "tests", "testthat")
  if (changed)
    file.create(file.path(testPath, "changed-output"))

  # vdiffr normally skips visual comparisons outside interactive/developer runs.
  withr::local_envvar(c(NOT_CRAN = "true"))
  if (changed)
    message("The example deliberately changes four snapshots. Expect four test failures.")
  testthat::test_dir(testPath, reporter = "summary", stop_on_failure = !changed)
  report <- jaspTools::reviewSnapshots(modulePath, output = file.path(modulePath, "review.html"),
                                       open = open, includeUnchanged = !changed)
  message("Example module: ", modulePath)
  invisible(list(modulePath = modulePath, report = report))
}
