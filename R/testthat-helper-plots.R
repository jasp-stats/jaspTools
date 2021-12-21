#' Compares JASP plots in unit tests.
#'
#' This function compares a stored .svg of a plot, to the plot that is created when the tests are run.
#' If no .svg exists yet then you must first run \code{manageTestPlots}.
#'
#'
#' @param test The plot object you wish to test (does not work well for non-ggplot2 objects).
#' @param name The name of the reference plot (a .svg stored in /tests/testthat/_snaps).
#' @param dir `r lifecycle::badge('deprecated')`
#'
#' @examples
#'
#' options <- analysisOptions("BinomialTest")
#' options$variables <- "contBinom"
#' options$descriptivesPlots <- TRUE
#' results <- runAnalysis("BinomialTest", "test.csv", options)

#' testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
#' expect_equal_plots(testPlot, "descriptives-1", dir = "BinomialTest")
#'
#' @export expect_equal_plots
expect_equal_plots <- function(test, name, dir = lifecycle::deprecated()) {
  if (!missing(dir))
    lifecycle::deprecate_warn(
      when = "1.5.0",
      what = "expect_equal_plots(dir)",
    )

  if (length(test) == 0) {
    expect(FALSE, getEmptyTestMsg("expect_equal_plots()"))
    return()
  }

  skip_if_grob(test)
  skip_if_recordedPlot(test)

  if (inherits(test, "jaspGraphsPlot")) {
    subplots <- test$subplots

    for (i in seq_along(subplots))
      vdiffr::expect_doppelganger(paste(name, "subplot", i, sep = "-"), subplots[[i]])

  } else {
    if (inherits(test, "qgraph")) {
      qq <- test
      test <- function() plot(qq)
    }
    suppressWarnings(vdiffr::expect_doppelganger(name, test))
  }
}

skip_if_grob <- function(test) {
  if (inherits(test, "grob"))
    skip("Cannot reliably test matrix plots (they fail Windows <-> OSX)")
}

skip_if_recordedPlot <- function(test) {
  if (inherits(test, "recordedplot"))
    skip("Recorded plots are skipped until the scaling of these plots is fixed")
}

getEmptyTestMsg <- function(expectationFn) {
  error <- getErrorMsgFromLastResults()
  if (!is.null(error[["type"]])) {
    if (error[["type"]] == "validationError" || error[["type"]] == "fatalError")
      msg <- paste0("The `test` argument provided to `", expectationFn, "` is empty. Likely reason: the last run of jaspTools exited with a ", error[["type"]], ":\n\n", error[["message"]])
    else if (error[["type"]] == "localError")
      msg <- paste0("The `test` argument provided to `", expectationFn,"` is empty. Likely reasons: (1) the path to the results in the unit test is not correct, or (2) one of the following errors in the results interfered with the test:\n\n", error[["message"]])
  } else {
    msg <- paste0("The `test` argument provided to `", expectationFn,"` is empty. Likely reason: the path to the results in the unit test is not correct.")
  }

  return(msg)
}
