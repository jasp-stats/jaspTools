#' Compares JASP plots in unit tests.
#'
#' This function compares a stored .svg of a plot, to the plot that is created when the tests are run.
#' If no .svg exists yet then you must first run \code{manageTestPlots}.
#'
#'
#' @param test The plot object you wish to test (does not work well for non-ggplot2 objects).
#' @param name The name of the reference plot (a .svg stored in /tests/figs).
#' @param dir The directory in tests/figs where the .svg is stored (commonly the name of the analysis).
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
expect_equal_plots <- function(test, name, dir) {
  if (length(test) == 0) {
    expect(FALSE, getEmptyTestMsg("plot"))
    return()
  }

  skip_if_grob(test)
  skip_if_recordedPlot(test)

  if (inherits(test, "jaspGraphsPlot")) {
    subplots <- test$subplots

    for (i in seq_along(subplots))
      vdiffr::expect_doppelganger(paste(dir, name, "subplot", i, sep="-"), subplots[[i]], path=dir)

  } else {
    if (inherits(test, "qgraph")) {
      qq <- test
      test <- function() plot(qq)
    }
    suppressWarnings(vdiffr::expect_doppelganger(paste(dir, name, sep="-"), test, path=dir))
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

getEmptyTestMsg <- function(element) {
  error <- getErrorMsgFromLastResults()
  if (!is.null(error[["type"]])) {
    if (error[["type"]] == "validationError" || error[["type"]] == "fatalError")
      msg <- paste0("Tried retrieving ", element, " from results, but last run of jaspTools exited with a ", error[["type"]], ":\n\n", error[["message"]])
    else if (error[["type"]] == "localError")
      msg <- paste0("The new ", element, " has no data. Please check the validity of your unit test; found the following local errors in the results:\n\n", error[["message"]])
  } else {
    msg <- paste0("The new ", element, " has no data. Please check the validity of your unit test; is the index path to the plot specified correctly?")
  }

  return(msg)
}
