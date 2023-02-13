#' Compares JASP tables in unit tests.
#'
#' This function compares a stored list of table data, to the table that is created when the tests are run.
#'
#' @param test The table object.
#' @param ref The reference table list created previously by applying \code{makeTestTable} to a JASP table returned from \code{runAnalysis}.
#' @param label Used to customise failure messages.
#'
#' @details By default, numbers are compared using `signif(round(x, digits = 4), digits = 4)`. A custom function to use for rounding numeric values can be passed by setting
#' `options("jaspRoundToPrecision")`. This function should return *numbers* rounded to the desired accuracy.
#'
#' @examples
#'
#' options <- analysisOptions("BinomialTest")
#' options$variables <- "contBinom"
#' options$testValue <- 0.6
#' options$hypothesis <- "greaterThanTestValue"
#' options$confidenceInterval <- TRUE
#' options$VovkSellkeMPR <- TRUE
#' results <- runAnalysis("BinomialTest", "test.csv", options)
#' table <- results[["results"]][["binomialTable"]][["data"]]
#' expect_equal_tables(table,
#'                     list("TRUE", 1, 58, 0, 0.492841460660175, 0.696739870156555, 0.58, 100, 1, "contBinom",
#'                          "FALSE", 1, 42, 1, 0.336479745077558, 0.999903917924738, 0.42, 100, 1, "contBinom"))
#'
#' # illustration of custom tolerance
#' expect_equal_tables(list(0.50001), list(0.50000))
#' try(expect_equal_tables(list(0.51), list(0.50)))
#'
#' # increase tolerance
#' options("jaspRoundToPrecision" = function(x) signif(round(x, digits = 1), digits = 1))
#' expect_equal_tables(list(0.51), list(0.50))
#'
#' # reset tolerance to default
#' options("jaspRoundToPrecision" = NULL) # reset default
#' try(expect_equal_tables(list(0.51), list(0.50)))
#'
#' @export expect_equal_tables
expect_equal_tables <- function(test, ref, label=NULL) {
  if (length(test) == 0) {
    expect(FALSE, getEmptyTestMsg("expect_equal_tables()"))
    return()
  }

  nRows <- length(test)
  nCols <- length(test[[1]])
  cellNames <- names(unlist(test))
  test <- collapseTestTable(test)

  if (is.null(label))
    label <- "New table"

  if (length(test) == length(ref)) {
    mismatches <- getMismatchesEqualSizeTables(test, ref, nRows, nCols, cellNames)
    expect(mismatches == "", paste0(label, " is not equal to old table:\n", mismatches))
  } else {
    missingValues <- getMissingValuesDiffSizeTables(test, ref, cellNames)

    cellDiff <- abs(length(ref) - length(test))
    type <- "cells (possibly footnotes, .isNewGroup, etc?)"
    if (cellDiff %% nRows == 0) type <- "columns"
    else if (cellDiff %% nCols == 0) type <- "rows"

    if (length(test) > length(ref))
      reason <- paste0("likely reason: there are one or more new ", type , " in the new table that were not in the old table.\n",
                       "New table values that are not matched: ", missingValues)
    else
      reason <- paste0("likely reason: one or more ", type, " are no longer in the new table that were in the old table.\n",
                       "Old table values that are not matched: ", missingValues)

    testthat::expect(FALSE, paste0(label, " (# cells: ", length(test), ") and old table (# cells: ", length(ref), ") are not of equal length, ", reason))
  }
}

roundToPrecision <- function(x) {
  if (is.numeric(x))  {

    userRounder <- getOption("jaspRoundToPrecision", default = NULL)
    if (is.null(userRounder)) {

      return(signif(round(x, digits = 4), digits = 4))

    } else {

      if (!is.function(userRounder))
        stop("options(\"jaspRoundToPrecision\") should be a function!)", domain = NA)

      result <- try(userRounder(x))
      if (inherits(result, "try-error"))
        stop("options(\"jaspRoundToPrecision\")(x) failed with error message:\n", result, domain = NA)
      if (length(result) != length(x))
        stop("length(options(\"jaspRoundToPrecision\")(x)) != length(x)")
      if (!is.numeric(result))
        stop("options(\"jaspRoundToPrecision\")(x)) is not numeric")

      return(result)
    }
  } else x
}

isUnicodeMismatch <- function(mismatch) {
  grepl("<unicode>", mismatch, fixed=TRUE)
}

excludeUnicodeMismatches <- function(mismatches) {
  indicesToRemove <- NULL
  for (i in seq_along(mismatches))
    if (isUnicodeMismatch(mismatches[i]))
      indicesToRemove <- c(indicesToRemove, i)

  if (!is.null(indicesToRemove))
    return(mismatches[-indicesToRemove])

  return(mismatches)
}

getMismatchesEqualSizeTables <- function(test, ref, nRows, nCols, cellNames) {
  testVec <- tableListToAnnotatedCharacterVector(test, cellNames)
  refVec <- tableListToAnnotatedCharacterVector(ref)

  tableMismatches <- character(0)
  for (row in 1:nRows) {
    unmatchedRowVals <- character(0)

    cellRange <- (1 + (row - 1) * nCols):(row * nCols)
    lookupRow <- refVec[cellRange]

    for (cell in cellRange) {
      indicesMatch <- which(lookupRow %in% testVec[cell])
      if (length(indicesMatch) > 0)
        lookupRow <- lookupRow[-min(indicesMatch)]
      else {
        if (isUnicodeMismatch(names(testVec)[cell]))
          next

        unmatchedRowVals <- c(unmatchedRowVals, paste0("`", names(testVec)[cell], "` (col: `", attr(testVec, "cellNames")[cell], "`)"))

      }
    }

    if (length(unmatchedRowVals) > 0) {
      haveMultipleVals <- length(unmatchedRowVals) > 1
      tableMismatches <- c(
        tableMismatches,
        paste0("*** Row ", row, " ***"),
        paste0("  Table ", ifelse(haveMultipleVals, "cells", "cell"), " that changed:"),
        paste0("    - ", paste(unmatchedRowVals, collapse = "\n    - ")),
        paste0("  Original ", ifelse(haveMultipleVals, "values that were", "value that was"), " expected:"),
        paste0("    - `", paste(excludeUnicodeMismatches(names(lookupRow)), collapse = "`, `"), "`")
      )
    }

  }

  return(paste(tableMismatches, collapse="\n"))
}

getMissingValuesDiffSizeTables <- function(test, ref, cellNames) {
  testVec <- tableListToAnnotatedCharacterVector(test, cellNames)
  refVec <- tableListToAnnotatedCharacterVector(ref)

  if (length(testVec) > length(refVec)) {
    searchFor <- testVec
    searchIn <- refVec
  } else {
    searchFor <- refVec
    searchIn <- testVec
  }

  missingValues <- character(0)
  for (i in seq_along(searchFor)) {

    if (!searchFor[i] %in% searchIn) {

      missingValue <- paste0("`", names(searchFor)[i], "`")
      col <- attr(searchFor, "cellNames")[i]
      if (!is.null(col))
        missingValue <- paste0(missingValue, " (col `", col, "`)")
      missingValues <- c(missingValues, missingValue)

    } else {
      searchIn <- searchIn[-min(which(searchIn %in% searchFor[i]))]
    }

  }

  return(paste(missingValues, collapse=", "))
}

tableListToAnnotatedCharacterVector <- function(tableList, cellNames=NULL) {
    fullValues <- unlist(tableList)
    tableVec <- as.character(unlist(lapply(tableList, roundToPrecision)))
    names(tableVec) <- fullValues
    attr(tableVec, "cellNames") <- cellNames

    return(tableVec)
}
