# make documentation (R analyses and on github)
# experiment with runLast = TRUE

testAnalysis <- function(analysis) {
  analysis <- .validateAnalysis(analysis)
  root <- .getPkgOption("tests.dir")
  file <- file.path(root, paste0("test-", analysis, ".R"))
  testthat::test_file(file)
}

testAll <- function() {
  testDir <- .getPkgOption("tests.dir")
  testthat::test_dir(testDir)
}

inspectTestPlots <- function(analysis = NULL) {
  if (! is.null(analysis)) {
    analysis <- .validateAnalysis(analysis)
    analysis <- paste0("^", analysis, "$")
  }
  testDir <- .getPkgOption("tests.dir")
  on.exit(unloadNamespace("SomePkg")) # unload fake pkg needed to run vdiffr
  vdiffr::manage_cases(testDir, analysis)
}

makeTestTable <- function(rows) {
  x <- collapseTable(rows)
  result <- ""
  nChars <- 0
  for (i in 1:length(x)) {
    element <- x[[i]]
    if (! is.numeric(element)) {
      element <- paste0("\"", element, "\"")
    }

    nOldChars <- nchar(result)
    if (nchar(result) == 0) {
      result <- element
    } else {
      result <- paste(result, element, sep=", ")
    }

    nChars <- nChars + nchar(result) - nOldChars
    if (nChars >= 60) {
      result <- paste0(result, "\n")
      nChars <- 0
    }
  }

  result <- gsub("\n,", ",\n", result, fixed=TRUE)
  if (endsWith(result, "\n")) {
    result <- substr(result, 1, nchar(result)-1)
  }
  result <- paste0("list(", result, ")")

  cat(result)
}
