makeUnitTestsFromResults <- function(results, name, dataset, options) {
  if (!is.list(results) || is.null(names(results)) || results$status == "error")
    stop("Can't make unit test from results: not a results list")

  tests <- getTests(results$results)
  if (length(tests) == 0)
    stop("Could not identify any tables or plots to test")

  output <- makeExpectations(tests, name, options, dataset)
  cat(output)
}

getTests <- function(results) {
  tests <- list()

  markResultsLocationExtractTests <- function(x) {
    if (! "list" %in% class(x))
      return(x)

    unitTestType <- NULL
    if (all(c("data", "schema") %in% names(x)))
      unitTestType <- "table"
    else if (all(c("data", "width", "height") %in% names(x)))
      unitTestType <- "plot"

    if (!is.null(unitTestType) && unitTestType == "plot" || (unitTestType == "table" && length(x[["data"]]) > 0)) {
      testid <- length(tests)
      tests[[paste0("itemToUnitTest-", testid)]] <<- list(
        title = unlist(x[["title"]]),
        id = testid,
        type = unitTestType,
        data =  ifelse(unitTestType == "table", makeTestTable(x[["data"]], print=FALSE), ""))
      x[["itemToUnitTest"]] <- testid
    }

    return(lapply(x, markResultsLocationExtractTests))
  }

  markedResults <- markResultsLocationExtractTests(results)
  if (length(tests) > 0) {
    tests <- addPathIndexToTests(tests, markedResults)
    tests <- getOneTestPerCollection(tests)
  }

  return(tests)
}

addPathIndexToTests <- function(tests, markedResults) {
  results <- unlist(markedResults)
  for (testName in names(tests)) {
    id <- tests[[testName]][["id"]]
    dotSeparatedPathName <- getTestLocationInResultsById(results, id)
    tests[[testName]][["index"]] <- normalizeTestPath(dotSeparatedPathName)
  }
  return(tests)
}

getTestLocationInResultsById <- function(results, id) {
  testIndices <- which(grepl("itemToUnitTest", names(results)))
  index <- testIndices[results[testIndices] == id]
  if (length(index) != 1)
    stop("Failed to uniquely identify test case in results")

  location <- names(results)[index]
  return(gsub("itemToUnitTest", "data", location))
}

normalizeTestPath <- function(index) {
  indexNames <- unlist(strsplit(index, ".", fixed=TRUE))
  path <- paste0('[["', paste0(indexNames, collapse='"]][["'), '"]]')

  return(path)
}

getOneTestPerCollection <- function(tests) {
  pathNames <- NULL
  for (test in tests) {
    pathNames <- c(pathNames, test$index)
  }

  firstSiblingOrUnique <- rep(TRUE, length(tests))

  purgedTests <- tests[firstSiblingOrUnique]

  return(purgedTests)
}

makeExpectations <- function(tests, name, options, dataset) {
  centralizePreamble <- FALSE
  if (length(tests) > 1)
    centralizePreamble <- TRUE

  expectations <- ""

  if (centralizePreamble) {
    preamble <- addPreambleLines(name, options, dataset)
    expectations <- paste0(preamble, "\n")
  }

  for (test in tests) {
    if (!test$type %in% c("table", "plot"))
      stop("Unknown test type extracted from results, cannot continue: ", test$type)

    expectation <- makeSingleExpectation(test, name, options, dataset, centralizePreamble)
    expectations <- paste(expectations, expectation, sep="\n\n")
  }

  return(expectations)
}

makeSingleExpectation <- function(test, name, options, dataset, centralizePreamble) {
  if (!is.character(test$title) || test$title == "") {
    test$title <- paste("titleless", test$type, test$id, sep="-")
    warning(test$type, " does not have a title, using a generic one: ", test$title, immediate.=TRUE)
  }

  openingLine <- addOpeningLine(test)

  preambleLines <- NA
  if (!centralizePreamble) {
    preambleLines <- addPreambleLines(name, options, dataset)
    preambleLines <- gsub("\n", "\n\t", paste0("\t", preambleLines))
  }

  testSpecificLines <- NA
  if (test$type == "table")
    testSpecificLines <- addTableSpecificLines(test)
  else if (test$type == "plot")
    testSpecificLines <- addPlotSpecificLines(test, name)

  closingLine <- "})"

  expectation <- paste(openingLine, preambleLines, testSpecificLines, closingLine, sep="\n")
  expectation <- gsub("NA\n", "", expectation)

  return(expectation)
}

addOpeningLine <- function(test) {
  opening <- paste0('test_that("', test$title)

  titleContainsTestType <- grepl(test$type, test$title, ignore.case=TRUE)
  if (test$type == "table")
    opening <- paste0(opening, ifelse(titleContainsTestType, '',  ' table'), ' results match", {')
  else if (test$type == "plot")
    opening <- paste0(opening, ifelse(titleContainsTestType, '', ' plot'), ' matches", {')

  return(opening)
}

addPreambleLines <- function(name, options, dataset) {
  settingOfOptions <- addOptionSpecificationLines(name, options)
  settingOfSeed <- "set.seed(1)"
  runningOfAnalysis <- addRunAnalysisLines(name, dataset)

  return(paste(settingOfOptions, settingOfSeed, runningOfAnalysis, sep="\n"))
}

addRunAnalysisLines <- function(name, dataset) {
  if (is.character(dataset))
    dataArg <- paste0('"', dataset ,'"')
  else
    dataArg <- paste0('dataset')

  readingData <- paste0('dataset <- ', paste(capture.output(dput(dataset)), collapse="\n"))

  running <- paste0('results <- runAnalysis("', name, '", ', dataArg, ', options)')

  if (is.character(dataset))
    return(running)
  else
    return(paste(readingData, running, sep="\n"))
}

addTableSpecificLines <- function(test) {
  gettingTable <- paste0('\ttable <- results[["results"]]', test$index)

  comparingTables <- paste0("\tjaspTools::expect_equal_tables(table,\n\t\t", gsub("\n", "\n\t\t", test$data), ")")

  return(paste(gettingTable, comparingTables, sep="\n"))
}

addPlotSpecificLines <- function(test, name) {
  gettingPlotName <- paste0('\tplotName <- results[["results"]]', test$index)

  gettingPlot <- paste0('\ttestPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]')

  title <- gsub("-+", "-", gsub("\\W", "-", tolower(test$title)))
  comparingPlots <- paste0('\tjaspTools::expect_equal_plots(testPlot, "', title, '", dir="', name, '")')

  return(paste(gettingPlotName, gettingPlot, comparingPlots, sep="\n"))
}

addOptionSpecificationLines <- function(name, options) {
  settingOfOptions <- paste0('options <- analysisOptions("', name, '")')

  nonDefaultOpts <- getNonDefaultOptions(name, options)
  if (length(nonDefaultOpts) > 0) {
    nonDefaults <- paste0("options$", names(nonDefaultOpts), " <- ", nonDefaultOpts, collapse="\n")
    settingOfOptions <- paste0(settingOfOptions, "\n", nonDefaults)
  }

  return(settingOfOptions)
}

getNonDefaultOptions <- function(name, options) {
  defaultOpts <- analysisOptions(name)
  if (!is.list(defaultOpts) || is.null(names(defaultOpts)))
    stop("Couldn't find the default analysis options for this analysis")

  nonDefaultOpts <- NULL
  for (optName in names(options)) {
    optValue <- options[[optName]]
    if (!isTRUE(all.equal(defaultOpts[[optName]], optValue))) {
      options[[optName]] <- prepOptionValueForPrinting(optValue)
      nonDefaultOpts <- c(nonDefaultOpts, options[optName])
    }
  }

  return(nonDefaultOpts)
}

prepOptionValueForPrinting <- function(value) {
  if (is.list(value))
    result <- paste(capture.output(dput(value)), collapse="\n")
  else if (is.character(value) && length(value) == 1 && !startsWith(value, "\""))
    result <- paste0("\"", value, "\"")
  else
    result <- value

  return(result)
}
