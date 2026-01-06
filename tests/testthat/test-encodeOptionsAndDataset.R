context("encodeOptionsAndDataset")

test_that("encodeOptionsAndDataset encodes BCG Vaccine analysis options and dataset", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  csvFile  <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")
  skip_if_not(file.exists(csvFile), "Expected CSV file not found")

  # Get options from the third analysis (has effectSizeModelTerms with types)
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]

  # Load the dataset
  dataset <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Encode options and dataset
  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset)

  # Check that result has the expected structure
  expect_true(is.list(result))
  expect_true(all(c("options", "dataset", "encodingMap") %in% names(result)))

  # Check encoding map structure
  expect_true(is.data.frame(result$encodingMap))
  expect_true(all(c("original", "encoded", "type") %in% names(result$encodingMap)))

  # If there are encoded variables, verify encoding

  if (nrow(result$encodingMap) > 0) {
    # All encoded names should follow the pattern "jaspColumnN"
    expect_true(all(grepl("^jaspColumn\\d+$", result$encodingMap$encoded)))

    # All types should be valid
    expect_true(all(result$encodingMap$type %in% c("scale", "ordinal", "nominal")))

    # Encoded dataset should have the encoded column names
    expect_true(all(result$encodingMap$encoded %in% colnames(result$dataset)))

    # Check that encoded options no longer contain original variable names
    # that were in the encoding map
    originalVars <- result$encodingMap$original
    encodedVars <- result$encodingMap$encoded

    # Helper to check if a value contains any original variable names
    containsOriginal <- function(x) {
      if (is.character(x)) {
        any(x %in% originalVars)
      } else if (is.list(x)) {
        any(vapply(x, containsOriginal, logical(1)))
      } else {
        FALSE
      }
    }

    # Helper to check if a value contains encoded variable names
    containsEncoded <- function(x) {
      if (is.character(x)) {
        any(x %in% encodedVars)
      } else if (is.list(x)) {
        any(vapply(x, containsEncoded, logical(1)))
      } else {
        FALSE
      }
    }

    # Check that encoded options contain encoded names instead of original
    optNames <- names(result$options)
    optNames <- optNames[!grepl("\\.types$", optNames) & optNames != ".meta"]

    for (nm in optNames) {
      opt <- result$options[[nm]]
      if (containsEncoded(opt)) {
        # If it contains encoded names, it should not contain original names
        expect_false(containsOriginal(opt),
                     info = paste("Option", nm, "should not contain original variable names after encoding"))
      }
    }
  }
})

test_that("encodeOptionsAndDataset encodes debug-descriptives analysis options and dataset", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile  <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")
  skip_if_not(file.exists(csvFile), "Expected CSV file not found")

  # Get options from the first analysis
  opts <- jaspTools::analysisOptions(jaspFile)

  # Load the dataset
  dataset <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Encode options and dataset
  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset)

  # Check that result has the expected structure
  expect_true(is.list(result))
  expect_true(all(c("options", "dataset", "encodingMap") %in% names(result)))

  # Check encoding map structure
  expect_true(is.data.frame(result$encodingMap))
  expect_true(all(c("original", "encoded", "type") %in% names(result$encodingMap)))

  # If there are encoded variables, verify encoding
  if (nrow(result$encodingMap) > 0) {
    # All encoded names should follow the pattern "jaspColumnN"
    expect_true(all(grepl("^jaspColumn\\d+$", result$encodingMap$encoded)))

    # All types should be valid
    expect_true(all(result$encodingMap$type %in% c("scale", "ordinal", "nominal")))

    # Encoded dataset should have the encoded column names
    expect_true(all(result$encodingMap$encoded %in% colnames(result$dataset)))

    # Verify type coercion in dataset
    for (i in seq_len(nrow(result$encodingMap))) {
      encodedName <- result$encodingMap$encoded[i]
      colType <- result$encodingMap$type[i]
      col <- result$dataset[[encodedName]]

      if (colType == "nominal") {
        expect_true(is.factor(col),
                    info = paste(encodedName, "should be a factor for nominal type"))
      } else if (colType == "ordinal") {
        expect_true(is.ordered(col),
                    info = paste(encodedName, "should be an ordered factor for ordinal type"))
      } else if (colType == "scale") {
        expect_true(is.numeric(col),
                    info = paste(encodedName, "should be numeric for scale type"))
      }
    }
  }
})

test_that("encodeOptionsAndDataset produces unique variable-type combinations", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  csvFile  <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")
  skip_if_not(file.exists(csvFile), "Expected CSV file not found")

  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset)

  if (nrow(result$encodingMap) > 0) {
    # Check that variable-type combinations are unique
    varTypeCombos <- paste(result$encodingMap$original, result$encodingMap$type, sep = "_")
    expect_equal(length(varTypeCombos), length(unique(varTypeCombos)),
                 info = "Each variable-type combination should be unique in the encoding map")
  }
})

test_that("encodeOptionsAndDataset handles empty types gracefully", {

  # Create a simple options list without .types entries
  opts <- list(
    someOption = "value",
    anotherOption = 42,
    `.meta` = list()
  )

  # Create a simple dataset
  dataset <- data.frame(
    col1 = 1:5,
    col2 = letters[1:5]
  )

  # Should warn about no variable-type pairs found
  expect_warning(
    result <- jaspTools:::encodeOptionsAndDataset(opts, dataset),
    "No variable-type pairs found"
  )

  # Should return original options and dataset unchanged
  expect_equal(result$options, opts)
  expect_equal(result$dataset, dataset)
  expect_equal(nrow(result$encodingMap), 0)
})

test_that("encodeOptionsAndDataset correctly encodes nested option structures", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  csvFile  <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")
  skip_if_not(file.exists(csvFile), "Expected CSV file not found")

  # Get options with nested structure (effectSizeModelTerms has list of lists)
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset)

  # If effectSizeModelTerms was encoded, check nested structure
  if ("effectSizeModelTerms" %in% names(result$options)) {
    encodedTerms <- result$options$effectSizeModelTerms

    # Should still be a list
    expect_true(is.list(encodedTerms))

    # If it contains encoded values, they should be jaspColumnN format
    extractStrings <- function(x) {
      if (is.character(x)) {
        return(x)
      } else if (is.list(x)) {
        return(unlist(lapply(x, extractStrings)))
      } else {
        return(character(0))
      }
    }

    strings <- extractStrings(encodedTerms)
    encodedStrings <- strings[grepl("^jaspColumn\\d+$", strings)]

    if (length(encodedStrings) > 0) {
      # All encoded strings should be in the encoding map
      expect_true(all(encodedStrings %in% result$encodingMap$encoded),
                  info = "All encoded variable names should be in the encoding map")
    }
  }
})
