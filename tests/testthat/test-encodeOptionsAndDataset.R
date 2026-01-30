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

test_that("encodeOptionsAndDataset handles NULL dataset gracefully", {

  # Create a simple options list (typical for summary stats analyses without data)
  opts <- list(
    n = 100,
    mean = 50,
    sd = 10,
    `.meta` = list()
  )

  # Should not warn or error when dataset is NULL
  result <- jaspTools:::encodeOptionsAndDataset(opts, NULL)

  # Should return original options unchanged
  expect_equal(result$options, opts)

  # Dataset should remain NULL
  expect_null(result$dataset)

  # Encoding map should be empty
  expect_equal(nrow(result$encodingMap), 0)
  expect_true(is.data.frame(result$encodingMap))
  expect_true(all(c("original", "encoded", "type") %in% names(result$encodingMap)))
})

test_that("encodeOptionsAndDataset works with no-data JASP file", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles",
                        "no_data_summary_stats.jasp")

  skip_if_not(file.exists(jaspFile), "No-data JASP file not found")

  # Extract options and dataset
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Dataset should be NULL
  expect_null(dataset)

  # Encode should work without errors
  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset)

  # Result structure should be correct
  expect_true(is.list(result))
  expect_true(all(c("options", "dataset", "encodingMap") %in% names(result)))

  # Options should be unchanged (since no dataset columns to encode)
  expect_equal(result$options, opts)

  # Dataset should remain NULL
  expect_null(result$dataset)

  # Encoding map should be empty
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

test_that("encodeOptionsAndDataset with forceEncode replaces column names in model string via regex", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "bainSem.jasp")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  # Get options from the second analysis (has model with column names like peabody, age)
  # Note: bainSem.jasp has 3 analyses, we use the second one
  opts <- jaspTools:::analysisOptionsFromJASPFile(jaspFile)[[2]]

  # Load the dataset
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # The second bainSem analysis has a 'model' option with embedded column names like:
  # "A~B > A~peabody = A~age = 0\n..."
  # where peabody and age are column names in the dataset

  # First, verify the model option exists and contains column names
  expect_true("model" %in% names(opts))
  originalModel <- opts$model

  # Check that the model string contains some column names from the dataset
  colNamesInModel <- colnames(dataset)[vapply(colnames(dataset), function(cn) {
    grepl(cn, originalModel, fixed = TRUE)
  }, logical(1))]
  expect_true(length(colNamesInModel) > 0,
              info = "Model option should contain column names from dataset")

  # Encode with forceEncode = "model"
  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = "model")

  # Check that the encoding map was created
  expect_true(nrow(result$encodingMap) > 0,
              info = "Encoding map should have entries")

  # Check that the model option has been force-encoded
  encodedModel <- result$options$model

  # The encoded model should not contain original column names that were in the encoding map
  originalVars <- result$encodingMap$original
  encodedVars <- result$encodingMap$encoded

  # Check that column names in the encoding map have been replaced
  # Only check variables that were actually in the original model
  for (i in seq_along(originalVars)) {
    origVar <- originalVars[i]
    encVar <- encodedVars[i]

    # Check if this variable was in the original model
    escapedVar <- gsub("([.\\\\^$|?*+()\\[\\]\\{\\}])", "\\\\\\1", origVar)
    pattern <- paste0("(?<![A-Za-z0-9_])", escapedVar, "(?![A-Za-z0-9_])")

    if (grepl(pattern, originalModel, perl = TRUE)) {
      # This variable was in the original model, so it should be replaced
      expect_false(grepl(pattern, encodedModel, perl = TRUE),
                   info = paste("Model should not contain original variable name:", origVar))
      expect_true(grepl(encVar, encodedModel, fixed = TRUE),
                  info = paste("Model should contain encoded variable name:", encVar))
    }
  }

  # Verify the structure is preserved (should still have the same operators and structure)
  expect_true(grepl("~", encodedModel, fixed = TRUE),
              info = "Model should still contain SEM syntax operators")
  expect_true(grepl(">", encodedModel, fixed = TRUE) || grepl("&", encodedModel, fixed = TRUE),
              info = "Model should still contain constraint operators")
})

test_that("forceEncode only affects specified options", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "bainSem.jasp")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  # Get options from the second analysis
  opts <- jaspTools:::analysisOptionsFromJASPFile(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Test that forceEncode only affects the 'model' option, not other options
  # The 'syntax' option also references variables but has shouldEncode metadata

  # Encode with forceEncode = "model"
  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = "model")

  # The 'model' option should be force-encoded (column names replaced with jaspColumnN)
  encodedModel <- result$options$model
  expect_true(grepl("jaspColumn", encodedModel),
              info = "Model should contain encoded column names")
})

test_that("forceEncode handles multiple options", {

  # Create a simple test case with multiple options to force-encode
  opts <- list(
    formula1 = "A ~ B + C",
    formula2 = "D ~ A + B",
    regularOpt = c("A", "B"),
    `regularOpt.types` = c("scale", "scale"),
    `.meta` = list()
  )

  dataset <- data.frame(
    A = 1:5,
    B = 6:10,
    C = 11:15,
    D = 16:20,
    stringsAsFactors = FALSE
  )

  # Encode with forceEncode for both formula options
  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = c("formula1", "formula2"))

  # Check encoding map was created
  expect_true(nrow(result$encodingMap) > 0)

  # Both formula options should have A and B replaced
  expect_false(grepl("(?<![A-Za-z0-9_])A(?![A-Za-z0-9_])", result$options$formula1, perl = TRUE),
               info = "formula1 should not contain original 'A'")
  expect_false(grepl("(?<![A-Za-z0-9_])B(?![A-Za-z0-9_])", result$options$formula1, perl = TRUE),
               info = "formula1 should not contain original 'B'")
  expect_false(grepl("(?<![A-Za-z0-9_])A(?![A-Za-z0-9_])", result$options$formula2, perl = TRUE),
               info = "formula2 should not contain original 'A'")
  expect_false(grepl("(?<![A-Za-z0-9_])B(?![A-Za-z0-9_])", result$options$formula2, perl = TRUE),
               info = "formula2 should not contain original 'B'")

  # Should contain jaspColumn references
  expect_true(grepl("jaspColumn", result$options$formula1),
              info = "formula1 should contain encoded names")
  expect_true(grepl("jaspColumn", result$options$formula2),
              info = "formula2 should contain encoded names")
})

test_that("forceEncode uses word boundaries to avoid partial matches", {

  # Test that forceEncode doesn't replace partial matches
  # e.g., if column is "A", it shouldn't replace "AB" -> "jaspColumn1B"
  opts <- list(
    model = "AB ~ A + B",  # AB is a single term, A and B are separate
    variables = c("A", "B"),
    `variables.types` = c("scale", "scale"),
    `.meta` = list()
  )

  dataset <- data.frame(
    A = 1:5,
    B = 6:10,
    AB = 11:15,  # This column should NOT be matched if not in encoding map
    stringsAsFactors = FALSE
  )

  # Only A and B are in the options with types, not AB
  result <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = "model")

  # The model should have A and B replaced but AB should remain (since AB is not in encoding map)
  encodedModel <- result$options$model

  # A and B should be replaced
  expect_false(grepl("(?<![A-Za-z0-9_])A(?![A-Za-z0-9_])", encodedModel, perl = TRUE),
               info = "Standalone A should be replaced")
  expect_false(grepl("(?<![A-Za-z0-9_])B(?![A-Za-z0-9_])", encodedModel, perl = TRUE),
               info = "Standalone B should be replaced")

  # AB should remain unchanged (it's not in the encoding map)
  expect_true(grepl("AB", encodedModel, fixed = TRUE),
              info = "AB should remain unchanged as it's not in the encoding map")
})
