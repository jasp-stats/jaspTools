context("extractDatasetFromJASPFile")

# The dataset is read by jaspSyntax::readDatasetFromJaspFile: scale columns are numeric, nominal
# and ordinal columns are character vectors of their values (also when these are numbers).
expectColumnsMatchCsv <- function(df, csv) {
  expect_equal(nrow(df), nrow(csv), info = "Row count should match")
  expect_equal(names(df), names(csv), info = "Column names should match")

  for (col in names(csv)) {
    if (is.numeric(df[[col]])) {
      expect_equal(df[[col]], csv[[col]], tolerance = 1e-6, info = paste("Column", col, "should match"))
    } else {
      expect_type(df[[col]], "character")
      expect_equal(df[[col]], as.character(csv[[col]]), info = paste("Column", col, "should match"))
    }
  }
}

test_that("extractDatasetFromJASPFile extracts dataset correctly from JASP file", {

  # Path to test JASP file and expected CSV
  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")
  skip_if_not(file.exists(csvFile), "Expected CSV file not found")

  # Extract dataset from JASP file
  df <- extractDatasetFromJASPFile(jaspFile)

  # Read expected CSV
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Check dimensions

  expect_equal(nrow(df), nrow(csv), info = "Row count should match")
  expect_equal(ncol(df), ncol(csv), info = "Column count should match")
  expect_equal(names(df), names(csv), info = "Column names should match")

})

test_that("extractDatasetFromJASPFile handles numeric columns correctly", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Check numeric columns match (with tolerance)
  numericCols <- c("V1", "contNormal", "contGamma", "contExpon", "contWide", "contNarrow",
                   "contOutlier", "contcor1", "contcor2", "facFifty", "debMiss1", "debMiss30",
                   "debMiss80", "debMiss99", "debNaN10", "debCollin1", "debCollin2", "debCollin3",
                   "debEqual1", "debEqual2", "debSame")

  for (col in numericCols) {
    expect_equal(df[[col]], csv[[col]], tolerance = 1e-6,
                 info = paste("Numeric column", col, "should match"))
  }
})

test_that("extractDatasetFromJASPFile handles categorical columns correctly", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Check categorical columns match
  categoricalCols <- c("facGender", "facExperim", "facOutlier", "debString", "unicode")

  for (col in categoricalCols) {
    expect_equal(df[[col]], csv[[col]], info = paste("Categorical column", col, "should match"))
  }
})

test_that("extractDatasetFromJASPFile handles binary columns correctly", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Binary columns like contBinom are nominal: their values 0 and 1 come as character
  expect_equal(sort(unique(na.omit(df[["contBinom"]]))), c("0", "1"),
               info = "contBinom should have values 0 and 1")
  expect_equal(df[["contBinom"]], as.character(csv[["contBinom"]]),
               info = "contBinom values should match CSV")

  expect_equal(sort(unique(na.omit(df[["debBinMiss20"]]))), c("0", "1"),
               info = "debBinMiss20 should have values 0 and 1 (with NAs)")
  expect_equal(df[["debBinMiss20"]], as.character(csv[["debBinMiss20"]]),
               info = "debBinMiss20 values should match CSV")
})

test_that("extractDatasetFromJASPFile handles infinity correctly", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # debInf is a scale column: the infinity symbol of the CSV is Inf
  expect_type(df[["debInf"]], "double")
  expect_equal(unique(csv[["debInf"]]), "∞")
  expect_equal(df[["debInf"]], rep(Inf, nrow(csv)), info = "debInf should be Inf where the CSV has the infinity symbol")
})

test_that("extractDatasetFromJASPFile handles NaN/NA correctly", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # debNaN should be all NA
  expect_true(all(is.na(df[["debNaN"]])), info = "debNaN should be all NA")

  # debNaN10 should have some NAs matching CSV
  expect_equal(is.na(df[["debNaN10"]]), is.na(csv[["debNaN10"]]),
               info = "debNaN10 NA positions should match CSV")

  # debMiss30 should have NAs matching CSV
  expect_equal(is.na(df[["debMiss30"]]), is.na(csv[["debMiss30"]]),
               info = "debMiss30 NA positions should match CSV")
})

test_that("extractDatasetFromJASPFile handles Unicode correctly", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Unicode column should match CSV exactly
  expect_equal(df[["unicode"]], csv[["unicode"]], info = "Unicode column should match CSV")
})

test_that("extractDatasetFromJASPFile validates input", {

  # Non-existent file
  expect_error(extractDatasetFromJASPFile("nonexistent.jasp"), "File not found")

  # Wrong extension
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")
  if (file.exists(csvFile)) {
    expect_error(extractDatasetFromJASPFile(csvFile), "must have a .jasp extension")
  }
})

test_that("extractDatasetFromJASPFile handles ordinal columns correctly", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "debug-descriptives.csv")

  skip_if_not(file.exists(jaspFile), "Test JASP file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # facFive is ordinal with values 1-5, which come as character
  expect_equal(sort(unique(df[["facFive"]])), as.character(1:5),
               info = "facFive should have values 1-5")
  expect_equal(df[["facFive"]], as.character(csv[["facFive"]]),
               info = "facFive values should match CSV")
})

test_that("extractDatasetFromJASPFile works for BCG Vaccine dataset", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles",
                        "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles",
                       "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.csv")

  skip_if_not(file.exists(jaspFile), "BCG Vaccine JASP file not found")
  skip_if_not(file.exists(csvFile), "BCG Vaccine CSV file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  expectColumnsMatchCsv(df, csv)
})

test_that("extractDatasetFromJASPFile reads files saved by JASP 0.97 and later", {

  # Since JASP 0.97 internal.sqlite has one Column_<n> per variable instead of Column_<n>_DBL and
  # Column_<n>_INT: such a file has to be read the way JASP itself does.
  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Invisibility_Cloak.jasp")
  csvFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Invisibility_Cloak.csv")

  skip_if_not(file.exists(jaspFile), "Invisibility Cloak JASP file not found")
  skip_if_not(file.exists(csvFile), "Invisibility Cloak CSV file not found")

  df <- extractDatasetFromJASPFile(jaspFile)
  csv <- read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  expect_equal(dim(df), c(24L, 3L))
  expectColumnsMatchCsv(df, csv)
})

test_that("extractDatasetFromJASPFile returns NULL for JASP files without data", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles",
                        "no_data_summary_stats.jasp")

  skip_if_not(file.exists(jaspFile), "No-data JASP file not found")

  # Should return NULL instead of error
  result <- extractDatasetFromJASPFile(jaspFile)

  expect_null(result, info = "JASP files without data should return NULL")
})
