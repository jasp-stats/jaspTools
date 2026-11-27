test_that("nested result keys retain their case and result names", {
  for (model in c("model", "model2", "model10")) {
    case <- sub("^model([0-9]+)$", "model-\\1", model)
    root <- paste0("selectionModelResults_", model, "_")
    expect_identical(snapshotKeySlug(paste0(root, "weightFunctionSummary_selectionParameters")),
                     paste0(case, "-selection-parameters"))
    expect_identical(snapshotKeySlug(paste0(root, "modelSummaryContainer_pooledEstimatesTable")),
                     paste0(case, "-pooled-estimates"))
  }
  expect_identical(snapshotKeySlug("comparisonResults_trial3_summaryContainer_errorRates"),
                   "trial-3-error-rates")
  expect_identical(snapshotKeySlug("marginalMeansContainer_effectSize_year_meansTable"),
                   "effect-size-year-means")
  expect_identical(snapshotKeySlug("randomEstimatesContainer_containerS_tableS"), "random-estimates-s")
})

test_that("long snapshot names retain whole words and distinguish their results", {
  prefix <- paste("analysis-3", paste(rep("comparison", 8), collapse = "-"), sep = "-")
  first <- uniqueSnapshotName(paste0(prefix, "-selection-parameters"), character())
  second <- uniqueSnapshotName(paste0(prefix, "-selection-frequencies"), first)
  expect_match(first, "^analysis-3(-comparison)*-selection-parameters$")
  expect_match(second, "^analysis-3(-comparison)*-selection-frequencies$")
  expect_lte(nchar(first), 80L)
  expect_lte(nchar(second), 80L)

  # An unbroken token can use the full limit, even when a collision needs a suffix.
  token <- strrep("a", 90L)
  used <- uniqueSnapshotName(token, character())
  expect_identical(used, strrep("a", 80L))
  expect_identical(uniqueSnapshotName(token, used), paste0(strrep("a", 78L), "-2"))
})
