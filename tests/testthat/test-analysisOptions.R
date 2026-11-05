context("analysisOptions")

test_that("analysisOptions flattens simple types/value structures", {
  # Test case: split option with empty types and simple value
  # Raw structure: $split$types = list(), $split$value = ""
  # Expected: $split = "", $split.types = list()

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]


  # The split option should be flattened to just the value

  expect_true(is.character(opts$split) || is.null(opts$split$value))

  # If flattened correctly, split should be a character, not a list with types/value


  if (is.list(opts$split) && !is.null(opts$split$value)) {
    fail("split option was not flattened - still has $value structure")
  }

  # The .types should be stored separately
  expect_true("split.types" %in% names(opts) || identical(opts$split, ""))
})

test_that("analysisOptions flattens complex types/value structures with optionKey", {
  # Test case: effectSizeModelTerms with optionKey and nested lists
  # Raw structure:
  #   $effectSizeModelTerms$optionKey = "components"
  #   $effectSizeModelTerms$types = list("scale", "nominal")
  #   $effectSizeModelTerms$value = list(list(components = list("ablat")), list(components = list("alloc")))
  # Expected, as JASP gives it to an analysis (jaspSyntax::readAnalysisOptionsFromJaspFile(flattenTypes = TRUE)):
  #   $effectSizeModelTerms = list(list(components = list("ablat")), list(components = list("alloc")))
  #   $effectSizeModelTerms.types = list("scale", "nominal")

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]

  # The effectSizeModelTerms should be flattened to just the value (a list)
  expect_true(is.list(opts$effectSizeModelTerms))

  # It should NOT have the optionKey, types, value structure anymore
  expect_false("optionKey" %in% names(opts$effectSizeModelTerms))
  expect_false("types" %in% names(opts$effectSizeModelTerms))
  expect_false("value" %in% names(opts$effectSizeModelTerms))

  # The rows keep their components
  expect_equal(opts$effectSizeModelTerms[[1]]$components, list("ablat"))
  expect_equal(opts$effectSizeModelTerms[[2]]$components, list("alloc"))

  # The .types are stored separately, one per row
  expect_true("effectSizeModelTerms.types" %in% names(opts))
  expect_equal(opts$`effectSizeModelTerms.types`, list("scale", "nominal"))
})

test_that("analysisOptions flattens deeply nested types/value structures", {
  # Test case: variables[[1]]$coefficientAlpha with nested types/value
  # Raw structure:
  #   $variables[[1]]$coefficientAlpha$types = list()
  #   $variables[[1]]$coefficientAlpha$value = ""
  # Expected, as JASP does it, the .types next to the option in its row:
  #   $variables[[1]]$coefficientAlpha = ""
  #   $variables[[1]]$coefficientAlpha.types = list()

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]

  # The coefficientAlpha inside variables[[1]] should be flattened to just the value
  expect_true(is.character(opts$variables[[1]]$coefficientAlpha))
  expect_equal(opts$variables[[1]]$coefficientAlpha, "")

  # It should NOT have the types/value structure anymore
  expect_false(is.list(opts$variables[[1]]$coefficientAlpha))

  # The .types are stored next to it, in the same row
  expect_true("coefficientAlpha.types" %in% names(opts$variables[[1]]))
  expect_equal(opts$variables[[1]]$`coefficientAlpha.types`, list())
  expect_false("variables.types" %in% names(opts))
})

test_that("analysisOptions keeps a saved option with an optionKey and other members whole, like JASP", {
  # Test case: the lavaan syntax of a SEM analysis
  # Raw structure: $syntax = list(columns, model, modelOriginal, optionKey = "value", prefixedColumns, types, value)

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]

  expect_equal(opts$syntax$modelOriginal, "postnumb ~ prenumb + peabody")
  expect_equal(opts$syntax$value, list("peabody", "postnumb", "prenumb"))
  expect_equal(opts$syntax$optionKey, "value")
  expect_equal(opts$`syntax.types`, list("scale", "scale", "scale"))
})

test_that("analysisOptions returns the options of each analysis in a .jasp file with its name", {
  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)

  expect_length(opts, 3)
  expect_null(names(opts))
  expect_equal(
    vapply(opts, attr, character(1), which = "analysisName"),
    c("EffectSizeComputation", "FunnelPlot", "ClassicalMetaAnalysis")
  )
  expect_named(attributes(opts[[3]]), c("names", "analysisName"))
  expect_true(".meta" %in% names(opts[[3]]))

  single <- jaspTools::analysisOptions(file.path(testthat::test_path(), "..", "JASPFiles", "Invisibility_Cloak.jasp"))
  expect_equal(attr(single, "analysisName"), "TTestIndependentSamples")
  expect_equal(single$dependent, list("Mischief"))
  expect_equal(single$`dependent.types`, list("scale"))
  expect_equal(single$group, "Cloak")
})

test_that("analysisOptions extracts options from no-data JASP file", {

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles",
                        "no_data_summary_stats.jasp")

  skip_if_not(file.exists(jaspFile), "No-data JASP file not found")

  # Should be able to extract options without error
  opts <- jaspTools::analysisOptions(jaspFile)

  # Should return a valid options list
  expect_true(is.list(opts))

  # Should have an analysisName attribute
  expect_true(!is.null(attr(opts, "analysisName")))
})

test_that("analysisOptions takes the defaults of an analysis from its QML form, also from the QML files it imports", {

  # a minimal source module: Description.qml declares the analysis, its form uses a sibling QML file
  modulePath <- file.path(tempfile("jaspToolsModule"), "jaspFakeModule")
  dir.create(file.path(modulePath, "inst", "qml"), recursive = TRUE)
  dir.create(file.path(modulePath, "inst", "icons")) # jaspSyntax reads Description.qml like JASP, which requires it
  on.exit(unlink(dirname(modulePath), recursive = TRUE))

  writeLines(c("Package: jaspFakeModule", "Version: 0.1.0", "Title: Fake module", "License: GPL (>= 2)"),
             file.path(modulePath, "DESCRIPTION"))
  writeLines("export(FakeAnalysis)", file.path(modulePath, "NAMESPACE"))
  writeLines(c(
    'import QtQuick',
    'import JASP.Module',
    'Description',
    '{',
    '  title: "Fake module"',
    '  Analysis { title: "Fake analysis"; func: "FakeAnalysis"; qml: "FakeAnalysis.qml" }',
    '}'
  ), file.path(modulePath, "inst", "Description.qml"))
  writeLines(c(
    'import QtQuick',
    'import JASP.Controls',
    'Form',
    '{',
    '  VariablesForm',
    '  {',
    '    AvailableVariablesList { name: "allVariables" }',
    '    AssignedVariablesList  { name: "variables" }',
    '  }',
    '  DropDown',
    '  {',
    '    name: "correction"',
    '    values: [',
    '      { label: qsTr("None"), value: "none", info: qsTr("No adjustment, the default.") },',
    '      { label: "Bonferroni", value: "bonferroni", info: qsTr("Bonferroni correction.") }',
    '    ]',
    '  }',
    '  ImportedOptions {}',
    '}'
  ), file.path(modulePath, "inst", "qml", "FakeAnalysis.qml"))
  writeLines(c(
    'import QtQuick',
    'import JASP.Controls',
    'Group',
    '{',
    '  CheckBox { name: "importedCheck"; checked: true }',
    '  IntegerField { name: "importedCount"; defaultValue: 7 }',
    '}'
  ), file.path(modulePath, "inst", "qml", "ImportedOptions.qml"))

  local_mocked_bindings(getModulePathFromRFunction = function(funName) modulePath)

  opts <- analysisOptions("FakeAnalysis")

  expect_identical(attr(opts, "analysisName"), "FakeAnalysis")
  expect_identical(opts$correction, "none")
  expect_identical(opts$importedCheck, TRUE)
  expect_equal(opts$importedCount, 7)
  expect_false(".meta" %in% names(opts))
  expect_false(any(endsWith(names(opts), ".types")))
  # an empty list is left out, so the bridge fills it like JASP does
  expect_false("variables" %in% names(opts))
})

test_that("the deprecated readQML loads a form through the bridge and gives nothing for an imported QML file", {

  qmlDir <- tempfile("jaspToolsQml")
  dir.create(qmlDir)
  on.exit(unlink(qmlDir, recursive = TRUE))

  formFile <- file.path(qmlDir, "SomeForm.qml")
  writeLines(c(
    'import QtQuick',
    'import JASP.Controls',
    'Form',
    '{',
    '  CheckBox { name: "formCheck"; checked: true }',
    '  DropDown { name: "formChoice"; values: [{ label: "A", value: "a", info: qsTr("First.") }, { label: "B", value: "b" }] }',
    '}'
  ), formFile)

  importedFile <- file.path(qmlDir, "SomeSection.qml")
  writeLines(c(
    'import QtQuick',
    'import JASP.Controls',
    'Section',
    '{',
    '  title: "Imported"',
    '  CheckBox { name: "importedCheck" }',
    '}'
  ), importedFile)

  formOptions <- lifecycle::expect_deprecated(jaspTools:::readQML(formFile))
  expect_identical(formOptions$formCheck, TRUE)
  expect_identical(formOptions$formChoice, "a")

  importedOptions <- suppressWarnings(jaspTools:::readQML(importedFile))
  expect_identical(importedOptions, list())
})
