context("analysisOptions")

test_that("analysisOptions gives the options of a .jasp file as JASP saved them", {
  # The QML form reads a variables option with its types inside it: {value, types} (and optionKey for rows),
  # also inside the rows of a list. jaspTools does not split them into `x` and `x.types`.
  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "Effectiveness_of_the_BCG_Vaccine_Against_Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)

  expect_equal(opts[[2]]$split, list(types = list(), value = ""))
  expect_false("split.types" %in% names(opts[[2]]))

  expect_equal(opts[[3]]$effectSizeModelTerms$optionKey, "components")
  expect_equal(opts[[3]]$effectSizeModelTerms$types, list("scale", "nominal"))
  expect_equal(opts[[3]]$effectSizeModelTerms$value,
               list(list(components = list("ablat")), list(components = list("alloc"))))
  expect_false("effectSizeModelTerms.types" %in% names(opts[[3]]))

  expect_equal(opts[[1]]$variables[[1]]$coefficientAlpha, list(types = list(), value = ""))
  expect_false("coefficientAlpha.types" %in% names(opts[[1]]$variables[[1]]))
})

test_that("analysisOptions keeps a saved option with an optionKey and other members whole, like JASP", {
  # Test case: the lavaan syntax of a SEM analysis
  # Raw structure: $syntax = list(columns, model, modelOriginal, optionKey = "value", prefixedColumns, types, value)

  jaspFile <- file.path(testthat::test_path(), "..", "JASPFiles", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]

  expect_equal(opts$syntax$modelOriginal, "postnumb ~ prenumb + peabody")
  expect_equal(opts$syntax$value, list("peabody", "postnumb", "prenumb"))
  expect_equal(opts$syntax$optionKey, "value")
  expect_equal(opts$syntax$types, list("scale", "scale", "scale"))
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
  expect_equal(single$dependent, list(types = list("scale"), value = list("Mischief")))
  expect_equal(single$group, list(types = list("nominal"), value = "Cloak"))
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
