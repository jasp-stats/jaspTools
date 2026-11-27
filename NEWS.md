# Development version

* Generated table and HTML tests use external JSON and HTML snapshots.
  `expect_snapshot_table()` checks the complete JASP table, including typed
  cells, column order and footnotes. Existing inline tests remain supported.
  Continuous results use configurable per-cell numerical tolerances.
  Passing reruns retain the accepted values, including in unchanged cells
  of a table with other changes under review.
* `reviewSnapshots()` shows table, HTML and figure changes together.
  Tables open with JASP's default number formatting. Click once for
  full-precision differences, then cycle through the candidate and baseline.
  `manageTestPlots()` opens the same reviewer for pending figures.
  `agentTestAnalysis()` and `agentTestAll()` link the report from their results
  and logs. `applySnapshotReview()` applies exported decisions and returns
  comments for each reviewed snapshot.

# jaspTools 0.21.0

## Running analyses

* `runAnalysis()` runs the analysis through `jaspBase::runWrappedAnalysis()`, the function every R wrapper calls: the
  QML form of the analysis checks and encodes the options, and jaspBase gives the results back with the plain column
  names. jaspTools no longer encodes options or decodes results itself.
* New `runAnalysisFromJaspFile()` runs an analysis of a .jasp file again with the data and options saved in it, its
  options upgraded from the module version that saved the file. New `expect_equal_jasp_file()` compares the tables of
  such a run with the results saved in the file.
* New `runAnalysisSyntax()` runs a call of the R wrapper of an analysis (its R syntax, as JASP shows it), optionally on
  the data of a .jasp file, and gives the results as `runAnalysis()` does.
* `makeTestsFromExamples()` generates tests that run the R syntax of each analysis with `runAnalysisSyntax()` on the
  data of its .jasp file: the call of its R wrapper, with the options of the file upgraded to the version of the module
  (`jaspSyntax::analysisSyntax()`). An analysis without R wrapper is run with `runAnalysisFromJaspFile()` instead. The
  generator warns when an analysis no longer gives the results saved in its file.
* With a jaspBase before 0.20.6 (which the `renv.lock` of a module may still pin), `runAnalysis()` runs the analysis as
  before: jaspTools parses the options through the QML form, calls `jaspBase::runJaspResults()` and decodes the
  results. `runAnalysisSyntax()` decodes the results of the R wrapper itself then. `runAnalysisFromJaspFile()` needs
  jaspBase 0.20.6 or later. jaspBase is in `Suggests`, so installing
  jaspTools never replaces the jaspBase a module pins. The code for jaspBase before 0.16.4 (the separate jaspResults
  package) is removed.
* A module of `module.dirs` is also reinstalled when one of its QML files changes, as the analysis reads the form of
  the installed module.
* With `quiet = TRUE` (always inside tests) the warnings the QML form gives while it parses the options (an unknown
  option, the upgrade of options of an older module version) are no longer hidden.

## Breaking changes for module tests

* `analysisOptions(<file>.jasp)` gives the options as JASP saved them: a variables option is
  `list(value = ..., types = ...)` (with `optionKey` for the rows of a list), and there are no `x.types` options next to
  it any more. Tests that load options from a .jasp file and then set `options[["x.types"]]`, read `x.types`, or append
  to a variables option need updating; regenerating them with `makeTestsFromExamples()` gives tests that run
  the R syntax of the analysis instead. Options from `analysisOptions("<analysis>")` are not affected, and a test may
  still set `x.types` on those.
* `extractDatasetFromJASPFile()` gives nominal columns as factors and ordinal columns as ordered factors, with their
  levels in the order of the file (from jaspSyntax 1.4.3; character vectors before).
* `encodeOptionsAndDataset()` is deprecated: it gives the options and the dataset back unchanged, and `runAnalysis()`
  ignores its `encodedDataset` argument, so tests written as
  `encoded <- encodeOptionsAndDataset(options, dataset); runAnalysis(name, encoded$dataset, encoded$options, encodedDataset = TRUE)`
  still run. New tests call `runAnalysis(name, dataset, options)`.
* The unused internals `approxMatch()`, `fixRNGForTesting()`, `getFilename()` and `devcat()` are removed.
