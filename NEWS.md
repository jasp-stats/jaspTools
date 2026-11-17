# jaspTools 0.21.0

## Running analyses

* `runAnalysis()` runs the analysis through `jaspBase::runWrappedAnalysis()`, the function every R wrapper calls: the
  QML form of the analysis checks and encodes the options, and jaspBase gives the results back with the plain column
  names. jaspTools no longer encodes options or decodes results itself.
* New `runAnalysisFromJaspFile()` runs an analysis of a .jasp file again with the data and options saved in it, its
  options upgraded from the module version that saved the file. New `expect_equal_jasp_file()` compares the tables of
  such a run with the results saved in the file.
* `makeTestsFromExamples()` generates tests that call `runAnalysisFromJaspFile()`, and warns when an analysis no longer
  gives the results saved in its file.
* jaspTools needs jaspBase 0.20.6 or later, which it now declares in `Imports`. The code for jaspBase before 0.16.4
  (the separate jaspResults package) is removed.
* A module of `module.dirs` is also reinstalled when one of its QML files changes, as the analysis reads the form of
  the installed module.
* With `quiet = TRUE` (always inside tests) the warnings the QML form gives while it parses the options (an unknown
  option, the upgrade of options of an older module version) are no longer hidden.

## Breaking changes for module tests

* `analysisOptions(<file>.jasp)` gives the options as JASP saved them: a variables option is
  `list(value = ..., types = ...)` (with `optionKey` for the rows of a list), and there are no `x.types` options next to
  it any more. Tests that load options from a .jasp file and then set `options[["x.types"]]`, read `x.types`, or append
  to a variables option need updating; regenerating them with `makeTestsFromExamples()` gives tests that use
  `runAnalysisFromJaspFile()` instead. Options from `analysisOptions("<analysis>")` are not affected, and a test may
  still set `x.types` on those.
* `extractDatasetFromJASPFile()` gives nominal columns as factors and ordinal columns as ordered factors, with their
  levels in the order of the file (from jaspSyntax 1.4.3; character vectors before).
* The unused internals `approxMatch()`, `fixRNGForTesting()`, `getFilename()` and `devcat()` are removed.
