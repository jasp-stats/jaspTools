<div align="right">

[![Unit Tests](https://github.com/jasp-stats/jaspTools/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspTools/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspTools/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspTools)
<br>
<b>Maintainer:</b> <a href="https://github.com/vandenman/">Don van den Bergh</a>

</div>

# jaspTools
R package which aids developers with writing JASP analyses.

## Installation
You can install the R package with the use of `remotes`:

```
install.packages("remotes")
remotes::install_github("jasp-stats/jaspTools")
```

jaspTools needs [jaspSyntax](https://github.com/jasp-stats/jaspSyntax), which comes from the [jasp-stats R-universe](https://jasp-stats.r-universe.dev) as a binary package for Windows, macOS and Linux. `remotes` finds it there through the `Additional_repositories` field of jaspTools. `pak` and `renv` do not read that field, so with them add the universe to your repositories first:

```
options(repos = c(jasp = "https://jasp-stats.r-universe.dev", getOption("repos")))
```

### Using another jaspSyntax
jaspTools uses whatever jaspSyntax is installed; the universe is only used when jaspSyntax is missing or too old. To run jaspTools with another jaspSyntax, install that one over it:

- a branch, tag or commit of jaspSyntax:
  ```
  remotes::install_github("jasp-stats/jaspSyntax@<ref>", upgrade = "never")
  ```
  This builds from source. Set `Sys.setenv(JASPSYNTAX_RELEASE_TAG = "<release>")` first to use the SyntaxInterface library of another jaspSyntax release.
- your own jasp-desktop build:
  ```
  install.packages("<path to jaspSyntax checkout>", repos = NULL, type = "source",
                   configure.vars = "JASP_BUILD_DIR='<jasp-desktop build dir>'")
  ```
  or `configure.vars = "JASPSYNTAX_LIB_PATH='<path to the SyntaxInterface library>'"` for a single library file.

The universe only serves the latest version of jaspSyntax, so an older one has to come from its git tag. Keep in mind that `update.packages()`, or `pak`/`install.packages()` with the universe in your repositories, replaces your jaspSyntax as soon as the universe has a higher version.

Note: On some macs, it seems to be hard to build the dependency "data.table" from source resulting in the error message "clang: error: unsupported option '-fopenmp'". One hack to get around this is to select no when asked to install the package from source. This will install an older version of data.table without openmp support, which suffices for jaspTools. Alternatively, you might want to upgrade [clang](https://cran.r-project.org/bin/macosx/tools/). 

After loading jaspTools with `library(jaspTools)`, you need to call `setupJaspTools()`. This is
required once after every reinstall of jaspTools.

By default jaspTools runs the analyses with the development version of JASP. To run them with a released
JASP version, give its tag, e.g. `setupJaspTools(jaspVersion = "v0.99.1")` (or set the environment variable
`JASPTOOLS_JASP_VERSION`, as CI does), in a fresh R session. That tag names a
[jaspSyntax release](https://github.com/jasp-stats/jaspSyntax/releases): jaspTools installs jaspSyntax at that tag
with the library of that release, and takes the html files and data sets from the jasp-desktop commit that
library was built from. jaspBase is not part of a JASP version: the module's own jaspBase is used.

## Functionality
At the moment jaspTools has three classes of functions.
Each of these functions has documentation you may view by the usual syntax, e.g., `?runAnalysis`.

The general classes are:


### 1. Modifying jaspTools' settings
- `viewPkgOptions`: views options in the package
- `setPkgOption`: change an options in the package (e.g., what module you are working on)

After `setupJaspTools()` retrieves all the necessary dependencies (packages, html, data files), the paths to a number of these dependencies are stored inside of the pkgOptions.
You will generally not need to change them.
However, what you must usually do is tell jaspTools what modules you are working on (unless your working directory already points to a module):

`setPkgOption("module.dirs", c("/path/to/module1", "/path/to/module2"))`

jaspTools will use these to locate R functions, tests, etc. The module(s) that you specify are automatically reinstalled every time you change your R, NAMESPACE or DESCRIPTION files.

### 2. Running JASP analyses
- `runAnalysis`: run a JASP analysis
- `view`: view output of a JASP analysis
- `analysisOptions`: create options used by `runAnalysis`

#### Obtaining options
There are three general procedures to obtaining the options to run an analysis with in jaspTools.  

##### Procedure 1
The first procedure uses the .qml files to create option lists.
These lists will almost always require further editing.

###### Example
```
#create default options
options <- analysisOptions("BinomialTest")

#we want to perform the binomial test on 'contBinom'
options[["variables"]] <- "contBinom"
runAnalysis("BinomialTest", dataset="debug.csv", options=options)
```

##### Procedure 2
And so the second procedure might be preferred. You can set the options to your liking in JASP and then save the .jasp file (it may contain several analyses).
You can then let jaspTools read the .jasp file to extract the options from.

###### Example
```
#fetch all the options from all analyses in a .jasp file
options <- analysisOptions("~/path/to/file.jasp")

#we want to use the options of the first analysis (note that you may omit the analysis name argument)
runAnalysis(dataset="debug.csv", options=options[[1]])
```

##### Procedure 3
The third procedure uses syntax generated by Qt and doesn't require you to save the .jasp file, but instead can be done in real-time.
To use this enable "Log to file" in `JASP > Preferences > Advanced > Logging options`.
Press on "Show logs" and then run the analysis you're interested in in JASP.
Once you've set the analysis options to your liking go to the log directory.
There will be multiple log files. Search for the set of most recent "*Engine *.log" files.
Then go to "*Engine 1.log" (it's usually in the first engine unless you were running multiple analyses at the same time) 
and scroll to the bottom, under "Engine::receiveAnalysisMessage:" you will find a json string.
Copy the json, including the "{" and "}" and insert this into `analysisOptions`.

###### Example
```
#create options from the Qt json string
options <- analysisOptions('{
   "dynamicModuleCall" : "",
   "id" : 1,
   "jaspResults" : true,
   "name" : "BinomialTest",
   "options" : {
      ".meta" : {
         "variables" : {
            "containsColumn" : true
         }
      },
      "VovkSellkeMPR" : false,
      "confidenceInterval" : false,
      "confidenceIntervalInterval" : 0.950,
      "descriptivesPlots" : false,
      "descriptivesPlotsConfidenceInterval" : 0.950,
      "hypothesis" : "notEqualToTestValue",
      "plotHeight" : 320,
      "plotWidth" : 480,
      "testValue" : "0.5",
      "variables" : [ "contBinom" ]
   },
   "perform" : "run",
   "revision" : 1,
   "rfile" : "",
   "title" : "Binomial Test",
   "typeRequest" : "analysis"
}')
runAnalysis("BinomialTest", dataset="debug.csv", options=options)
```

jaspTools comes bundled with all the datasets from JASP, so you may simply refer to them by name (e.g., "debug.csv").
It is not necessary to use a JASP dataset (such as the debug.csv file we showed in the examples), however, you may provide any
data.frame to the dataset argument of the `runAnalysis` function. 

### 3. Testing JASP analyses

Run `testAnalysis("AnalysisName")` for one analysis or `testAll()` for the
configured modules. The corresponding `agentTestAnalysis()` and `agentTestAll()`
return the test results and link an HTML report when the tested files have
pending snapshots. Each module's report is available in `result$snapshotReviews`
and its entry in the JSON test log.

#### Writing and migrating tests

`makeTestsFromExamples()` generates expectations from a fresh analysis run and
writes table and HTML references under `tests/testthat/_snaps/<test-file>/`.
Commit the generated R file and reviewed references together. Regenerating a
test with an existing reference stages changes for review. The same expectations
work in handwritten tests. For example, given a result with a binomial table,
an HTML summary and a figure, the checks inside `test_that()` can read

```r
expect_snapshot_table(results$results$binomialTable, "binomial")
expect_snapshot_html(results$results$summary, "analysis-summary")
expect_equal_plots(results$state$figures[[1]]$obj, "distribution")
```

To migrate an inline `expect_equal_tables(table$data, list(...))`, pass the
whole table to `expect_snapshot_table(table, "name")` and run the test. Review
the first candidate against the original expected result before accepting it.
Existing inline tests still use their original rounding rules. New JSON
snapshots retain the numeric values supplied by JASP, ordered column
definitions, typed cells, groups, footnotes and table errors. HTML snapshots
are readable HTML files. Plot snapshots continue to use vdiffr SVGs.

Table snapshots keep the full-precision reference. Each continuous result
passes when its absolute difference is at most the larger of `atol` and
`rtol * max(abs(reference), abs(current))`. The defaults are `rtol = 1e-8`
and `atol = 1e-12`. The schema's `number` and `pvalue` types identify
continuous results, including typed values inside mixed cells. A p-value
uses `atol = 0` by default so very small tail probabilities retain relative
precision. Untyped fields and raw numbers in mixed cells compare exactly,
as do integer fields, missing values, column definitions and footnotes.
The module test uses the value supplied by the analysis regardless of how
many digits JASP prints.

Set defaults at the top of a test file, or override them for one table or a
column identified by its schema name. For example, an older module might
return its p-values in an ordinary numeric column named `p`.

```r
jaspTools::local_table_snapshot_tolerance(rtol = 1e-8, atol = 1e-12)

test_that("the model reports estimates and tail probabilities", {
  jaspTools::expect_snapshot_table(results$results$modelTable, "model",
    columns = list(p = list(atol = 0)))
})
```

A passing rerun keeps the accepted file byte-for-byte. When a table has a
reviewable change, the candidate retains the reference values in cells that
passed within tolerance. For an intentional refresh, run the expectation
with `refresh = TRUE` on the chosen reference platform and review the staged
changes before accepting them.

Generated tests call `preserveSnapshotReferences()` before replaying the
analysis, so an early error or skip retains the files needed for review. Use
the same call at the beginning of handwritten tests that run an analysis.
Regeneration reports previously named outputs that would lose their expectation.
When retiring an output intentionally, remove its expectation and reference
together in the reviewed change.

#### Reviewing a run

```r
result <- agentTestAnalysis("BinomialTest")
result$snapshotReviews
# Or build a report directly from a module checkout.
reviewSnapshots(modulePath = "../jaspFrequencies")
```

Each changed snapshot opens on its visual differences. Click the preview or
press **T** to cycle through the candidate, the baseline and the differences.
Tables show changes in place with footnotes directly below. Expand **Change
details** for exact values, numeric deltas and the first differing decimal
place. Figures open on a pixel overlay. HTML shows additions and edits inside
the rendered element.

Choose **Accept** or **Decline** at the top. To leave the decision to an agent,
write a note below the preview and choose **Comment only**. Select **Export
decisions** and give the JSON to the agent or apply it from R in the same
checkout. The export includes comments and structured table changes.

```r
feedback <- applySnapshotReview("snapshot-decisions.json", modulePath = "../jaspFrequencies")
feedback[, c("id", "status", "comment")]
```

An acceptance promotes the reviewed candidate, including a paired plot
structure candidate when present. A changed reference or candidate requires a
fresh review. A matching rerun clears the pending candidate. Use
`reviewSnapshots(modulePath, includeUnchanged = TRUE)` to browse the accepted
snapshot library. The offline report can also be generated from CI artifacts
by restoring the references and candidates in their module directory layout.

#### Try the included example

```r
source(system.file("examples/snapshot-review/demo.R", package = "jaspTools"))
demo <- snapshotReviewExample()                # four deliberate differences
clean <- snapshotReviewExample(changed = FALSE) # passing reference run
```

The example makes a temporary copy of its test suite, including a fifth-decimal
change, a column replacement, a footnote, an HTML summary and a plot. See the
[worked example](inst/examples/snapshot-review/README.md) and the
[JASP module testing guide](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/r-unit-test-guide.md).

## Known limitations:
- Click actions in the browser are not supported (e.g., "save as")
- State is ignored
