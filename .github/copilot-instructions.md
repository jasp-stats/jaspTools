# jaspTools Development Guide

## Project Overview

jaspTools is an R package that enables JASP developers to preview, debug, and test JASP analyses locally without rebuilding the entire JASP application. It orchestrates the local JASP runtime in R while delegating native QML, saved `.jasp`, dataset encoding, and result-decoding semantics to `jaspSyntax` and Desktop.

## Architecture

### Three-Environment System

jaspTools uses specialized environments for variable scoping:

1. **`.pkgenv$internal`**: Internal runtime state (dataset, state, module MD5 checksums). Access via `.setInternal()` and `.getInternal()` in `R/pkg-settings.R`.
2. **`.pkgenv$pkgOptions`**: User-configurable settings (module paths, HTML directory, data directories). Access via `setPkgOption()` and `getPkgOption()`.
3. **`.GlobalEnv`**: RCPP bridge functions that JASP analyses expect globally (`.ppi`, `.baseCitation`, `.readDatasetToEndNative`, etc.). Injected by `.insertRbridgeIntoEnv()` in `R/rbridge.R`.

### Analysis Execution Flow

1. **Setup**: `setupJaspTools()` fetches dependencies (jaspBase, jaspSyntax, jaspGraphs, datasets, HTML resources) and validates paths
2. **Options**: `analysisOptions()` reads QML defaults and saved `.jasp` options through `jaspSyntax`; JSON requests are parsed directly
3. **Runtime Init**: `initAnalysisRuntime()` in `R/run.R` sets up dataset, state, and global RCPP masks
4. **Execution**: `runAnalysis()` resolves the module QML through `jaspSyntax`, then calls `jaspBase::runWrappedAnalysis()` / `runJaspResults()`
5. **Output**: Results are converted to JSON and optionally displayed via `view()` using JASP's HTML/JS/CSS

**Critical**: S3 methods from `common.R` are temporarily exported to `.GlobalEnv` during analysis execution (see `Developers-note.md` "Handling of S3 methods").

## Essential Workflows

### Initial Setup

```r
library(jaspTools)
setupJaspTools()  # Interactive prompts guide you through dependency installation
setPkgOption("module.dirs", "path/to/jaspModule")  # Point to your JASP module(s)
```

**Module Requirements**: JASP modules must have `DESCRIPTION`, `NAMESPACE`, and `inst/Description.qml` files.

### Running Analyses

```r
# Method 1: From QML (generates default options)
options <- analysisOptions("BinomialTest")
options$variables <- "contBinom"
runAnalysis("BinomialTest", "debug.csv", options)

# Method 2: From .jasp file (preserves user settings)
options <- analysisOptions("~/path/to/file.jasp")[[1]]
runAnalysis(dataset="debug.csv", options=options)

# Method 3: From Qt log JSON (real-time from JASP)
# Enable "Log to file" in JASP > Preferences > Advanced
# Copy JSON from Engine 1.log under "Engine::receiveAnalysisMessage:"
options <- analysisOptions('{"name": "BinomialTest", "options": {...}}')
runAnalysis("BinomialTest", "debug.csv", options)
```

**Dataset Handling**: Pass data.frame, matrix, string path, or JASP dataset name (e.g., "debug.csv"). Bundled datasets are in `inst/extdata/`.

### Extracting Data from JASP Files

```r
# Extract dataset from a saved .jasp file
dataset <- extractDatasetFromJASPFile("path/to/analysis.jasp")

# Extract options from a .jasp file
options <- analysisOptions("path/to/analysis.jasp")  # Returns list if multiple analyses

# For single-analysis files, options are returned directly
# For multi-analysis files, access by index: options[[1]], options[[2]], etc.
```

### Native Option and Dataset Semantics

Do not reimplement option encoding, type coercion, QML parsing, or saved `.jasp`
dataset reconstruction in jaspTools. Those semantics belong to `jaspSyntax` and
Desktop. jaspTools should pass saved options and extracted datasets into the
bridge and let `jaspSyntax` return the runtime-ready dataset, column mapping,
and decoded results.

```r
savedOptions <- analysisOptions("path/to/file.jasp")
dataset <- extractDatasetFromJASPFile("path/to/file.jasp")

runAnalysis("AnalysisName", dataset, savedOptions)
```

Use `analysisRuntimeOptions()` only for inspecting the backend-prepared option
shape. Do not feed those options back into `runAnalysis()`, because that would
prepare an already-prepared option payload a second time.

### Generating Tests from JASP Example Files

Use `makeTestsFromExamples()` to automatically generate test files from JASP example files:
 
```r
# Generate tests from all source folders (library, verified, other)
# Uses working directory as module by default
makeTestsFromExamples()

# Specify the module directory explicitly
makeTestsFromExamples(module.dir = "path/to/jaspModule")

# Only generate from the 'library' source folder
makeTestsFromExamples(source = "library")

# Import JASP files from external path (copies to jaspfiles/other/ and generates tests)
makeTestsFromExamples(path = "path/to/jasp/files", module.dir = "path/to/module")

# Overwrite existing test files (skips verified by default)
makeTestsFromExamples(overwrite = TRUE)

# Overwrite including verified (prompts for confirmation)
makeTestsFromExamples(source = c("library", "verified", "other"), overwrite = TRUE)

# Sanitize filenames (replace spaces/special chars with hyphens)
makeTestsFromExamples(sanitize = TRUE)
```

**Source Folders**: JASP example files are stored in `tests/testthat/jaspfiles/{library,verified,other}/`. The `source` argument controls which folders to process:
- `"library"` - JASP files from the analysis library
- `"verified"` - Manually verified/curated test files
- `"other"` - Other/imported JASP files (default target for `path` imports)

**Defaults**: When `overwrite = FALSE` (default), all three sources are processed. When `overwrite = TRUE`, only `"library"` and `"other"` are processed to protect verified tests.

Generated test files:
- Named `test-{source}-{filename}.R` in `tests/testthat/` (e.g., `test-library-MyAnalysis.R`)
- Load options and data from JASP files at runtime via `testthat::test_path("jaspfiles", "{source}", "file.jasp")`
- Include table expectations via `expect_equal_tables()`
- Include plot expectations via `expect_equal_plots()` with unique names: `analysis-1_figure-1_plot-title`

### Testing Workflow

```r
# Agent-optimized (preferred -- compact output, returns queryable result object)
x <- agentTestAll()                # All modules in module.dirs
x <- agentTestAnalysis("BinomialTest")  # Specific analysis

# Human-oriented (verbose output)
testAnalysis("BinomialTest")  # Single analysis
testAll()                      # All modules in module.dirs
testAll(onlyPlots = TRUE)      # Only plot tests (faster for vdiffr)
```

**Test Structure**: Tests live in `module/tests/testthat/test-analysisName.R`. Use custom expectations:

- `expect_equal_tables(test, ref)`: Compares table data with tolerance `signif(round(x, 4), 4)`. Customize via `options("jaspRoundToPrecision")`.
- `expect_equal_plots(test, name)`: Uses `vdiffr` for visual regression. Requires `Sys.setenv("NOT_CRAN" = "true")`.

**Test Generation**: Run `runAnalysis(..., makeTests = TRUE)` to auto-generate test code from results.

### Generating Tests from Results

```r
# Create reference test expectations from analysis output
options <- analysisOptions("BinomialTest")
options$variables <- "contBinom"
runAnalysis("BinomialTest", "debug.csv", options, makeTests = TRUE)
# Copy printed test code to tests/testthat/test-BinomialTest.R
```

## Key Conventions

### Module Auto-Reinstallation

When `reinstall.modules = TRUE` (default), jaspTools tracks MD5 checksums of `R/`, `NAMESPACE`, and `DESCRIPTION` files. Changes trigger automatic `remotes::install_local()` before analysis execution. This ensures the *installed* package version matches your source.

### Function Name Resolution

Analysis functions may have `Internal` suffixes. `findCorrectFunction()` searches for `FunctionNameInternal` first, falling back to `FunctionName`. Always reference by the user-facing name.

### QML Parsing

jaspTools does not parse QML directly. Use `jaspSyntax::readDefaultAnalysisOptions()`,
`jaspSyntax::readAnalysisOptionsFromJaspFile()`, and
`jaspSyntax::resolveAnalysisQml()` for native option semantics.

### State Management

jaspTools keeps a standalone state file for `jaspBase` replay and decodes
returned plot state after analysis execution. Browser-only interactions are not
replayed.

## Common Pitfalls

1. **Forgetting `setupJaspTools()`**: All resource paths and dependencies are configured here. Without it, `module.dirs` is empty and analyses fail to load.
2. **Missing `module.dirs`**: If not set and CWD isn't a module, you'll hit "jaspTools needs to know what module to obtain resources from."
3. **QML vs. JSON vs. JASP file confusion**: `analysisOptions()` accepts all three but requires exact formats. JSON must include outer `{}`, JASP files need `.jasp` extension.
4. **Test environment detection**: `insideTestEnvironment()` checks for `testthat` in the call stack. Tests automatically set `view = FALSE` and `quiet = TRUE`.
5. **RNG differences**: Use `options(jaspLegacyRngKind = TRUE/FALSE)` to match JASP's RNG behavior. `fixRNGForTesting()` is deprecated.

## External Dependencies

- **jaspBase**: Core JASP R infrastructure, provides `runJaspResults()` and utility functions
- **jaspGraphs**: Plotting system for JASP-compatible graphics
- **jaspResults**: Legacy state container (now merged into jaspBase)
- **vdiffr**: Visual regression testing for plots
- **testthat**: Unit testing framework (requires >=3.2.2)

Check versions with `.checkUpdatesJaspCorePkgs()` on package load.

## File Organization

- `R/run.R`: Analysis execution, RCPP mask setup, native QML provenance, JSON conversion, state-file replay
- `R/test.R`: Testing infrastructure, `testAnalysis()`, `testAll()`
- `R/test-agent.R`: Agent-friendly test wrappers, `agentTestAll()`, `agentTestAnalysis()`
- `R/options.R`: Option parsing from QML/JASP/JSON. Cross-platform path handling for `.jasp` files.
- `R/dataset.R`: Dataset loading orchestration and `jaspSyntax` bridge wrappers
- `R/rbridge.R`: RCPP bridge replacements (`.readDatasetToEndNative`, `.requestTempFileNameNative`, etc.)
- `R/pkg-setup.R`: Initial setup, dependency fetching
- `R/utils.R`: Module path resolution, validation, helper functions
- `R/test-generator.R`: Auto-generate test expectations from results, `makeTestsFromExamples()`. JASP files live in `tests/testthat/jaspfiles/{library,verified,other}/`; generated tests are named `test-{source}-{name}.R`.
- `R/testthat-helper-*.R`: Custom `expect_equal_tables()` and `expect_equal_plots()`

## Debugging Tips

- Use `view = FALSE` in `runAnalysis()` to suppress browser output
- Set `quiet = FALSE` to see analysis warnings/messages
- Check `.getInternal("dataset")` to inspect loaded data
- Examine `getPkgOption("module.dirs")` to verify module paths
- Use `getModulePathFromRFunction("FunctionName")` to locate analysis source
- Enable JASP logging (Preferences > Advanced) to capture Qt-generated JSON for `analysisOptions()`
