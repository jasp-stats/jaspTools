# jaspTools Development Guide

## Project Overview

jaspTools is an R package that enables JASP developers to preview, debug, and test JASP analyses locally without rebuilding the entire JASP application. It replicates the JASP runtime environment in R, including RCPP bridges, data handling, and state management.

## Architecture

### Three-Environment System

jaspTools uses specialized environments for variable scoping:

1. **`.pkgenv$internal`**: Internal runtime state (dataset, state, module MD5 checksums). Access via `.setInternal()` and `.getInternal()` in `R/pkg-settings.R`.
2. **`.pkgenv$pkgOptions`**: User-configurable settings (module paths, HTML directory, data directories). Access via `setPkgOption()` and `getPkgOption()`.
3. **`.GlobalEnv`**: RCPP bridge functions that JASP analyses expect globally (`.ppi`, `.baseCitation`, `.readDatasetToEndNative`, etc.). Injected by `.insertRbridgeIntoEnv()` in `R/rbridge.R`.

### Analysis Execution Flow

1. **Setup**: `setupJaspTools()` fetches dependencies (jaspBase, jaspGraphs, datasets, HTML resources) and validates paths
2. **Options**: `analysisOptions()` parses QML files, JASP files, or JSON to generate option lists
3. **Runtime Init**: `initAnalysisRuntime()` in `R/run.R` sets up dataset, state, and global RCPP masks
4. **Execution**: `runAnalysis()` calls `jaspBase::runJaspResults()` with the analysis function
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

### Testing Workflow

```r
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

`readQML()` in `R/options-parser-qml.R` strips comments, whitespace, and newlines, then uses regex to extract QML form elements. Supports `IntegerField`, `CheckBox`, `DropDown`, `RadioButtonGroup`, etc. Static elements like `SetSeed` and `BayesFactorType` inject default options.

### State Management

**State is ignored** in jaspTools (noted in README limitations). State from JASP files is stored in `.internal` but not persisted between runs. Analyses should be stateless or handle missing state gracefully.

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
- **testthat**: Unit testing framework (requires ≥3.0.3)

Check versions with `.checkUpdatesJaspCorePkgs()` on package load.

## File Organization

- `R/run.R`: Analysis execution, RCPP mask setup, JSON conversion
- `R/test.R`: Testing infrastructure, `testAnalysis()`, `testAll()`
- `R/options.R`: Option parsing from QML/JASP/JSON
- `R/rbridge.R`: RCPP bridge replacements (`.readDatasetToEndNative`, `.requestTempFileNameNative`, etc.)
- `R/pkg-setup.R`: Initial setup, dependency fetching
- `R/utils.R`: Module path resolution, validation, helper functions
- `R/test-generator.R`: Auto-generate test expectations from results
- `R/testthat-helper-*.R`: Custom `expect_equal_tables()` and `expect_equal_plots()`

## Debugging Tips

- Use `view = FALSE` in `runAnalysis()` to suppress browser output
- Set `quiet = FALSE` to see analysis warnings/messages
- Check `.getInternal("dataset")` to inspect loaded data
- Examine `getPkgOption("module.dirs")` to verify module paths
- Use `getModulePathFromRFunction("FunctionName")` to locate analysis source
- Enable JASP logging (Preferences > Advanced) to capture Qt-generated JSON for `analysisOptions()`
