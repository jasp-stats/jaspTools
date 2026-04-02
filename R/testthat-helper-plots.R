#' Compares JASP plots in unit tests.
#'
#' This function compares a stored .svg of a plot, to the plot that is created when the tests are run.
#' If no visual reference (.svg) exists yet, \pkg{vdiffr} handles it like other visual snapshots.
#'
#' For \pkg{ggplot2} objects, a structural fallback snapshot is also maintained.
#' In interactive test runs, if that structural snapshot is missing, it is created automatically
#' (even when the visual comparison passes).
#'
#' To accept changed structural snapshots, use \code{testthat::snapshot_accept()} from the
#' package root after running tests.
#'
#'
#' @param test The plot object you wish to test (does not work well for non-ggplot2 objects).
#' @param name The name of the reference plot (a .svg stored in /tests/testthat/_snaps).
#' @param dir `r lifecycle::badge('deprecated')`
#'
#' @examples
#'
#' options <- analysisOptions("BinomialTest")
#' options$variables <- "contBinom"
#' options$descriptivesPlots <- TRUE
#' results <- runAnalysis("BinomialTest", "test.csv", options)

#' testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
#' expect_equal_plots(testPlot, "descriptives-1", dir = "BinomialTest")
#'
#' @param tolerance Optional numeric tolerance for the structural fallback
#'   comparison (default \code{1e-6}). Set to a larger value (e.g. \code{1e-3})
#'   for plots whose data vary across platforms (e.g. MCMC-based Bayesian plots).
#'
#' @export expect_equal_plots
expect_equal_plots <- function(test, name, dir = lifecycle::deprecated(), tolerance = NULL) {
  if (length(test) == 0) {
    expect(FALSE, getEmptyTestMsg("expect_equal_plots()"))
    return()
  }

  skip_if_grob(test)
  skip_if_recordedPlot(test)

  if (inherits(test, "jaspGraphsPlot")) {
    subplots <- test$subplots

    for (i in seq_along(subplots))
      expect_plot_with_fallback(paste(name, "subplot", i, sep = "-"), subplots[[i]], tolerance = tolerance)

  } else {
    if (inherits(test, "qgraph")) {
      qq <- test
      test <- function() plot(qq)
    }
    expect_plot_with_fallback(name, test, tolerance = tolerance)
  }
}

expect_plot_with_fallback <- function(name, test, tolerance = NULL) {
  result <- capture_vdiffr_expectation(name, test)
  if (isTRUE(result$passed)) {
    maybe_seed_ggplot_structure_snapshot(test, name)
    return(invisible(TRUE))
  }

  # Save the CI-generated SVG so it can be uploaded as an artifact for comparison
  save_failed_plot_svg(test, name)

  fallbackResult <- expect_doppelganger_fallback(test, name, vdiffr_result = result, tolerance = tolerance)
  if (isTRUE(fallbackResult$passed)) {
    warning("vdiffr mismatch for '", name, "' accepted by structural fallback.", call. = FALSE)
    testthat::succeed(paste0("vdiffr mismatch for '", name, "' accepted by fallback."))
    return(invisible(TRUE))
  }

  vdiffrMsg <- conditionMessage(result$exception)
  fallbackMsg <- build_fallback_failure_message(fallbackResult)
  testthat::fail(paste0(
    "vdiffr mismatch for '", name, "'.\n",
    "Original vdiffr failure: ", vdiffrMsg, "\n",
    "Fallback failure: ", fallbackMsg
  ))

  invisible(FALSE)
}

build_fallback_failure_message <- function(fallbackResult) {
  fallbackMsg <- fallbackResult$message

  fallbackExceptionMsg <- NULL
  if (!is.null(fallbackResult$exception) && inherits(fallbackResult$exception, "condition"))
    fallbackExceptionMsg <- conditionMessage(fallbackResult$exception)

  combinedFallbackMsg <- fallbackMsg
  if (!is.null(fallbackExceptionMsg) && nzchar(fallbackExceptionMsg)) {
    if (is.null(combinedFallbackMsg) || !nzchar(combinedFallbackMsg))
      combinedFallbackMsg <- fallbackExceptionMsg
    else
      combinedFallbackMsg <- paste0(combinedFallbackMsg, " (", fallbackExceptionMsg, ")")
  }

  structuralDiff <- get_last_structural_diff()
  if (!is.null(structuralDiff) && nzchar(structuralDiff)) {
    combinedFallbackMsg <- paste0(combinedFallbackMsg, "\n", structuralDiff)
  }

  if (is.null(combinedFallbackMsg) || !nzchar(combinedFallbackMsg))
    combinedFallbackMsg <- "<no fallback details available>"

  combinedFallbackMsg
}

maybe_seed_ggplot_structure_snapshot <- function(test, name) {
  if (!is_ggplot(test))
    return(invisible(FALSE))

  testthat::local_edition(3)

  snapshotName <- ggplot_structure_snapshot_name(name)
  snapshotPath <- snapshot_relative_path(snapshotName)
  ensure_snapshot_subdir(snapshotName)
  testthat::announce_snapshot_file(path = snapshotPath, name = snapshotName)

  updateMode <- should_update_ggplot_structure_snapshots()

  if (!updateMode && file.exists(snapshotPath))
    return(invisible(FALSE))

  if (!updateMode && !is_interactive_plot_snapshot_mode())
    return(invisible(FALSE))

  writeRes <- write_ggplot_structure_snapshot(test, snapshotName, snapshotPath, overwrite = updateMode)
  if (!isTRUE(writeRes$passed))
    return(invisible(FALSE))

  action <- if (updateMode) "Updated" else "Created"
  testthat::succeed(paste0(
    action, " ggplot structural snapshot for '",
    name,
    "'."
  ))
  invisible(TRUE)
}

capture_vdiffr_expectation <- function(name, test) {
  out <- list(passed = FALSE, exception = NULL)

  tryCatch(
    {
      suppressWarnings(vdiffr::expect_doppelganger(name, test))
      out$passed <- TRUE

      # In interactive mode, vdiffr silently accepts mismatches by writing a
      # .new.svg file. Detect this and treat it as a mismatch.
      newSvgName <- paste0(str_standardise_snapshot_name(name), ".new.svg")
      newSvgPath <- snapshot_relative_path(newSvgName)
      if (file.exists(newSvgPath)) {
        out$passed <- FALSE
        out$exception <- simpleError(paste0(
          "vdiffr mismatch for '", name, "' (detected via .new.svg in interactive mode)."
        ))
        unlink(newSvgPath)
      }

      out
    },
    expectation_failure = function(cnd) {
      out$exception <- cnd
      out
    },
    expectation_warning = function(cnd) {
      out$exception <- cnd
      out
    },
    error = function(cnd) {
      out$exception <- cnd
      out
    }
  )
}

#' @noRd
expect_doppelganger_fallback <- function(test, name, ..., tolerance = NULL) {
  if (is.function(test))
    return(expect_doppelganger_fallback.default(test, name, ..., tolerance = tolerance))

  # ggplot2 now prepends namespaced classes (e.g. "ggplot2::ggplot").
  # Strip prefixes so methods like *.ggplot are reachable.
  dispatchTest <- test
  cls <- class(dispatchTest)
  plainCls <- sub("^.*::", "", cls)
  class(dispatchTest) <- unique(c(plainCls, cls))

  UseMethod("expect_doppelganger_fallback", dispatchTest)
}

#' @noRd
#' @method expect_doppelganger_fallback default
#' @export
expect_doppelganger_fallback.default <- function(test, name, ...) {
  list(
    passed = FALSE,
    has_fallback = FALSE,
    exception = NULL,
    message = paste0(
      "No fallback expectation is implemented for class(es): ",
      paste(class(test), collapse = ", "),
      "."
    )
  )
}

#' @noRd
#' @method expect_doppelganger_fallback ggplot
#' @export
expect_doppelganger_fallback.ggplot <- function(test, name, vdiffr_result = NULL, ..., tolerance = NULL) {
  expect_equal_ggplot_structure(test, name, vdiffr_result = vdiffr_result, tolerance = tolerance)
}

expect_equal_ggplot_structure <- function(plot, name, vdiffr_result = NULL, tolerance = NULL) {
  testthat::local_edition(3)

  if (!is.null(tolerance))
    withr::local_options(jaspTools.plotStructure.tolerance = tolerance)

  snapshotName <- ggplot_structure_snapshot_name(name)
  snapshotPath <- snapshot_relative_path(snapshotName)
  ensure_snapshot_subdir(snapshotName)
  testthat::announce_snapshot_file(path = snapshotPath, name = snapshotName)

  buildRes <- build_ggplot_structure_snapshot(plot)
  if (!isTRUE(buildRes$passed)) {
    return(list(
      passed = FALSE,
      has_fallback = TRUE,
      exception = buildRes$exception,
      message = buildRes$message
    ))
  }

  tmpPath <- buildRes$tmpPath

  if (should_update_ggplot_structure_snapshots()) {
    hadSnapshot <- file.exists(snapshotPath)
    writeRes <- write_ggplot_structure_snapshot(plot, snapshotName, snapshotPath, overwrite = TRUE)
    if (!isTRUE(writeRes$passed))
      return(writeRes)

    action <- if (hadSnapshot) "updated" else "created"
    return(list(
      passed = TRUE,
      has_fallback = TRUE,
      exception = NULL,
      message = paste0(
        "ggplot structural snapshot for '",
        name,
        "' ",
        action,
        " in update mode."
      )
    ))
  }

  snapshotRes <- tryCatch(
    {
      testthat::expect_snapshot_file(
        tmpPath,
        name = snapshotName,
        cran = FALSE,
        compare = compare_ggplot_structure_snapshot
      )

      list(
        passed = TRUE,
        has_fallback = TRUE,
        exception = NULL,
        message = if (is.null(vdiffr_result) || isTRUE(vdiffr_result$passed)) {
          paste0("ggplot structural snapshot for '", name, "' passed.")
        } else {
          paste0("vdiffr mismatch for '", name, "' accepted by ggplot structural fallback.")
        }
      )
    },
    expectation_failure = function(cnd) {
      list(
        passed = FALSE,
        has_fallback = TRUE,
        exception = cnd,
        message = conditionMessage(cnd)
      )
    },
    error = function(cnd) {
      list(
        passed = FALSE,
        has_fallback = TRUE,
        exception = cnd,
        message = conditionMessage(cnd)
      )
    }
  )

  snapshotRes
}

is_interactive_plot_snapshot_mode <- function() {
  interactive() && !identical(tolower(Sys.getenv("CI", "false")), "true")
}

build_ggplot_structure_snapshot <- function(plot) {
  current <- tryCatch(
    extract_ggplot_structure(plot),
    error = function(cnd) cnd
  )

  if (inherits(current, "error")) {
    return(list(
      passed = FALSE,
      exception = current,
      message = conditionMessage(current)
    ))
  }

  tmpPath <- tempfile(pattern = "ggplot-structure-", fileext = ".rds")
  saveRDS(current, tmpPath)

  list(
    passed = TRUE,
    exception = NULL,
    message = NULL,
    tmpPath = tmpPath
  )
}

write_ggplot_structure_snapshot <- function(plot, snapshotName, snapshotPath, overwrite = FALSE) {
  ensure_snapshot_subdir(snapshotName)
  buildRes <- build_ggplot_structure_snapshot(plot)
  if (!isTRUE(buildRes$passed)) {
    return(list(
      passed = FALSE,
      has_fallback = TRUE,
      exception = buildRes$exception,
      message = buildRes$message
    ))
  }

  writeRes <- tryCatch(
    file.copy(buildRes$tmpPath, snapshotPath, overwrite = overwrite),
    error = function(cnd) cnd
  )

  if (!isTRUE(writeRes)) {
    err <- if (inherits(writeRes, "error")) {
      writeRes
    } else {
      simpleError(paste0("Failed to write snapshot file: ", snapshotPath))
    }

    return(list(
      passed = FALSE,
      has_fallback = TRUE,
      exception = err,
      message = conditionMessage(err)
    ))
  }

  list(
    passed = TRUE,
    has_fallback = TRUE,
    exception = NULL,
    message = NULL
  )
}

should_update_ggplot_structure_snapshots <- function() {
  isTRUE(getOption("jaspTools.plotStructure.update", FALSE)) ||
    identical(tolower(Sys.getenv("JASP_PLOT_STRUCTURE_UPDATE", "false")), "true")
}

ggplot_structure_snapshot_path <- function(name) {
  snapshot_relative_path(ggplot_structure_snapshot_name(name))
}

ggplot_structure_snapshot_name <- function(name) {
  file <- paste0(str_standardise_snapshot_name(name), ".rds")
  file.path("reference_plotobject", file)
}

str_standardise_snapshot_name <- function(x, sep = "-") {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]", sep, x)
  x <- gsub(paste0(sep, sep, "+"), sep, x)
  x <- gsub(paste0("^", sep, "|", sep, "$"), "", x)
  x
}

# ggplot2 >= 3.5.2 prepends namespaced classes (e.g. "ggplot2::ggplot"),
# so plain inherits(x, "ggplot") may return FALSE.
is_ggplot <- function(x) {
  "ggplot" %in% sub("^.*::", "", class(x))
}

# Save the plot as SVG in the _snaps folder when vdiffr fails.
# This allows CI to upload the generated SVG as an artifact for visual comparison.
save_failed_plot_svg <- function(test, name) {
  tryCatch({
    newSvgName <- paste0(str_standardise_snapshot_name(name), ".new.svg")
    svgPath    <- snapshot_relative_path(newSvgName)
    ensure_snapshot_subdir(newSvgName)

    if (is_ggplot(test)) {
      svglite::svglite(svgPath, width = 7, height = 5)
      print(test)
      dev.off()
    } else if (is.function(test)) {
      svglite::svglite(svgPath, width = 7, height = 5)
      test()
      dev.off()
    }
  }, error = function(e) {
    # silently ignore — saving is best-effort
  })
}

# Environment to store the last structural comparison details, so they can be
# retrieved and included in failure messages (message() output is lost on CI).
.structuralDiffEnv <- new.env(parent = emptyenv())
.structuralDiffEnv$lastDiff <- NULL

# Recursively strip waiver objects from a snapshot structure so that
# snapshots created with older code (which stored waivers) can be compared
# with snapshots created by the current code (which normalises them to NULL).
normalize_snapshot_structure <- function(x) {
  if (inherits(x, "waiver"))
    return(NULL)
  if (is.list(x))
    return(lapply(x, normalize_snapshot_structure))
  x
}

compare_ggplot_structure_snapshot <- function(old, new) {
  .structuralDiffEnv$lastDiff <- NULL

  oldStructure <- tryCatch(readRDS(old), error = function(cnd) cnd)
  newStructure <- tryCatch(readRDS(new), error = function(cnd) cnd)

  if (inherits(oldStructure, "error") || inherits(newStructure, "error")) {
    .structuralDiffEnv$lastDiff <- "Could not read one or both .rds snapshot files."
    return(FALSE)
  }

  # Normalize both sides so old snapshots (with waiver objects) match new ones
  oldStructure <- normalize_snapshot_structure(oldStructure)
  newStructure <- normalize_snapshot_structure(newStructure)

  tol <- getOption("jaspTools.plotStructure.tolerance", 1e-6)
  result <- all.equal(
    oldStructure,
    newStructure,
    tolerance = tol,
    check.attributes = FALSE
  )

  if (!isTRUE(result)) {
    diffLines <- utils::head(result, 10)
    diffSummary <- paste0(
      "Structural fallback mismatch (tolerance = ", format(tol, scientific = TRUE), "):\n  ",
      paste(diffLines, collapse = "\n  "),
      if (length(result) > 10) paste0("\n  ... and ", length(result) - 10, " more differences") else ""
    )
    .structuralDiffEnv$lastDiff <- diffSummary
  }

  isTRUE(result)
}

get_last_structural_diff <- function() {
  .structuralDiffEnv$lastDiff
}

get_snapshotter <- function() {
  x <- getOption("testthat.snapshotter")
  if (is.null(x))
    return(NULL)
  if (!x$is_active())
    return(NULL)
  x
}

snapshot_relative_path <- function(name) {
  snapshotter <- get_snapshotter()
  if (is.null(snapshotter))
    return(file.path("tests", "testthat", "_snaps", name))

  file.path(snapshotter$snap_dir, snapshotter$file, name)
}

ensure_snapshot_subdir <- function(name) {
  path <- snapshot_relative_path(name)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

extract_ggplot_structure <- function(plot) {
  if (!is_ggplot(plot))
    stop("`plot` must inherit from 'ggplot'.")

  built <- ggplot2::ggplot_build(plot)

  list(
    labels = normalize_named_list(plot$labels),
    layer_data = lapply(built$data, normalize_data_frame_for_snapshot),
    layer_spec = lapply(plot$layers, extract_layer_spec),
    layout = extract_layout_spec(plot, built),
    scales = extract_scale_spec(plot)
  )
}

normalize_data_frame_for_snapshot <- function(df) {
  if (!is.data.frame(df))
    return(df)

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df) > 0)
    df <- df[, sort(names(df)), drop = FALSE]

  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.factor(col))
      df[[nm]] <- as.character(col)
    if (inherits(col, "POSIXct") || inherits(col, "POSIXt"))
      df[[nm]] <- format(col, tz = "UTC", usetz = TRUE)
    if (is.numeric(col))
      df[[nm]] <- signif(col, getOption("jaspTools.plotStructure.signif", 10))
  }

  rownames(df) <- NULL
  attributes(df) <- attributes(df)[intersect(names(attributes(df)), c("names", "class", "row.names"))]
  df
}

normalize_named_list <- function(x) {
  if (is.null(x) || inherits(x, "waiver") || is.function(x))
    return(NULL)

  if (is.list(x) && !is.null(names(x))) {
    x <- x[sort(names(x))]
    x <- lapply(x, normalize_named_list)
    return(x)
  }

  if (is.list(x))
    return(lapply(x, normalize_named_list))

  if (is.factor(x))
    return(as.character(x))

  if (is.numeric(x))
    return(signif(x, getOption("jaspTools.plotStructure.signif", 10)))

  x
}

extract_layer_spec <- function(layer) {
  list(
    geom = class(layer$geom)[1],
    stat = class(layer$stat)[1],
    position = class(layer$position)[1],
    mapping = if (is.null(layer$mapping)) NULL else sort(names(layer$mapping)),
    aes_params = normalize_named_list(layer$aes_params),
    stat_params = normalize_named_list(layer$stat_params)
  )
}

extract_layout_spec <- function(plot, built) {
  panelLayout <- NULL
  if (!is.null(built$layout$layout) && is.data.frame(built$layout$layout)) {
    panelLayout <- built$layout$layout
    keep <- intersect(c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"), names(panelLayout))
    panelLayout <- panelLayout[, keep, drop = FALSE]
    panelLayout <- normalize_data_frame_for_snapshot(panelLayout)
  }

  list(
    coord = class(plot$coordinates)[1],
    facet = class(plot$facet)[1],
    panel_layout = panelLayout
  )
}

extract_scale_spec <- function(plot) {
  scales <- tryCatch(plot$scales$scales, error = function(e) list())
  lapply(scales, function(scale) {
    transName <- NULL
    # ggplot2 >= 3.5.0 renamed $trans to $transform; in S7 builds
    # $transform may be the transformation *function* rather than an object.
    transObj <- tryCatch(scale$trans, error = function(e) NULL)
    if (is.null(transObj))
      transObj <- tryCatch(scale$transform, error = function(e) NULL)
    if (!is.null(transObj) && !is.function(transObj) && !is.null(transObj$name))
      transName <- transObj$name

    scaleAes <- tryCatch(scale$aesthetics, error = function(e) NULL)

    list(
      class = class(scale)[1],
      aesthetics = if (is.null(scaleAes)) NULL else sort(scaleAes),
      trans = transName
    )
  })
}

skip_if_grob <- function(test) {
  if (inherits(test, "grob"))
    skip("Cannot reliably test matrix plots (they fail Windows <-> OSX)")
}

skip_if_recordedPlot <- function(test) {
  if (inherits(test, "recordedplot"))
    skip("Recorded plots are skipped until the scaling of these plots is fixed")
}

getEmptyTestMsg <- function(expectationFn) {
  error <- getErrorMsgFromLastResults()
  if (!is.null(error[["type"]])) {
    if (error[["type"]] == "validationError" || error[["type"]] == "fatalError")
      msg <- paste0("The `test` argument provided to `", expectationFn, "` is empty. Likely reason: the last run of jaspTools exited with a ", error[["type"]], ":\n\n", error[["message"]])
    else if (error[["type"]] == "localError")
      msg <- paste0("The `test` argument provided to `", expectationFn,"` is empty. Likely reasons: (1) the path to the results in the unit test is not correct, or (2) one of the following errors in the results interfered with the test:\n\n", error[["message"]])
  } else {
    msg <- paste0("The `test` argument provided to `", expectationFn,"` is empty. Likely reason: the path to the results in the unit test is not correct.")
  }

  return(msg)
}
