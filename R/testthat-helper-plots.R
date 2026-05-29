#' Compares JASP plots in unit tests.
#'
#' This function compares a stored .svg of a plot, to the plot that is created when the tests are run.
#' If no visual reference (.svg) exists yet, \pkg{vdiffr} handles it like other visual snapshots.
#'
#' For \pkg{ggplot2} objects, a structural fallback snapshot is also maintained.
#' When a visual comparison passes, missing structural snapshots are created in
#' interactive test runs, in update mode (\code{options(jaspTools.plotStructure.update = TRUE)}
#' or \code{JASP_PLOT_STRUCTURE_UPDATE=true}), and for newly generated visual snapshots.
#' They are not created after a visual mismatch against an existing visual snapshot.
#'
#' If a visual mismatch is accepted by the structural fallback, inspect and accept
#' the generated visual snapshot with \code{manageTestPlots()} so future runs do
#' not keep using the slower fallback.
#' Plot snapshot rendering triggers garbage collection by default to avoid
#' accumulated \pkg{ggplot2} build state slowing large generated test files; set
#' \code{options(jaspTools.plotSnapshot.gc = FALSE)} to disable this.
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
  freshVisual <- is_fresh_visual_snapshot(result)

  if (isTRUE(result$passed)) {
    maybe_seed_ggplot_structure_snapshot(test, name, force = freshVisual)
    return(invisible(TRUE))
  }

  if (freshVisual)
    maybe_seed_ggplot_structure_snapshot(test, name, force = TRUE)

  # Save the CI-generated SVG so it can be uploaded as an artifact for comparison
  save_failed_plot_svg(test, name, vdiffr_result = result)

  if (freshVisual)
    fail_fresh_visual_snapshot(name, result)

  fallbackResult <- expect_doppelganger_fallback(test, name, vdiffr_result = result, tolerance = tolerance)
  if (isTRUE(fallbackResult$passed)) {
    warning(build_structural_fallback_review_message(name, result), call. = FALSE)
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

fail_fresh_visual_snapshot <- function(name, vdiffr_result) {
  vdiffrMsg <- if (!is.null(vdiffr_result$exception))
    conditionMessage(vdiffr_result$exception)
  else
    "<no vdiffr details available>"

  testthat::fail(paste0(
    "vdiffr created a new visual snapshot for '",
    name,
    "'. Review and accept the visual snapshot before structural fallback can be used. ",
    "Original vdiffr failure: ",
    vdiffrMsg
  ))
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

maybe_seed_ggplot_structure_snapshot <- function(test, name, force = FALSE) {
  if (!is_ggplot(test))
    return(invisible(FALSE))

  testthat::local_edition(3)

  snapshotName <- ggplot_structure_snapshot_name(name)
  snapshotPath <- snapshot_relative_path(snapshotName)

  updateMode <- should_update_ggplot_structure_snapshots()

  if (!updateMode && file.exists(snapshotPath))
    return(invisible(FALSE))

  if (!updateMode && !isTRUE(force) && !is_interactive_plot_snapshot_mode())
    return(invisible(FALSE))

  ensure_snapshot_subdir(snapshotName)
  testthat::announce_snapshot_file(path = snapshotPath, name = snapshotName)

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
  maybe_collect_plot_snapshot_garbage()

  svgName <- paste0(str_standardise_snapshot_name(name), ".svg")
  newSvgName <- paste0(str_standardise_snapshot_name(name), ".new.svg")
  svgPath <- snapshot_relative_path(svgName)
  newSvgPath <- snapshot_relative_path(newSvgName)
  newSvgBefore <- snapshot_file_state(newSvgPath)

  out <- list(
    passed = FALSE,
    exception = NULL,
    svg_path = svgPath,
    new_svg_path = newSvgPath,
    reference_existed = file.exists(svgPath),
    new_svg_current = FALSE
  )

  tryCatch(
    {
      suppressWarnings(vdiffr::expect_doppelganger(name, test))
      out$passed <- TRUE
      out <- update_vdiffr_result_paths(out, newSvgBefore)

      # In interactive mode, vdiffr silently accepts mismatches by writing a
      # .new.svg file. Detect this and treat it as a mismatch.
      if (isTRUE(out$new_svg_current)) {
        out$passed <- FALSE
        out$exception <- simpleError(paste0(
          "vdiffr mismatch for '", name, "' (detected via .new.svg in interactive mode)."
        ))
      }

      out
    },
    expectation_failure = function(cnd) {
      out$exception <- cnd
      out <- update_vdiffr_result_paths(out, newSvgBefore)
      out
    },
    expectation_warning = function(cnd) {
      out$exception <- cnd
      out <- update_vdiffr_result_paths(out, newSvgBefore)
      out
    },
    error = function(cnd) {
      out$exception <- cnd
      out <- update_vdiffr_result_paths(out, newSvgBefore)
      out
    }
  )
}

maybe_collect_plot_snapshot_garbage <- function() {
  if (isFALSE(getOption("jaspTools.plotSnapshot.gc", TRUE)))
    return(invisible(FALSE))

  invisible(gc(FALSE))
}

update_vdiffr_result_paths <- function(result, newSvgBefore) {
  result$new_svg_current <- snapshot_file_changed(newSvgBefore, result$new_svg_path)
  result
}

snapshot_file_state <- function(path) {
  if (!file.exists(path))
    return(list(exists = FALSE, size = NA_real_, mtime = as.POSIXct(NA), bytes = NULL))

  info <- file.info(path)
  size <- info$size
  bytes <- tryCatch(readBin(path, what = "raw", n = size), error = function(cnd) NULL)

  list(
    exists = TRUE,
    size = size,
    mtime = info$mtime,
    bytes = bytes
  )
}

snapshot_file_changed <- function(before, path) {
  after <- snapshot_file_state(path)

  if (!isTRUE(after$exists))
    return(FALSE)
  if (!isTRUE(before$exists))
    return(TRUE)
  if (!identical(before$size, after$size))
    return(TRUE)
  if (!identical(before$mtime, after$mtime))
    return(TRUE)
  if (!identical(before$bytes, after$bytes))
    return(TRUE)

  FALSE
}

is_visual_failure_result <- function(vdiffr_result) {
  !is.null(vdiffr_result) && !isTRUE(vdiffr_result$passed)
}

is_fresh_visual_snapshot <- function(vdiffr_result) {
  !is.null(vdiffr_result) &&
    !isTRUE(vdiffr_result$reference_existed) &&
    (file.exists(vdiffr_result$svg_path) || isTRUE(vdiffr_result$new_svg_current))
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
  clear_last_structural_diff()

  if (!is.null(tolerance))
    withr::local_options(jaspTools.plotStructure.tolerance = tolerance)

  snapshotName <- ggplot_structure_snapshot_name(name)
  snapshotPath <- snapshot_relative_path(snapshotName)
  visualFailure <- is_visual_failure_result(vdiffr_result)

  if (visualFailure && !file.exists(snapshotPath)) {
    return(list(
      passed = FALSE,
      has_fallback = TRUE,
      exception = NULL,
      message = paste0(
        "No ggplot structural snapshot exists for '",
        name,
        "'. Not creating one because the visual comparison failed."
      )
    ))
  }

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

  if (!visualFailure && should_update_ggplot_structure_snapshots()) {
    hadSnapshot <- file.exists(snapshotPath)
    writeRes <- write_ggplot_structure_snapshot(plot, snapshotName, snapshotPath, overwrite = TRUE, buildRes = buildRes)
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

  if (visualFailure) {
    passed <- compare_ggplot_structure_snapshot(snapshotPath, tmpPath)
    return(list(
      passed = isTRUE(passed),
      has_fallback = TRUE,
      exception = NULL,
      message = if (isTRUE(passed)) {
        paste0("vdiffr mismatch for '", name, "' accepted by ggplot structural fallback.")
      } else {
        paste0("ggplot structural snapshot for '", name, "' failed.")
      }
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

write_ggplot_structure_snapshot <- function(plot, snapshotName, snapshotPath, overwrite = FALSE, buildRes = NULL) {
  ensure_snapshot_subdir(snapshotName)
  if (is.null(buildRes))
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
save_failed_plot_svg <- function(test, name, vdiffr_result = NULL) {
  tryCatch({
    newSvgName <- paste0(str_standardise_snapshot_name(name), ".new.svg")
    svgPath <- if (!is.null(vdiffr_result) && !is.null(vdiffr_result$new_svg_path))
      vdiffr_result$new_svg_path
    else
      snapshot_relative_path(newSvgName)

    if (isTRUE(vdiffr_result$new_svg_current) && file.exists(svgPath))
      return(invisible(svgPath))

    ensure_snapshot_subdir(newSvgName)

    if (is_ggplot(test)) {
      svglite::svglite(svgPath, width = 7, height = 5)
      print(test)
      grDevices::dev.off()
    } else if (is.function(test)) {
      svglite::svglite(svgPath, width = 7, height = 5)
      test()
      grDevices::dev.off()
    }
  }, error = function(e) {
    # silently ignore — saving is best-effort
  })
}

build_structural_fallback_review_message <- function(name, vdiffr_result = NULL) {
  newSvgPath <- if (!is.null(vdiffr_result)) vdiffr_result$new_svg_path else NULL
  reviewPath <- if (!is.null(newSvgPath) && file.exists(newSvgPath))
    newSvgPath
  else
    "<no .new.svg was saved>"

  paste0(
    "vdiffr mismatch for '",
    name,
    "' accepted by structural fallback. Review the visual change at ",
    reviewPath,
    " and accept the visual snapshot with jaspTools::manageTestPlots() or ",
    "testthat::snapshot_review(). Until the visual snapshot is accepted, ",
    "future runs will keep using the slower structural fallback."
  )
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

clear_last_structural_diff <- function() {
  .structuralDiffEnv$lastDiff <- NULL
  invisible(NULL)
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
    if (is.list(col)) {
      df[[nm]] <- NULL  # drop list columns (e.g., ggraph embeds igraph objects)
      next
    }
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

normalize_named_list <- function(x, .depth = 0L) {
  if (is.null(x) || inherits(x, "waiver") || is.function(x))
    return(NULL)

  if (.depth > 10L)
    return("<truncated: max depth exceeded>")

  if (is.environment(x) || typeof(x) == "externalptr" || inherits(x, "igraph"))
    return(paste0("<", typeof(x), ">"))

  if (inherits(x, "R6") || inherits(x, "refObjectGenerator") || isS4(x))
    return(paste0("<", paste(class(x), collapse = "/"), ">"))

  if (inherits(x, "vctrs_rcrd"))
    return(paste0("<", paste(class(x), collapse = "/"), ">"))

  if (is.call(x) || is.expression(x) || is.symbol(x))
    return(deparse(x))

  if (is.list(x) && !is.null(names(x))) {
    x <- x[sort(names(x))]
    x <- lapply(x, normalize_named_list, .depth = .depth + 1L)
    return(x)
  }

  if (is.list(x))
    return(lapply(x, normalize_named_list, .depth = .depth + 1L))

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
