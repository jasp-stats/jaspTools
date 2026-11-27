#' Compares JASP plots in unit tests.
#'
#' This function compares a stored .svg of a plot, to the plot that is created when the tests are run.
#' New and changed figures are saved as pending \code{.new.svg} files. Use
#' \code{reviewSnapshots(modulePath)} to inspect and accept them.
#'
#' Plot recipes are materialized before they are passed to \pkg{vdiffr}. For a
#' \pkg{ggplot2} figure, the reviewer accepts its structural snapshot together
#' with its SVG. An existing structural reference can satisfy the test when the
#' SVG changes, and the visual change remains available for review.
#' When an existing SVG matches, missing structural references are seeded in
#' interactive runs and in update mode (\code{options(jaspTools.plotStructure.update = TRUE)}
#' or \code{JASP_PLOT_STRUCTURE_UPDATE=true}).
#' Plot snapshot rendering triggers garbage collection by default to avoid
#' accumulated \pkg{ggplot2} build state slowing large generated test files; set
#' \code{options(jaspTools.plotSnapshot.gc = FALSE)} to disable this.
#'
#'
#' @param test The plot object or plot recipe you wish to test.
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

  originalTest <- test
  test <- materialize_plot_for_vdiffr(test)
  test <- decode_plot_for_snapshot(test)

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
      originalTest <- test
    }
    fallbackTest <- if (is_ggplot(test)) test else originalTest
    expect_plot_with_fallback(name, test, fallback_test = fallbackTest, tolerance = tolerance)
  }
}

materialize_plot_for_vdiffr <- function(test) {
  if (!("jaspPlotRecipe" %in% sub("^.*::", "", class(test))))
    return(test)

  if (!requireNamespace("jaspGraphs", quietly = TRUE))
    stop("Package 'jaspGraphs' is required to render a jaspPlotRecipe.", call. = FALSE)

  materialize <- getExportedValue("jaspGraphs", "materializeJaspPlotRecipe")
  materialize(test)
}

# The analysis only sees encoded column names, so the plots it stores in the state carry them too. JASP decodes a
# plot when it draws it (jaspBase's writeImage), so decode it the same way before comparing it with a snapshot.
# With ggplot2 >= 4 decodeplot() decodes the guide titles while drawing, which needs the bridge to still hold the
# dataset of the run, as it does right after runAnalysis(). Only these classes stay plottable: for the others
# decodeplot() returns a grob.
decode_plot_for_snapshot <- function(test) {
  if (!inherits(test, c("gg", "jaspGraphsPlot", "qgraph")) || !requireNamespace("jaspBase", quietly = TRUE))
    return(test)

  if (inherits(test, "jaspGraphsPlot"))
    test$subplots <- lapply(test$subplots, decode_ggplot_data)
  else
    test <- decode_ggplot_data(test)

  decodeplot <- utils::getFromNamespace("decodeplot", "jaspBase")
  decodeplot(test, returnGrob = FALSE)
}

# JASP decodes the drawn plot as a whole (decodeplot() with returnGrob = TRUE), so also column names that are data or
# scale labels, e.g. the variables on the x-axis of a paired samples plot. Drawing a grob would change how vdiffr
# renders every plot, so decode those values in the plot instead. Layers and scales are cloned, as they are mutable
# ggproto objects that the plot in the results shares.
decode_ggplot_data <- function(plot) {
  if (!is_ggplot(plot) || inherits(plot, "patchwork"))
    return(plot)

  decodeValues <- function(df) {
    if (!is.data.frame(df))
      return(df)
    for (nm in names(df))
      if (is.character(df[[nm]]) || is.factor(df[[nm]]))
        df[[nm]] <- jaspBase::decodeColNames(df[[nm]])
    df
  }

  plot$data <- decodeValues(plot$data)
  plot$layers <- lapply(plot$layers, function(layer) {
    if (!is.data.frame(layer$data))
      return(layer)
    clone <- ggplot2::ggproto(NULL, layer)
    clone$data <- decodeValues(layer$data)
    clone
  })

  scales <- plot$scales$clone()
  for (i in seq_along(scales$scales)) {
    scale <- scales$scales[[i]]$clone()
    # limits can also be a factor, e.g. scale_x_discrete(limits = <the factor on the x-axis>): left encoded,
    # none of the decoded data values would fall within them
    for (field in c("breaks", "labels", "limits"))
      if (is.character(scale[[field]]) || is.factor(scale[[field]]))
        scale[[field]] <- jaspBase::decodeColNames(scale[[field]])
    scales$scales[[i]] <- scale
  }
  plot$scales <- scales

  plot
}

# decodeplot() leaves plot$labels encoded with ggplot2 >= 4, so the structural snapshot decodes them itself
decode_plot_labels <- function(labels) {
  if (!is.list(labels) || !requireNamespace("jaspBase", quietly = TRUE))
    return(labels)

  rapply(labels, jaspBase::decodeColNames, classes = "character", how = "replace")
}

expect_plot_with_fallback <- function(name, test, fallback_test = test, tolerance = NULL) {
  testthat::local_edition(3)
  structureName <- ggplot_structure_snapshot_name(name)
  # A non-ggplot can replace an accepted ggplot. Keep the old structure until review.
  testthat::announce_snapshot_file(name = structureName)
  structurePath <- snapshot_relative_path(structureName)
  structureCandidate <- snapshotCandidatePath(structurePath)
  result <- capture_vdiffr_expectation(name, test)

  if (!result$new_svg_current && !is.null(result$exception))
    return(testthat::fail(conditionMessage(result$exception)))
  if (!result$new_svg_current && !result$compared)
    return(invisible(TRUE))

  # Once vdiffr replaces the SVG, its old structural candidate must not be reviewed with it.
  pairReady <- !result$new_svg_current
  on.exit({
    if (!pairReady)
      unlink(c(result$new_svg_path, structureCandidate), expand = FALSE)
  }, add = TRUE)
  if (file.exists(structureCandidate) && !file.remove(structureCandidate))
    stop("Could not clear the previous structural candidate at ", structureCandidate, call. = FALSE)

  if (!result$new_svg_current) {
    if (is_ggplot(test) && (should_update_ggplot_structure_snapshots() ||
        (!file.exists(structurePath) && is_interactive_plot_snapshot_mode()))) {
      # A matching SVG already satisfies the expectation independently of optional seeding.
      tryCatch(write_ggplot_structure_snapshot(extract_ggplot_structure(test), structurePath),
               error = function(e) NULL)
    }
    return(invisible(TRUE))
  }

  fallback <- expect_doppelganger_fallback(fallback_test, name, snapshot_path = structurePath, tolerance = tolerance)
  pairReady <- TRUE
  if (!result$reference_existed) {
    testthat::fail(paste0("vdiffr created a new visual snapshot for '", name,
                          "'. Review and accept it with jaspTools::reviewSnapshots(modulePath)."))
  } else if (isTRUE(fallback$passed)) {
    warning("vdiffr mismatch for '", name, "' accepted by structural fallback. Review the visual change at ",
            result$new_svg_path, " with jaspTools::reviewSnapshots(modulePath).", call. = FALSE)
    testthat::succeed()
  } else {
    testthat::fail(paste0("vdiffr mismatch for '", name, "'.\n", conditionMessage(result$exception),
                          "\nFallback failure: ", fallback$message))
  }
}

capture_vdiffr_expectation <- function(name, test) {
  maybe_collect_plot_snapshot_garbage()

  slug <- str_standardise_snapshot_name(name)
  svgPath <- snapshot_relative_path(paste0(slug, ".svg"))
  newSvgPath <- snapshotCandidatePath(svgPath)
  referenceExisted <- file.exists(svgPath)
  renderedPath <- NULL
  writer <- function(plot, file, title) {
    vdiffr::write_svg(plot, file, title)
    renderedPath <<- file
  }

  # Outside a test file, vdiffr prints the rendered path for interactive inspection.
  on.exit(if (!is.null(renderedPath) && !is.null(get_snapshotter())) unlink(renderedPath, expand = FALSE))
  compared <- FALSE
  skippedOnCran <- FALSE
  exception <- tryCatch({
    withCallingHandlers(
      suppressWarnings(vdiffr::expect_doppelganger(name, test, writer = writer)),
      expectation_success = function(cnd) compared <<- TRUE,
      snapshot_on_cran = function(cnd) skippedOnCran <<- TRUE
    )
    NULL
  }, expectation_failure = identity, expectation_warning = identity)

  newSvgCurrent <- FALSE
  if (!referenceExisted && file.exists(svgPath)) {
    # testthat initially writes a reference in both local and CI runs.
    if (is.null(renderedPath) || !testthat::compare_file_binary(renderedPath, svgPath))
      stop("A different visual reference appeared during the test at ", svgPath, call. = FALSE)
    if (!suppressWarnings(file.rename(svgPath, newSvgPath))) {
      # A failed staging must not leave testthat's unreviewed SVG as an accepted reference.
      if (!file.remove(svgPath))
        stop("Could not clear the unreviewed visual snapshot at ", svgPath, call. = FALSE)
      stop("Could not stage the new visual snapshot at ", newSvgPath, call. = FALSE)
    }
    newSvgCurrent <- TRUE
  } else if (!is.null(renderedPath)) {
    newSvgCurrent <- !is.null(exception)
    if (!newSvgCurrent && !skippedOnCran && file.exists(newSvgPath) && !is.null(get_snapshotter())) {
      # A reporter can leave .new.svg without signalling an expectation.
      compared <- TRUE
      newSvgCurrent <- !file.exists(svgPath) || !testthat::compare_file_text(svgPath, renderedPath)
      if (!newSvgCurrent && !file.remove(newSvgPath))
        stop("Could not clear the previous visual candidate at ", newSvgPath, call. = FALSE)
    }
    if (newSvgCurrent) {
      # Use the exact rendering that vdiffr compared, including its title and theme.
      dir.create(dirname(newSvgPath), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(renderedPath, newSvgPath, overwrite = TRUE))
        stop("Could not save the visual snapshot at ", newSvgPath, call. = FALSE)
    }
  }

  if (newSvgCurrent && is.null(exception))
    exception <- simpleError(paste0("Visual snapshot for '", name, "' requires review."))

  list(exception = exception, compared = compared,
       new_svg_path = newSvgPath, reference_existed = referenceExisted,
       new_svg_current = newSvgCurrent)
}

maybe_collect_plot_snapshot_garbage <- function() {
  if (isFALSE(getOption("jaspTools.plotSnapshot.gc", TRUE)))
    return(invisible(FALSE))

  invisible(gc(FALSE))
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
expect_doppelganger_fallback.ggplot <- function(test, name, snapshot_path, ..., tolerance = NULL) {
  current <- extract_ggplot_structure(test)
  write_ggplot_structure_snapshot(current, snapshotCandidatePath(snapshot_path))
  if (!file.exists(snapshot_path))
    return(list(passed = FALSE, message = paste0("No accepted ggplot structural snapshot exists for '", name, "'.")))

  previous <- tryCatch(readRDS(snapshot_path), error = identity)
  if (inherits(previous, "error"))
    return(list(passed = FALSE, message = conditionMessage(previous)))

  if (is.null(tolerance))
    tolerance <- getOption("jaspTools.plotStructure.tolerance", 1e-6)
  differences <- all.equal(normalize_snapshot_structure(previous), normalize_snapshot_structure(current),
                           tolerance = tolerance, check.attributes = FALSE)
  if (isTRUE(differences))
    return(list(passed = TRUE))

  list(passed = FALSE, message = paste0(
    "Structural fallback mismatch (tolerance = ", format(tolerance, scientific = TRUE), "):\n  ",
    paste(utils::head(differences, 10L), collapse = "\n  "),
    if (length(differences) > 10L) paste0("\n  ... and ", length(differences) - 10L, " more differences")
  ))
}

is_interactive_plot_snapshot_mode <- function() {
  interactive() && !identical(tolower(Sys.getenv("CI", "false")), "true")
}

write_ggplot_structure_snapshot <- function(structure, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".plot-structure-", tmpdir = dirname(path))
  on.exit(unlink(tmp, expand = FALSE))
  saveRDS(structure, tmp)
  if (!file.rename(tmp, path))
    stop("Could not write the structural snapshot at ", path, call. = FALSE)
}

should_update_ggplot_structure_snapshots <- function() {
  isTRUE(getOption("jaspTools.plotStructure.update", FALSE)) ||
    identical(tolower(Sys.getenv("JASP_PLOT_STRUCTURE_UPDATE", "false")), "true")
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

extract_ggplot_structure <- function(plot) {
  if (!is_ggplot(plot))
    stop("`plot` must inherit from 'ggplot'.")

  built <- ggplot2::ggplot_build(plot)

  list(
    labels = normalize_named_list(decode_plot_labels(plot$labels)),
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
