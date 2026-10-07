context("expect_equal_plots fallback")

makeExpectationFailure <- function(msg = "vdiffr mismatch") {
  structure(
    list(message = msg),
    class = c("expectation_failure", "expectation", "condition")
  )
}

localPlotSnapshotRoot <- function(root = tempfile("plot-snapshot-root-")) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  env <- parent.frame()
  testthat::local_mocked_bindings(
    snapshot_relative_path = function(name) {
      file.path(root, name)
    },
    .package = "jaspTools",
    .env = env
  )

  invisible(root)
}

test_that("vdiffr mismatch falls back to ggplot structural snapshot", {
  skip_if_not_installed("ggplot2")

  tmp <- tempfile("plot-fallback-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  oldWd <- setwd(tmp)
  on.exit(setwd(oldWd), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  snapshotRoot <- tempfile("plot-snapshot-root-")
  snapshotName <- jaspTools:::ggplot_structure_snapshot_name("demo-plot")
  snapshotPath <- file.path(snapshotRoot, snapshotName)
  dir.create(dirname(snapshotPath), recursive = TRUE, showWarnings = FALSE)
  saveRDS(jaspTools:::extract_ggplot_structure(p), snapshotPath)
  localPlotSnapshotRoot(snapshotRoot)

  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) {
      stop(makeExpectationFailure(), call. = FALSE)
    },
    .package = "vdiffr"
  )

  vdiffrResult <- jaspTools:::capture_vdiffr_expectation("demo-plot", p)
  expect_false(vdiffrResult$passed)
  expect_s3_class(vdiffrResult$exception, "expectation_failure")

  fallbackResult <- jaspTools:::expect_doppelganger_fallback(p, "demo-plot", vdiffr_result = vdiffrResult)
  expect_true(fallbackResult$has_fallback)
  expect_true(fallbackResult$passed)
})

test_that("default fallback reports no method for non-ggplot", {
  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) {
      stop(makeExpectationFailure("forced mismatch"), call. = FALSE)
    },
    .package = "vdiffr"
  )

  vdiffrResult <- jaspTools:::capture_vdiffr_expectation("non-ggplot", function() plot(1:3))
  expect_false(vdiffrResult$passed)
  expect_s3_class(vdiffrResult$exception, "expectation_failure")

  fallbackResult <- jaspTools:::expect_doppelganger_fallback(function() plot(1:3), "non-ggplot", vdiffr_result = vdiffrResult)
  expect_false(fallbackResult$passed)
  expect_false(fallbackResult$has_fallback)
  expect_match(fallbackResult$message, "No fallback expectation is implemented")
})

test_that("update mode writes ggplot structural snapshot when missing", {
  skip_if_not_installed("ggplot2")
  oldOpts <- options(jaspTools.plotStructure.update = TRUE)
  on.exit(options(oldOpts), add = TRUE)

  tmp <- tempfile("plot-snapshot-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  oldWd <- setwd(tmp)
  on.exit(setwd(oldWd), add = TRUE)
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, hp)) + ggplot2::geom_point()

  fallbackResult <- jaspTools:::expect_equal_ggplot_structure(p, "new-plot")
  expect_true(fallbackResult$passed)
  expect_true(fallbackResult$has_fallback)
})

test_that("interactive run seeds missing ggplot structural snapshot", {
  skip_if_not_installed("ggplot2")

  tmp <- tempfile("plot-seed-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  oldWd <- setwd(tmp)
  on.exit(setwd(oldWd), add = TRUE)
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  snapshotPath <- jaspTools:::ggplot_structure_snapshot_path("seed-plot")
  if (file.exists(snapshotPath))
    file.remove(snapshotPath)

  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) {
      invisible(NULL)
    },
    .package = "vdiffr"
  )

  testthat::local_mocked_bindings(
    is_interactive_plot_snapshot_mode = function() TRUE,
    .package = "jaspTools"
  )

  jaspTools:::expect_plot_with_fallback("seed-plot", p)
  expect_true(file.exists(snapshotPath))
})

test_that("fresh visual snapshot seeds structure but still requires review", {
  skip_if_not_installed("ggplot2")
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  newSvgPath <- jaspTools:::snapshot_relative_path("fresh-plot.new.svg")
  snapshotPath <- jaspTools:::ggplot_structure_snapshot_path("fresh-plot")

  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) {
      dir.create(dirname(newSvgPath), recursive = TRUE, showWarnings = FALSE)
      writeLines("<svg>fresh</svg>", newSvgPath)
      invisible(NULL)
    },
    .package = "vdiffr"
  )

  failure <- tryCatch(
    {
      jaspTools:::expect_plot_with_fallback("fresh-plot", p)
      NULL
    },
    expectation_failure = function(cnd) cnd
  )

  expect_s3_class(failure, "expectation_failure")
  expect_match(conditionMessage(failure), "created a new visual snapshot", fixed = TRUE)
  expect_true(file.exists(snapshotPath))
})

test_that("visual mismatch with structural pass reports current new svg", {
  skip_if_not_installed("ggplot2")
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  svgPath <- jaspTools:::snapshot_relative_path("review-plot.svg")
  newSvgPath <- jaspTools:::snapshot_relative_path("review-plot.new.svg")
  snapshotPath <- jaspTools:::ggplot_structure_snapshot_path("review-plot")
  dir.create(dirname(svgPath), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(snapshotPath), recursive = TRUE, showWarnings = FALSE)
  writeLines("<svg>old</svg>", svgPath)
  saveRDS(jaspTools:::extract_ggplot_structure(p), snapshotPath)

  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) {
      writeLines("<svg>current</svg>", newSvgPath)
      invisible(NULL)
    },
    .package = "vdiffr"
  )

  expect_warning(
    jaspTools:::expect_plot_with_fallback("review-plot", p),
    "Review the visual change"
  )
  expect_true(file.exists(newSvgPath))
})

test_that("stale new svg is ignored when vdiffr passes", {
  skip_if_not_installed("ggplot2")
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  newSvgPath <- jaspTools:::snapshot_relative_path("stale-plot.new.svg")
  dir.create(dirname(newSvgPath), recursive = TRUE, showWarnings = FALSE)
  writeLines("<svg></svg>", newSvgPath)

  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) {
      invisible(NULL)
    },
    .package = "vdiffr"
  )

  result <- jaspTools:::capture_vdiffr_expectation("stale-plot", p)
  expect_true(result$passed)
  expect_false(result$new_svg_current)
  expect_true(file.exists(newSvgPath))
})

test_that("visual failure with missing structural snapshot does not create fallback", {
  skip_if_not_installed("ggplot2")
  oldOpts <- options(jaspTools.plotStructure.update = TRUE)
  on.exit(options(oldOpts), add = TRUE)
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  snapshotPath <- jaspTools:::ggplot_structure_snapshot_path("missing-plot")
  vdiffrResult <- list(passed = FALSE, exception = makeExpectationFailure())

  fallbackResult <- jaspTools:::expect_equal_ggplot_structure(p, "missing-plot", vdiffr_result = vdiffrResult)

  expect_false(fallbackResult$passed)
  expect_true(fallbackResult$has_fallback)
  expect_false(file.exists(snapshotPath))
  expect_match(fallbackResult$message, "Not creating one because the visual comparison failed", fixed = TRUE)
})

test_that("stale new svg is overwritten after visual failure", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("svglite")
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  svgPath <- jaspTools:::snapshot_relative_path("stale-failure-plot.svg")
  newSvgPath <- jaspTools:::snapshot_relative_path("stale-failure-plot.new.svg")
  snapshotPath <- jaspTools:::ggplot_structure_snapshot_path("stale-failure-plot")
  dir.create(dirname(svgPath), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(snapshotPath), recursive = TRUE, showWarnings = FALSE)
  writeLines("<svg>old</svg>", svgPath)
  writeLines("<svg>stale</svg>", newSvgPath)
  saveRDS(jaspTools:::extract_ggplot_structure(p), snapshotPath)

  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) {
      stop(makeExpectationFailure(), call. = FALSE)
    },
    .package = "vdiffr"
  )

  expect_warning(
    jaspTools:::expect_plot_with_fallback("stale-failure-plot", p),
    "Review the visual change"
  )
  expect_false(any(grepl("stale", readLines(newSvgPath, warn = FALSE), fixed = TRUE)))
})

test_that("update mode does not overwrite structural snapshot after visual failure", {
  skip_if_not_installed("ggplot2")
  oldOpts <- options(jaspTools.plotStructure.update = TRUE)
  on.exit(options(oldOpts), add = TRUE)
  localPlotSnapshotRoot()

  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(colour = "red")
  snapshotName <- jaspTools:::ggplot_structure_snapshot_name("locked-plot")
  snapshotPath <- jaspTools:::snapshot_relative_path(snapshotName)
  dir.create(dirname(snapshotPath), recursive = TRUE, showWarnings = FALSE)
  saveRDS(jaspTools:::extract_ggplot_structure(p1), snapshotPath)
  original <- readBin(snapshotPath, what = "raw", n = file.info(snapshotPath)$size)
  vdiffrResult <- list(passed = FALSE, exception = makeExpectationFailure())

  fallbackResult <- jaspTools:::expect_equal_ggplot_structure(p2, "locked-plot", vdiffr_result = vdiffrResult)
  current <- readBin(snapshotPath, what = "raw", n = file.info(snapshotPath)$size)

  expect_false(fallbackResult$passed)
  expect_identical(current, original)
})

test_that("compare_ggplot_structure_snapshot returns TRUE only for equal structures", {
  skip_if_not_installed("ggplot2")

  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, hp)) + ggplot2::geom_point()
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, hp)) + ggplot2::geom_point(colour = "red")

  s1 <- jaspTools:::extract_ggplot_structure(p1)
  s2 <- jaspTools:::extract_ggplot_structure(p2)

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")
  f3 <- tempfile(fileext = ".rds")

  saveRDS(s1, f1)
  saveRDS(s1, f2)
  saveRDS(s2, f3)

  expect_true(jaspTools:::compare_ggplot_structure_snapshot(f1, f2))
  expect_false(jaspTools:::compare_ggplot_structure_snapshot(f1, f3))
})

test_that("vdiffr and fallback can use different plot objects", {
  rendered <- structure(list(), class = "rendered-plot")
  original <- structure(list(), class = "original-plot")
  seen <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    capture_vdiffr_expectation = function(name, test) {
      seen$vdiffr <- test
      list(passed = FALSE, exception = simpleError("forced mismatch"))
    },
    save_failed_plot_svg = function(...) invisible(NULL),
    expect_doppelganger_fallback = function(test, ...) {
      seen$fallback <- test
      list(passed = TRUE, has_fallback = TRUE, exception = NULL, message = NULL)
    },
    .package = "jaspTools"
  )

  expect_warning(
    jaspTools:::expect_plot_with_fallback("separate-targets", rendered, fallback_test = original),
    "accepted by structural fallback"
  )
  expect_identical(seen$vdiffr, rendered)
  expect_identical(seen$fallback, original)
})

test_that("materialized ggplot is used for vdiffr and structural fallback", {
  skip_if_not_installed("ggplot2")

  recipe <- structure(list(fun = "example"), class = "jaspPlotRecipe")
  rendered <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  seen <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    materialize_plot_for_vdiffr = function(test) rendered,
    expect_plot_with_fallback = function(name, test, fallback_test, ...) {
      seen$vdiffr <- test
      seen$fallback <- fallback_test
      invisible(TRUE)
    },
    .package = "jaspTools"
  )

  jaspTools::expect_equal_plots(recipe, "recipe-plot")
  expect_identical(seen$vdiffr, rendered)
  expect_identical(seen$fallback, rendered)
})

test_that("non-ggplot recipe rendering keeps the recipe as fallback", {
  recipe <- structure(list(fun = "example"), class = "jaspPlotRecipe")
  rendered <- function() graphics::plot(1:3)
  seen <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    materialize_plot_for_vdiffr = function(test) rendered,
    expect_plot_with_fallback = function(name, test, fallback_test, ...) {
      seen$vdiffr <- test
      seen$fallback <- fallback_test
      invisible(TRUE)
    },
    .package = "jaspTools"
  )

  jaspTools::expect_equal_plots(recipe, "recipe-function-plot")
  expect_identical(seen$vdiffr, rendered)
  expect_identical(seen$fallback, recipe)
})
