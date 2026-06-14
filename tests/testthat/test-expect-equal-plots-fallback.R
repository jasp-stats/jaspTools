context("expect_equal_plots fallback")

makeExpectationFailure <- function(msg = "vdiffr mismatch") {
  structure(
    list(message = msg),
    class = c("expectation_failure", "expectation", "condition")
  )
}

test_that("vdiffr mismatch falls back to ggplot structural snapshot", {
  skip_if_not_installed("ggplot2")

  tmp <- tempfile("plot-fallback-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  oldWd <- setwd(tmp)
  on.exit(setwd(oldWd), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  snapshotPath <- jaspTools:::ggplot_structure_snapshot_path("demo-plot")
  dir.create(dirname(snapshotPath), recursive = TRUE, showWarnings = FALSE)
  saveRDS(jaspTools:::extract_ggplot_structure(p), snapshotPath)

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
