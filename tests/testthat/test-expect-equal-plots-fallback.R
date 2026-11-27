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
      testthat::succeed()
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

test_that("plot candidates follow the same review lifecycle locally and on CI", {
  skip_if_not_installed("ggplot2")
  withr::local_envvar(c(NOT_CRAN = "true"))
  withr::local_options(jaspTools.plotSnapshot.gc = FALSE)

  testDir <- withr::local_tempdir()
  writeLines(c(
    'testthat::test_that("plot snapshot", {',
    '  jaspTools:::expect_plot_with_fallback("Demo Plot", getOption("jaspTools.testPlot"))',
    '})'
  ), file.path(testDir, "test-figures.R"))
  runPlotTest <- function(plot, ci) {
    withr::local_options(jaspTools.testPlot = plot)
    withr::local_envvar(c(CI = ci))
    as.data.frame(testthat::test_dir(testDir, reporter = "silent", stop_on_failure = FALSE))
  }

  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  dir <- file.path(testDir, "_snaps", "figures")
  svg <- file.path(dir, "demo-plot.svg")
  newSvg <- file.path(dir, "demo-plot.new.svg")
  structure <- file.path(dir, "reference_plotobject", "demo-plot.rds")
  newStructure <- file.path(dir, "reference_plotobject", "demo-plot.new.rds")
  expectedSvg <- tempfile(fileext = ".svg")
  vdiffr::write_svg(plot, expectedSvg, "Demo Plot")

  for (ci in c("false", "true")) {
    # testthat writes the first snapshot to the canonical path in both modes.
    result <- runPlotTest(plot, ci)
    expect_equal(result$failed, 1L)
    expect_false(any(file.exists(c(svg, structure))))
    expect_true(all(file.exists(c(newSvg, newStructure))))
    expect_true(testthat::compare_file_binary(expectedSvg, newSvg))
    expect_identical(readRDS(newStructure), jaspTools:::extract_ggplot_structure(plot))
  }

  # Accept both files, then verify a full passing run preserves the references.
  expect_true(file.rename(newSvg, svg))
  expect_true(file.rename(newStructure, structure))
  result <- runPlotTest(plot, "false")
  expect_equal(result$failed, 0L)
  expect_false(result$error)
  expect_true(all(file.exists(c(svg, structure))))
  expect_false(any(file.exists(c(newSvg, newStructure))))

  changed <- plot + ggplot2::geom_line()
  result <- runPlotTest(changed, "true")
  expect_equal(result$failed, 1L)
  expect_identical(readRDS(structure), jaspTools:::extract_ggplot_structure(plot))
  expect_identical(readRDS(newStructure), jaspTools:::extract_ggplot_structure(changed))
  vdiffr::write_svg(changed, expectedSvg, "Demo Plot")
  expect_true(testthat::compare_file_binary(expectedSvg, newSvg))

  # A theme change keeps the structural fallback valid and the SVG reviewable.
  themeChange <- plot + ggplot2::theme_bw()
  result <- runPlotTest(themeChange, "false")
  expect_equal(result$failed, 0L)
  expect_false(result$error)
  expect_equal(result$warning, 1L)
  expect_true(all(file.exists(c(newSvg, newStructure))))
  expect_identical(readRDS(newStructure), readRDS(structure))

  withr::with_envvar(c(NOT_CRAN = "false"), runPlotTest(plot, "true"))
  expect_true(all(file.exists(c(newSvg, newStructure))))

  result <- runPlotTest(plot, "false")
  expect_equal(result$failed, 0L)
  expect_equal(result$warning, 0L)
  expect_false(result$error)
  expect_false(any(file.exists(c(newSvg, newStructure))))
  expect_true(all(file.exists(c(svg, structure))))
})

test_that("a reporter returning silently still stages the vdiffr rendering", {
  root <- localPlotSnapshotRoot()
  svg <- file.path(root, "silent.svg")
  candidate <- file.path(root, "silent.new.svg")
  writeLines("<svg>old</svg>", svg)
  plot <- function() graphics::plot(1:3)
  testthat::local_mocked_bindings(
    expect_doppelganger = function(title, fig, writer) {
      current <- tempfile(fileext = ".svg")
      writer(fig, current, title)
      file.copy(current, candidate, overwrite = TRUE)
      invisible(NULL)
    },
    .package = "vdiffr"
  )
  result <- jaspTools:::capture_vdiffr_expectation("silent", plot)
  expect_false(result$passed)
  expect_true(result$new_svg_current)
  expectedSvg <- tempfile(fileext = ".svg")
  vdiffr::write_svg(plot, expectedSvg, "silent")
  expect_true(testthat::compare_file_binary(candidate, expectedSvg))
})

test_that("a failed ggplot structure build clears an incomplete review pair", {
  root <- localPlotSnapshotRoot()
  svg <- file.path(root, "incomplete.new.svg")
  structure <- file.path(root, "reference_plotobject", "incomplete.new.rds")
  dir.create(dirname(structure))
  writeLines("<svg>current</svg>", svg)
  saveRDS("stale structure", structure)
  result <- list(passed = FALSE, new_svg_current = TRUE, new_svg_path = svg)
  testthat::local_mocked_bindings(
    build_ggplot_structure_snapshot = function(plot) stop("build failed"),
    .package = "jaspTools"
  )
  expect_error(jaspTools:::expect_equal_ggplot_structure(NULL, "incomplete", result), "build failed")
  expect_false(any(file.exists(c(svg, structure))))
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
  expect_match(fallbackResult$message, "No accepted ggplot structural snapshot", fixed = TRUE)
})

test_that("stale new svg is overwritten after visual failure", {
  skip_if_not_installed("ggplot2")
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
    expect_doppelganger = function(title, fig, writer) {
      writer(fig, tempfile(fileext = ".svg"), title)
      stop(makeExpectationFailure(), call. = FALSE)
    },
    .package = "vdiffr"
  )

  expect_warning(
    jaspTools:::expect_plot_with_fallback("stale-failure-plot", p),
    "Review the visual change"
  )
  expectedSvg <- tempfile(fileext = ".svg")
  vdiffr::write_svg(p, expectedSvg, "stale-failure-plot")
  expect_true(testthat::compare_file_binary(newSvgPath, expectedSvg))
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
      # a mismatch against an existing reference, so not a fresh snapshot
      list(passed = FALSE, exception = simpleError("forced mismatch"),
           svg_path = tempfile(fileext = ".svg"), reference_existed = TRUE, new_svg_current = FALSE)
    },
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
    decode_plot_for_snapshot = function(test) test,
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

test_that("plots are decoded like JASP does before they are compared with a snapshot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("jaspBase")

  # the analysis only sees encoded column names; jaspBase finds the engine's decoder in the global environment
  decoded <- c(JASPColumn_D1_F1_1_Encoded = "contBinom", JASPColumn_D1_F1_2_Encoded = "contNormal")
  decoder <- function(x) { hit <- x %in% names(decoded); x[hit] <- decoded[x[hit]]; x }
  decoderNames <- c(".decodeColNamesLax", ".decodeColNamesStrict")
  oldDecoders <- mget(decoderNames, envir = .GlobalEnv, ifnotfound = list(NULL))
  for (nm in decoderNames)
    assign(nm, decoder, envir = .GlobalEnv)
  on.exit({
    for (nm in decoderNames)
      if (is.null(oldDecoders[[nm]])) rm(list = nm, envir = .GlobalEnv) else assign(nm, oldDecoders[[nm]], envir = .GlobalEnv)
  }, add = TRUE)

  # jaspGraphs plots set their axis titles explicitly, which is what plot$labels holds with ggplot2 >= 4
  df <- data.frame(JASPColumn_D1_F1_1_Encoded = factor(c("a", "b")), JASPColumn_D1_F1_2_Encoded = c(1, 2))
  p <- ggplot2::ggplot(df, ggplot2::aes(JASPColumn_D1_F1_1_Encoded, JASPColumn_D1_F1_2_Encoded)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "JASPColumn_D1_F1_1_Encoded", y = "JASPColumn_D1_F1_2_Encoded")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  axisTitles <- function(plot) {
    grob <- ggplot2::ggplotGrob(plot)
    unlist(lapply(grob$grobs[grepl("^(xlab|ylab)", grob$layout$name)], function(g) rapply(g, identity, classes = "character", how = "unlist")))
  }

  expect_true(all(c("contBinom", "contNormal") %in% axisTitles(jaspTools:::decode_plot_for_snapshot(p))))
  expect_false(any(grepl("JASPColumn_", axisTitles(jaspTools:::decode_plot_for_snapshot(p)))))

  structureLabels <- unlist(jaspTools:::extract_ggplot_structure(p)$labels)
  expect_true(all(c("contBinom", "contNormal") %in% structureLabels))
  expect_false(any(grepl("JASPColumn_", structureLabels)))

  # column names can also be data, e.g. the variables on the x-axis of a paired samples plot, in the plot or a layer
  variables <- factor(c("JASPColumn_D1_F1_1_Encoded", "JASPColumn_D1_F1_2_Encoded"))
  q <- ggplot2::ggplot(data.frame(variable = variables, value = 1:2), ggplot2::aes(variable, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(data = data.frame(variable = variables, value = 1:2, label = as.character(variables)), ggplot2::aes(label = label))

  tickLabels <- function(plot) {
    grob <- ggplot2::ggplotGrob(plot)
    unlist(lapply(grob$grobs[grepl("^axis-b", grob$layout$name)], function(g) rapply(g, identity, classes = "character", how = "unlist")))
  }
  decodedQ <- jaspTools:::decode_plot_for_snapshot(q)
  expect_true(all(c("contBinom", "contNormal") %in% tickLabels(decodedQ)))
  expect_identical(levels(decodedQ$layers[[2]]$data$variable), c("contBinom", "contNormal"))
  expect_identical(decodedQ$layers[[2]]$data$label, c("contBinom", "contNormal"))
  # the plot in the results is left as it was
  expect_identical(levels(q$data$variable), levels(variables))
  expect_identical(q$layers[[2]]$data$label, as.character(variables))

  # or explicit scale labels, as in the paired samples plots
  s <- ggplot2::ggplot(data.frame(variable = factor(c("a", "b")), value = 1:2), ggplot2::aes(variable, value)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_discrete(labels = levels(variables))
  expect_true(all(c("contBinom", "contNormal") %in% tickLabels(jaspTools:::decode_plot_for_snapshot(s))))
  expect_identical(s$scales$scales[[1]]$labels, levels(variables))

  # or scale limits given as the factor on the x-axis, as in the bar plots: these must be decoded along with the
  # data, or no bar falls within them and positioning the bars fails
  b <- ggplot2::ggplot(data.frame(variable = variables, value = 1:2), ggplot2::aes(variable, value)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge2(preserve = "single")) +
    ggplot2::scale_x_discrete(limits = variables)
  decodedB <- jaspTools:::decode_plot_for_snapshot(b)
  expect_no_error(ggplot2::ggplot_build(decodedB))
  expect_true(all(c("contBinom", "contNormal") %in% tickLabels(decodedB)))
  expect_identical(b$scales$scales[[1]]$limits, variables)
})
