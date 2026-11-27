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

test_that("a figure comparison restores its caller's testthat edition", {
  testthat::local_edition(2)
  localPlotSnapshotRoot()
  testthat::local_mocked_bindings(
    capture_vdiffr_expectation = function(...) list(new_svg_current = FALSE, compared = TRUE),
    .package = "jaspTools"
  )
  jaspTools:::expect_plot_with_fallback("matches", function() NULL)
  expect_equal(testthat::edition_get(), 2L)
})

test_that("a drawing failure cannot pass via the structural fallback", {
  skip_if_not_installed("ggplot2")
  root <- localPlotSnapshotRoot()
  name <- "failed-draw"
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  structure <- file.path(root, "reference_plotobject", "failed-draw.rds")
  dir.create(dirname(structure))
  saveRDS(jaspTools:::extract_ggplot_structure(plot), structure)
  pending <- file.path(root, "failed-draw.new.svg")
  writeLines("previous visual candidate", pending)
  pendingStructure <- file.path(dirname(structure), "failed-draw.new.rds")
  saveRDS("previous structure", pendingStructure)

  testthat::local_mocked_bindings(
    expect_doppelganger = function(...) stop(makeExpectationFailure("drawing failed")),
    .package = "vdiffr"
  )
  expect_failure(jaspTools:::expect_plot_with_fallback(name, plot), "drawing failed")
  expect_identical(readLines(pending), "previous visual candidate")
  expect_identical(readRDS(pendingStructure), "previous structure")
})

test_that("plot candidates follow the same review lifecycle locally and on CI", {
  skip_if_not_installed("ggplot2")
  withr::local_envvar(c(NOT_CRAN = "true"))
  withr::local_options(jaspTools.plotSnapshot.gc = FALSE)

  testDir <- withr::local_tempdir()
  writeLines(c(
    'testthat::test_that("plot snapshot", {',
    '  jaspTools:::expect_plot_with_fallback("Demo Plot", getOption("jaspTools.testPlot"),',
    '                                        tolerance = getOption("jaspTools.testTolerance"))',
    '})'
  ), file.path(testDir, "test-figures.R"))
  runPlotTest <- function(plot, ci, tolerance = NULL) {
    withr::local_options(jaspTools.testPlot = plot, jaspTools.testTolerance = tolerance)
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

  # Update mode cannot overwrite an accepted structure after a visual mismatch.
  withr::with_options(list(jaspTools.plotStructure.update = TRUE), runPlotTest(changed, "true"))
  expect_identical(readRDS(structure), jaspTools:::extract_ggplot_structure(plot))

  shifted <- plot
  shifted$data$mpg[1L] <- shifted$data$mpg[1L] + 0.5
  expect_equal(runPlotTest(shifted, "true", tolerance = 0)$failed, 1L)
  result <- runPlotTest(shifted, "true", tolerance = 0.1)
  expect_equal(result$failed, 0L)
  expect_equal(result$warning, 1L)

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

  # An SVG-only candidate supersedes the pending ggplot pair. Preserve both accepted references.
  runPlotTest(changed, "false")
  result <- runPlotTest(function() graphics::plot(1:3), "false")
  expect_equal(result$failed, 1L)
  expect_false(result$error)
  expect_true(all(file.exists(c(svg, structure, newSvg))))
  expect_false(file.exists(newStructure))

  # Matching an accepted SVG can restore its legacy missing companion in update or interactive mode.
  unlink(structure)
  result <- runPlotTest(themeChange, "false")
  expect_equal(result$failed, 1L)
  expect_false(file.exists(structure))
  result <- withr::with_options(list(jaspTools.plotStructure.update = TRUE), runPlotTest(plot, "false"))
  expect_false(result$error)
  expect_identical(readRDS(structure), jaspTools:::extract_ggplot_structure(plot))
  unlink(structure)
  testthat::local_mocked_bindings(is_interactive_plot_snapshot_mode = function() TRUE, .package = "jaspTools")
  result <- runPlotTest(plot, "false")
  expect_false(result$error)
  expect_identical(readRDS(structure), jaspTools:::extract_ggplot_structure(plot))
  unlink(structure)
  testthat::local_mocked_bindings(extract_ggplot_structure = function(...) stop("build failed"), .package = "jaspTools")
  result <- runPlotTest(plot, "false")
  expect_false(result$error)
  expect_equal(result$failed, 0L)
  expect_false(file.exists(structure))
})

test_that("a staging failure cannot leave a new SVG accepted", {
  root <- localPlotSnapshotRoot()
  svg <- file.path(root, "blocked.svg")
  dir.create(file.path(root, "blocked.new.svg"))
  testthat::local_mocked_bindings(
    expect_doppelganger = function(title, fig, writer) {
      current <- tempfile(fileext = ".svg")
      writer(fig, current, title)
      file.copy(current, svg)
    },
    .package = "vdiffr"
  )
  expect_error(jaspTools:::capture_vdiffr_expectation("blocked", function() graphics::plot(1:3)),
               "Could not stage the new visual snapshot")
  expect_false(file.exists(svg))
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
  expect_true(result$new_svg_current)
  expectedSvg <- tempfile(fileext = ".svg")
  vdiffr::write_svg(plot, expectedSvg, "silent")
  expect_true(testthat::compare_file_binary(candidate, expectedSvg))
})

test_that("a failed ggplot structure build clears an incomplete review pair", {
  skip_if_not_installed("ggplot2")
  root <- localPlotSnapshotRoot()
  svg <- file.path(root, "incomplete.new.svg")
  structure <- file.path(root, "reference_plotobject", "incomplete.new.rds")
  dir.create(dirname(structure))
  writeLines("<svg>current</svg>", svg)
  saveRDS("stale structure", structure)
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  testthat::local_mocked_bindings(
    capture_vdiffr_expectation = function(...) list(compared = TRUE, new_svg_current = TRUE, new_svg_path = svg),
    extract_ggplot_structure = function(plot) stop("build failed"),
    .package = "jaspTools"
  )
  expect_error(jaspTools:::expect_plot_with_fallback("incomplete", plot), "build failed")
  expect_false(any(file.exists(c(svg, structure))))
})

test_that("stale new svg is overwritten after visual failure", {
  skip_if_not_installed("ggplot2")
  localPlotSnapshotRoot()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  svgPath <- jaspTools:::snapshot_relative_path("stale-failure-plot.svg")
  newSvgPath <- jaspTools:::snapshot_relative_path("stale-failure-plot.new.svg")
  snapshotPath <- jaspTools:::snapshot_relative_path(jaspTools:::ggplot_structure_snapshot_name("stale-failure-plot"))
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

test_that("vdiffr and fallback can use different plot objects", {
  rendered <- structure(list(), class = "rendered-plot")
  original <- structure(list(), class = "original-plot")
  seen <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    capture_vdiffr_expectation = function(name, test) {
      seen$vdiffr <- test
      # a mismatch against an existing reference, so not a fresh snapshot
      list(exception = simpleError("forced mismatch"), new_svg_path = tempfile(fileext = ".svg"),
           reference_existed = TRUE, new_svg_current = TRUE)
    },
    expect_doppelganger_fallback = function(test, ...) {
      seen$fallback <- test
      list(passed = TRUE)
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
