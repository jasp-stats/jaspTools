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
