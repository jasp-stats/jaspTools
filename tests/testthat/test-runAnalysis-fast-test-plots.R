context("runAnalysis fast test plots")

test_that("test plot image writer preserves objects without rendering", {
  skip_if_not_installed("jaspBase")

  ns <- asNamespace("jaspBase")
  original <- get("writeImageJaspResults", envir = ns)

  restore <- jaspTools:::useFastTestPlotImages()
  on.exit(restore(), add = TRUE)

  fast <- get("writeImageJaspResults", envir = ns)
  expect_true(isTRUE(attr(fast, "jaspToolsFastTestPlotImages")))

  plot <- structure(list(x = 1), class = "unit-test-plot")
  image <- fast(
    plot = plot,
    obj = TRUE,
    location = list(root = tempdir(), relativePath = "test-fast-plot.png")
  )

  expect_identical(image[["png"]], "test-fast-plot.png")
  expect_identical(image[["obj"]], plot)
  expect_false(file.exists(file.path(tempdir(), "test-fast-plot.png")))

  restore()
  expect_identical(get("writeImageJaspResults", envir = ns), original)
})

test_that("test plot image writer stores decoded objects", {
  skip_if_not_installed("jaspBase")
  skip_if_not_installed("ggplot2")

  ns <- asNamespace("jaspBase")
  skip_if_not(exists(".decodeJaspPlotObject", envir = ns, inherits = FALSE))

  oldDecoder <- if (exists(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)) {
    get(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  hadDecoder <- exists(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (hadDecoder) {
      assign(".decodeColNamesLax", oldDecoder, envir = .GlobalEnv)
    } else if (exists(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".decodeColNamesLax", envir = .GlobalEnv)
    }
  }, add = TRUE)

  assign(
    ".decodeColNamesLax",
    function(x) gsub("JaspColumn_1_Encoded", "group", x, fixed = TRUE),
    envir = .GlobalEnv
  )

  restore <- jaspTools:::useFastTestPlotImages()
  on.exit(restore(), add = TRUE)

  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "JaspColumn_1_Encoded")

  fast <- get("writeImageJaspResults", envir = ns)
  image <- fast(
    plot = plot,
    obj = TRUE,
    location = list(root = tempdir(), relativePath = "test-fast-decoded-plot.png")
  )

  expect_identical(unname(image[["obj"]]$labels$x), "group")
  expect_false(file.exists(file.path(tempdir(), "test-fast-decoded-plot.png")))
})
