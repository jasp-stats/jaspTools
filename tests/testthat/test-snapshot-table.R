snapshotExampleTable <- function() {
  list(
    title = "Effect estimates",
    schema = list(fields = list(
      list(name = "term", title = "Parameter", type = "string"),
      list(name = "estimate", title = "Estimate", overTitle = "Model", type = "mixed")
    )),
    data = list(
      list(term = "Alpha", estimate = list(value = 0.12345, format = "dp:3", type = "number")),
      list(term = "Beta", estimate = NULL, .isNewGroup = TRUE, .footnotes = list(estimate = list(0L)))
    ),
    footnotes = list(list(text = "Not estimated.", symbol = "a", rows = list("Beta"), cols = list("estimate")))
  )
}

test_that("table JSON is canonical and preserves finite numbers and JSON types", {
  x <- snapshotExampleTable()
  parse <- function(s) jsonlite::fromJSON(s, simplifyVector = FALSE)
  values <- c(1 - .Machine$double.eps / 2, 1 + .Machine$double.eps,
    pi, .Machine$double.xmin, .Machine$double.xmin * .Machine$double.eps,
    jsonlite::parse_json("3.6840148223564032e-34"), -0.0)
  x$data[[1L]]$estimate$value <- as.list(values)
  x$schema$fields[[1L]]$.footnotes <- 0L
  x$data[[2L]]$.footnotes$estimate <- 0L
  x$data[[2L]]$emptyObject <- structure(list(), names = character())
  x$data[[2L]]$emptyArray <- list()
  encoded <- snapshotTableJSON(x)
  obj <- parse(encoded)
  expect_identical(unlist(obj$data[[1L]]$estimate$value), values)
  expect_identical(1 / tail(unlist(obj$data[[1L]]$estimate$value), 1), -Inf)
  expect_identical(obj$schema$fields[[1L]]$.footnotes, list(0L))
  expect_identical(obj$data[[2L]]$.footnotes$estimate, list(0L))
  expect_null(obj$data[[2L]]$estimate)
  expect_true("estimate" %in% names(obj$data[[2L]]))
  expect_identical(names(obj$data[[2L]]$emptyObject), character())
  expect_null(names(obj$data[[2L]]$emptyArray))
  expect_identical(encoded, snapshotTableJSON(x[rev(names(x))]))
  expect_identical(encoded, snapshotTableJSON(obj))
})

test_that("adding a column keeps common cells aligned and exposes a fifth decimal change", {
  a <- snapshotExampleTable()
  b <- a
  b$schema$fields <- append(b$schema$fields, list(list(name = "se", title = "SE")), after = 1)
  b$data[[1]]$se <- 0.04
  b$data[[2]]$se <- 0.07
  b$data[[1]]$estimate$value <- 0.12346
  diff <- snapshotTableDiff(a, b)
  expect_identical(vapply(diff$changes, `[[`, "", "kind"), c("column_added", "cell"))
  change <- diff$changes[[2L]]
  expect_identical(change$column, "estimate")
  expect_identical(change$rowLabel, "Alpha")
  expect_identical(change$path, "value")
  expect_identical(change$decimalPlace, 5L)
  expect_equal(as.double(change$delta), 1e-5)
  expect_identical(change$old, "0.12345")
  expect_identical(change$new, "0.12346")
  expect_identical(snapshotTableDiff(a, a)$changes, list())
})

test_that("row insertion and reordering use stable row labels", {
  a <- snapshotExampleTable()
  b <- a
  b$data <- c(list(list(term = "Gamma", estimate = 1)), rev(a$data))
  d <- snapshotTableDiff(a, b)
  expect_identical(vapply(d$changes, `[[`, "", "kind"), c("row_order", "row_added"))
  expect_identical(lapply(d$rows, `[[`, "oldRow"), list(NULL, 2L, 1L))
  expect_identical(lapply(snapshotTableDiff(b, a)$rows, `[[`, "oldRow"), list(3L, 1L, 2L))
})

test_that("metadata, footnotes, groups and missing cells are reviewable", {
  a <- snapshotExampleTable()
  b <- a
  b$data[[2]]$estimate <- NULL
  b$data[[2]]$.isNewGroup <- FALSE
  b$footnotes[[1]]$text <- "Unavailable."
  b$schema$fields[[2]]$overTitle <- "Fit"
  d <- snapshotTableDiff(a, b)
  expect_setequal(vapply(d$changes, `[[`, "", "kind"), c("cell", "row_metadata", "column_metadata", "table_metadata"))
  cell <- Filter(function(x) x$kind == "cell", d$changes)[[1L]]
  expect_identical(c(cell$old, cell$new), c("null", "<absent>"))
  a$data <- list()
  a$error <- list(type = "badData", errorMessage = "Disconnected network")
  expect_match(snapshotTableJSON(a), "Disconnected network")
  expect_equal(length(snapshotTableDiff(NULL, a)$changes), 1L)
  expect_equal(length(snapshotTableDiff(a, NULL)$rows), 0L)
})

test_that("empty shapes and cell footnote attachments produce identifiable changes", {
  a <- snapshotExampleTable()
  b <- a
  b$data[[2]]$.footnotes$estimate <- list(1L)
  change <- snapshotTableDiff(a, b)$changes[[1L]]
  expect_identical(change$kind, "cell")
  expect_identical(change$column, "estimate")
  expect_identical(change$path, ".footnotes[1]")
  expect_identical(change$newRow, 2L)

  a <- list(schema = list(fields = NULL), data = list())
  b <- list(schema = list(fields = list()), data = NULL)
  d <- snapshotTableDiff(a, b)$changes
  expect_identical(vapply(d, `[[`, "", "path"), c("data", "schema.fields"))
  expect_identical(vapply(d, `[[`, "", "old"), c("[]", "null"))
  b$schema$fields <- NULL
  expect_identical(snapshotTableDiff(a, b)$changes[[2]]$new, "<absent>")
})

test_that("continuous fields use the larger absolute or symmetric relative allowance", {
  policy <- snapshotTableTolerance(rtol = 0.125, atol = 1)
  a <- list(schema = list(fields = list(list(name = "df", type = "number"))),
            data = lapply(c(0, 7, 6.5, -7), function(df) list(df = df)))
  b <- a
  b$data <- lapply(c(1, 8, 8, -8), function(df) list(df = df))
  d <- snapshotTableDiff(a, b, policy, merge = TRUE)
  # Rows 1, 2 and 4 are on the boundary. Row 3 would pass an additive rule.
  expect_length(d$changes, 1L)
  expect_identical(d$changes[[1L]]$newRow, 3L)
  expect_identical(d$changes[[1L]]$tolerance, list(rtol = 0.125, atol = 1, limit = 1))
  expect_identical(unlist(d$merged$data, use.names = FALSE), c(0, 7, 8, -7))
  expect_identical(snapshotTableDiff(b, a, policy)$changes[[1L]]$newRow, 3L)
})

test_that("only numeric payloads in aligned result cells can be merged", {
  a <- snapshotExampleTable()
  a$schema$fields[[3L]] <- list(name = "id", type = "integer")
  a$data[[1L]]$id <- 1000000000L
  a$data[[1L]]$estimate <- list(type = "number", value = list(2, 4), annotation = 1)
  b <- a
  b$data <- rev(b$data)
  b$data[[2L]]$id <- 1000000001L
  b$data[[2L]]$estimate$value <- list(2 + 1e-10, 5)
  b$data[[2L]]$estimate$annotation <- 1 + 1e-10
  b$data[[1L]]$estimate <- NULL
  b$data[[1L]]$.footnotes$estimate <- list(1e-13)
  b$schema$fields[[1L]]$width <- 1e-14
  b$status <- "error"
  d <- snapshotTableDiff(a, b, snapshotTableTolerance(), merge = TRUE)
  expected <- b
  expected$data[[2L]]$estimate$value[[1L]] <- 2
  expect_identical(d$merged, expected)
  expect_length(d$changes, 8L)
  expect_true(any(vapply(d$changes, function(x) identical(x$path, "annotation"), logical(1))))
  exact <- snapshotTableDiff(a, d$merged)$changes
  expect_identical(lapply(d$changes, function(x) x[setdiff(names(x), "tolerance")]), exact)

  b <- a
  b$data[[1L]]$estimate$type <- "integer"
  b$data[[1L]]$estimate$value[[1L]] <- 2 + 1e-10
  d <- snapshotTableDiff(a, b, snapshotTableTolerance(), merge = TRUE)
  expect_length(d$changes, 2L)
  expect_identical(d$merged, b)
})

test_that("column overrides preserve exact comparisons for untyped values", {
  fields <- list(list(name = "untyped"), list(name = "unknown", type = "custom"),
                 list(name = "bare", type = "mixed"))
  a <- list(schema = list(fields = fields), data = list(list(untyped = 1, unknown = 1, bare = 1)))
  b <- a
  b$data[[1L]] <- lapply(a$data[[1L]], function(x) x + 1e-10)
  ids <- .snapshotFieldNames(a)
  policy <- snapshotTableTolerance(columns = lapply(b$data[[1L]], function(x) list(atol = 1)),
                                   fields = ids)
  d <- snapshotTableDiff(a, b, policy, merge = TRUE)
  expect_identical(vapply(d$changes, `[[`, "", "column"), ids)
  expect_identical(d$merged, b)
})

test_that("declared p-values use a pure relative allowance unless the column opts in", {
  a <- snapshotExampleTable()
  a$schema$fields[[3L]] <- list(name = "p", type = "pvalue")
  a$data[[1L]]$p <- 1e-22
  a$data[[1L]]$estimate <- list(type = "pvalue", value = 1e-22)
  b <- a
  b$data[[1L]]$p <- 0
  b$data[[1L]]$estimate$value <- 1e-19
  d <- snapshotTableDiff(a, b, snapshotTableTolerance(atol = 0.1))
  expect_length(d$changes, 2L)
  expect_true(all(vapply(d$changes, function(x) x$tolerance$atol == 0, logical(1))))
  policy <- snapshotTableTolerance(columns = list(p = list(atol = 1e-20)))
  d <- snapshotTableDiff(a, b, policy, merge = TRUE)
  expect_identical(d$merged$data[[1L]]$p, a$data[[1L]]$p)
  expect_identical(d$changes[[1L]]$column, "estimate")
  expect_identical(d$merged$data[[1L]]$estimate, b$data[[1L]]$estimate)
})

test_that("tolerances validate scopes, component names and finite values", {
  expect_error(snapshotTableTolerance(rtol = NA_real_), "rtol")
  expect_error(snapshotTableTolerance(atol = -1), "atol")
  expect_error(snapshotTableTolerance(columns = list(list(atol = 1))), "column names")
  expect_error(snapshotTableTolerance(columns = list(x = list(rtol = Inf))), "x rtol")
  expect_error(snapshotTableTolerance(columns = list(x = c(rtoll = 1))), "Specify")
  expect_error(snapshotTableTolerance(columns = list(x = NULL)), "Specify")
  expect_error(snapshotTableTolerance(columns = list(x = list(atol = 1)), fields = "y"), "Unknown.*x")
  original <- snapshotTableTolerance()
  scoped <- function() {
    local_table_snapshot_tolerance(rtol = 1e-6)
    snapshotTableTolerance(atol = 1e-9)
  }
  expect_identical(scoped(), list(rtol = 1e-6, atol = 1e-9, columns = NULL))
  expect_identical(snapshotTableTolerance(), original)
})

test_that("strict numeric comparison preserves signed zero and finite extremes", {
  a <- list(schema = list(fields = list(list(name = "x", type = "number"))),
            data = list(list(x = -0.0), list(x = -.Machine$double.xmax)))
  b <- a
  b$data[[1L]]$x <- 0
  b$data[[2L]]$x <- .Machine$double.xmax
  strict <- snapshotTableDiff(a, b, snapshotTableTolerance(0, 0), merge = TRUE)
  expect_length(strict$changes, 2L)
  expect_identical(1 / strict$merged$data[[1L]]$x, Inf)
  loose <- snapshotTableDiff(a, b, snapshotTableTolerance(1.1, 0), merge = TRUE)
  expect_length(loose$changes, 1L)
  expect_type(loose$changes[[1L]]$tolerance$limit, "character")
  expect_identical(1 / loose$merged$data[[1L]]$x, -Inf)
  expect_length(snapshotTableDiff(a, b, snapshotTableTolerance(2, 0))$changes, 0L)
})
