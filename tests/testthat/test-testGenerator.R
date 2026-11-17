context("test-generator")

test_that("getTests captures tables with empty data", {
  # Mock a JASP results structure with an empty table
  results <- list(
    emptyTable = list(
      data = list(),
      schema = list(fields = list()),
      title = "Empty Table"
    ),
    nonEmptyTable = list(
      data = list(
        list(col1 = 1, col2 = "a"),
        list(col1 = 2, col2 = "b")
      ),
      schema = list(fields = list(
        list(name = "col1", type = "number"),
        list(name = "col2", type = "string")
      )),
      title = "Non-Empty Table"
    )
  )

  tests <- jaspTools:::getTests(results)

  # Should capture both tables (2 total)
  expect_equal(length(tests), 2)

  # Verify both table types are captured
  types <- vapply(tests, function(t) t$type, character(1))
  expect_true(all(types == "table"))

  # Verify titles
  titles <- vapply(tests, function(t) t$title, character(1))
  expect_true("Empty Table" %in% titles)
  expect_true("Non-Empty Table" %in% titles)

  # Verify empty table has "list()" as data
  emptyTest <- tests[[which(titles == "Empty Table")]]
  expect_equal(emptyTest$data, "list()")

  # Verify non-empty table has actual data
  nonEmptyTest <- tests[[which(titles == "Non-Empty Table")]]
  expect_true(nchar(nonEmptyTest$data) > 6) # longer than "list()"
})

test_that("getTests captures tables nested inside containers", {
  # Mock a nested JASP results structure (container with tables)
  results <- list(
    topTable = list(
      data = list(list(x = 1)),
      schema = list(fields = list(list(name = "x", type = "number"))),
      title = "Top Level Table"
    ),
    container = list(
      title = "My Container",
      nestedTable = list(
        data = list(list(y = 2)),
        schema = list(fields = list(list(name = "y", type = "number"))),
        title = "Nested Table"
      ),
      emptyNestedTable = list(
        data = list(),
        schema = list(fields = list()),
        title = "Empty Nested Table"
      )
    )
  )

  tests <- jaspTools:::getTests(results)

  # Should capture all 3 tables
  expect_equal(length(tests), 3)

  titles <- vapply(tests, function(t) t$title, character(1))
  expect_true("Top Level Table" %in% titles)
  expect_true("Nested Table" %in% titles)
  expect_true("Empty Nested Table" %in% titles)
})

test_that("expect_equal_tables passes when both tables are empty", {
  # Both empty should succeed
  expect_silent(expect_equal_tables(list(), list()))
})

test_that("expect_equal_tables fails when ref is empty but test is not", {
  # Non-empty test with empty ref should fail
  expect_failure(
    expect_equal_tables(list(list(x = 1)), list())
  )
})
