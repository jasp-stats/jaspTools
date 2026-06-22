context("expect_equal_tables")

test_that("expect_equal_tables compares table strings literally", {
  table <- list(
    list(effect = "Sleep", p = 0.1),
    list(effect = "Chronotype", p = 0.2)
  )

  ref <- list(
    "Sleep", 0.1,
    "Chronotype", 0.2
  )

  expect_silent(jaspTools::expect_equal_tables(table, ref))
})

test_that("expect_equal_tables does not treat encoded names as wildcards", {
  table <- list(
    list(effect = "Sleep", p = 0.1),
    list(effect = "Chronotype", p = 0.2)
  )

  ref <- list(
    "jaspColumn4", 0.1,
    "jaspColumn2", 0.2
  )

  expect_error(jaspTools::expect_equal_tables(table, ref), "not equal")
})
