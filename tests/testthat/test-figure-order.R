test_that("figures are put back in the order they were made", {
  # JASP/jaspSyntax names: the counter after the first underscore only goes up; it is
  # compared as a number, so _10_ comes after _2_
  names <- c("resources/_10_t1793786457535.png", "resources/_0_t1793786454210.png",
             "resources/_2_t1793786455231.png",  "resources/3/_4_t1793786457066.png")
  expect_identical(names[jaspTools:::orderFiguresByCreation(names)],
                   names[c(2, 3, 4, 1)])

  # old jaspResults names
  names <- c("plots/3.png", "plots/1.png", "plots/12.png", "plots/2.png")
  expect_identical(names[jaspTools:::orderFiguresByCreation(names)],
                   c("plots/1.png", "plots/2.png", "plots/3.png", "plots/12.png"))
})
