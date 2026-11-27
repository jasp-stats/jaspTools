testthat::local_edition(3)
changed <- file.exists("changed-output")

# These small result nodes follow the schema returned by runAnalysis().
# The group sizes, means, ranges and t-test come from JASP's Invisibility Cloak data.
testthat::test_that("the independent samples test reports its statistic", {
  node <- list(
    name = "ttest", title = "Independent Samples T-Test", status = "complete",
    schema = list(fields = list(
      list(name = "v", title = "Variable", type = "string"),
      list(name = "test", title = "Test", type = "string"),
      list(name = "Statistic", title = "Statistic", type = "number", format = "sf:4;dp:3")
    )),
    data = list(list(v = "Mischief", test = "Student",
                     Statistic = if (changed) -1.7134693839651469 else -1.7134593839651469)),
    footnotes = list()
  )
  jaspTools::expect_snapshot_table(node, "independent-samples")
})

testthat::test_that("group descriptives retain the mixed cells and footnote", {
  fields <- list(
    list(name = "Cloak", title = "Cloak", type = "string"),
    list(name = "Valid", title = "Valid", type = "integer"),
    list(name = "Minimum", title = "Minimum", type = "mixed")
  )
  rows <- list(
    list(Cloak = "Absent", Valid = 12L, Minimum = list(value = 0, type = "number", format = "dp:0")),
    list(Cloak = "Present", Valid = 12L, Minimum = list(value = 2, type = "number", format = "dp:0"))
  )
  note <- "Valid counts exclude missing observations."
  if (changed) {
    fields[[2]] <- list(name = "Maximum", title = "Maximum", type = "integer")
    rows[[1]]$Valid <- rows[[2]]$Valid <- NULL
    rows[[1]]$Maximum <- 6L
    rows[[2]]$Maximum <- 8L
    rows[[2]]$Minimum$value <- 3
    note <- "Ranges are shown for each cloak condition."
  }
  node <- list(
    name = "descriptives", title = "Group Descriptives", status = "complete",
    schema = list(fields = fields), data = rows,
    footnotes = list(list(cols = NULL, rows = NULL, myOrder = 0L,
                          symbol = "<em>Note.</em>", text = note))
  )
  jaspTools::expect_snapshot_table(node, "group-descriptives")
})

testthat::test_that("the analysis summary includes both group means", {
  text <- if (changed)
    "Mean mischief was <strong>3.75</strong> without the cloak and <strong>5.50</strong> with it."
  else
    "Mean mischief was <strong>3.75</strong> without the cloak and <strong>5.00</strong> with it."
  node <- list(name = "summary", title = "Summary", elementType = "p",
               class = "", maxWidth = "15cm", rawtext = text,
               text = paste0("<p>", text, "</p>"))
  jaspTools::expect_snapshot_html(node, "analysis-summary")
})

testthat::test_that("the plot shows the two group means", {
  plotMeans <- function() {
    means <- c(3.75, if (changed) 5.50 else 5.00)
    graphics::barplot(means, names.arg = c("Cloak absent", "Cloak present"),
                      ylab = "Mean mischief", ylim = c(0, 6),
                      col = c("#a7bac7", "#42647c"), border = NA)
  }
  jaspTools::expect_equal_plots(plotMeans, "group-means")
})
