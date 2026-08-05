context("view helpers")

test_that("html assets are copied into a fresh output directory", {
  sourceDir <- tempfile("jaspTools-html-src-")
  dir.create(file.path(sourceDir, "js"), recursive = TRUE)
  writeLines("<html></html>", file.path(sourceDir, "index-jasp.html"))
  writeLines("window.analysisChanged = function() {};", file.path(sourceDir, "js", "analysis.js"))

  oldJaspToolsPath <- .getInternal("jaspToolsPath")
  oldHtmlDir <- .pkgenv[["pkgOptions"]][["html.dir"]]
  tempJaspToolsPath <- tempfile("jaspTools-path-")
  dir.create(tempJaspToolsPath, recursive = TRUE)
  file.create(file.path(tempJaspToolsPath, "setup_complete.txt"))
  on.exit({
    .setInternal("jaspToolsPath", oldJaspToolsPath)
    .pkgenv[["pkgOptions"]][["html.dir"]] <- oldHtmlDir
  }, add = TRUE)
  .setInternal("jaspToolsPath", tempJaspToolsPath)
  .pkgenv[["pkgOptions"]][["html.dir"]] <- sourceDir

  outputDir <- tempfile("jaspTools-html-out-")
  expect_false(dir.exists(outputDir))

  moveJaspHtmlToDir(outputDir)

  expect_true(file.exists(file.path(outputDir, "index-jasp.html")))
  expect_true(file.exists(file.path(outputDir, "js", "analysis.js")))
})
