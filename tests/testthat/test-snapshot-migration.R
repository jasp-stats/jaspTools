migrationModule <- function(path, reference = "1.25") {
  tests <- file.path(path, "tests", "testthat")
  dir.create(tests, recursive = TRUE)
  writeLines(c(
    'results <- list(results = list(model = list(title = "Model",',
    '  schema = list(fields = list(list(name = "estimate", type = "number"))),',
    '  data = list(list(estimate = 1.250000000000003)))))',
    'test_that("model estimate", {',
    '  table <- results[["results"]][["model"]][["data"]]',
    paste0('  jaspTools::expect_equal_tables(table, list(', reference, '))'),
    '})'), file.path(tests, "test-model.R"))
  path
}

test_that("multi-module migration validates legacy results before publishing each file", {
  root <- withr::local_tempdir()
  good <- migrationModule(file.path(root, "good"))
  bad <- migrationModule(file.path(root, "bad"), "0.5")
  modules <- c(good, bad)
  files <- file.path(modules, "tests", "testthat", "test-model.R")
  dir.create(file.path(good, "examples"))
  writeLines("fixture contents", file.path(good, "examples", "data.txt"))
  cat('\ntest_that("module-root fixture", {\n  expect_equal(readLines(testthat::test_path("..", "..", "examples", "data.txt")), "fixture contents")\n})\n',
      file = files[1], append = TRUE)
  before <- tools::md5sum(files)
  plan <- migrateSnapshotTests(modules, files = "test-model.R")
  expect_identical(plan$status, c("ready", "ready"))
  expect_identical(tools::md5sum(files), before)
  expect_false(any(dir.exists(file.path(dirname(files), "_snaps"))))

  result <- migrateSnapshotTests(modules, apply = TRUE)
  expect_identical(result$status, c("migrated", "blocked"), info = paste(result$reason, collapse = "\n"))
  expect_identical(tools::md5sum(files[2]), before[2])
  expect_false(dir.exists(file.path(dirname(files[2]), "_snaps")))
  expect_match(snapshotText(files[1]), "expect_snapshot_table", fixed = TRUE)
  refs <- list.files(file.path(dirname(files[1]), "_snaps"), recursive = TRUE, full.names = TRUE)
  expect_length(refs, 1L)
  node <- jsonlite::fromJSON(refs, simplifyVector = FALSE)
  expect_identical(node$data[[1]]$estimate, 1.250000000000003)
  expect_identical(node$schema$fields[[1]]$name, "estimate")
  after <- tools::md5sum(c(files[1], refs))
  expect_identical(migrateSnapshotTests(good, apply = TRUE)$status, "unchanged")
  expect_identical(tools::md5sum(c(files[1], refs)), after)
})

test_that("migration scopes filenames and reports unusable files without executing dry runs", {
  module <- migrationModule(withr::local_tempdir())
  dir <- file.path(module, "tests", "testthat")
  writeLines("stop('dry run executed the test')", file.path(dir, "test-unrelated.R"))
  writeLines("options(jaspRoundToPrecision = function(x) round(x, 3))", file.path(dir, "helper-round.R"))
  expect_identical(migrateSnapshotTests(module, "test-unrelated.R")$status, "unchanged")
  result <- migrateSnapshotTests(module, "test-model.R")
  expect_identical(result$file, "test-model.R")
  expect_identical(result$status, "manual")
  expect_match(result$reason, "helper-round.R", fixed = TRUE)
  reviewed <- migrateSnapshotTests(module, "test-model.R", allowCustomRounding = TRUE)
  expect_identical(reviewed$status, "ready")
  bad <- file.path(module, "absent")
  both <- migrateSnapshotTests(c(bad, module), "test-model.R")
  expect_identical(both$status, c("blocked", "manual"))
  expect_error(migrateSnapshotTests(module, "test-missing.R"), "not found")
  expect_error(migrateSnapshotTests(module, "../test-model.R"), "exact test filenames")
})

test_that("migration preserves pending review candidates and unapproved precision rules", {
  module <- migrationModule(withr::local_tempdir())
  dir <- file.path(module, "tests", "testthat")
  path <- file.path(dir, "test-model.R")
  sourceHash <- snapshotFileHash(path)
  name <- migrateSnapshotTests(module)$name
  candidate <- file.path(dir, "_snaps", "model", snapshotCandidatePath(snapshotFilename(name, "table")))
  writeSnapshotText("already awaiting review\n", candidate)
  stopped <- migrateSnapshotTests(module, apply = TRUE)
  expect_identical(stopped$status, "blocked")
  expect_match(stopped$reason, "pending candidate", fixed = TRUE)
  expect_identical(snapshotText(candidate), "already awaiting review\n")
  expect_identical(snapshotFileHash(path), sourceHash)

  writeLines(sub("1.250000000000003", "1.25001", readLines(path), fixed = TRUE), path)
  sourceHash <- snapshotFileHash(path)
  writeLines("options(jaspRoundToPrecision = identity)", file.path(dir, "setup-round.R"))
  stopped <- migrateSnapshotTests(module, apply = TRUE)
  expect_identical(stopped$status, "manual")
  expect_identical(snapshotFileHash(path), sourceHash)
  # Approval still runs the old exact comparison, which fails for this fixture.
  stopped <- migrateSnapshotTests(module, apply = TRUE, allowCustomRounding = TRUE)
  expect_identical(stopped$status, "blocked")
  expect_match(stopped$reason, "Legacy test", fixed = TRUE)
  expect_identical(snapshotFileHash(path), sourceHash)
})

test_that("each migration pass reads the module's original fixtures", {
  module <- migrationModule(withr::local_tempdir())
  dir <- file.path(module, "tests", "testthat")
  path <- file.path(dir, "test-model.R")
  fixture <- file.path(dir, "value.txt")
  writeLines("1.25", fixture)
  code <- sub("1.250000000000003", 'as.numeric(readLines("value.txt"))', readLines(path), fixed = TRUE)
  # Legacy rounding accepts either value, but the new snapshots distinguish them.
  writeLines(c(code, 'writeLines("1.25001", "value.txt")'), file.path(dir, "test-first.R"))
  writeLines(code, path)
  result <- migrateSnapshotTests(module, apply = TRUE)
  expect_identical(result$status, rep("migrated", 2L), info = paste(result$reason, collapse = "\n"))
  expect_identical(readLines(fixture), "1.25")
  refs <- list.files(file.path(dir, "_snaps"), recursive = TRUE, full.names = TRUE)
  expect_length(refs, 2L)
  for (ref in refs)
    expect_identical(jsonlite::fromJSON(ref, simplifyVector = FALSE)$data[[1]]$estimate, 1.25)
})
