localSnapshotDecisionModule <- function(env = parent.frame()) {
  module <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(module, "tests/testthat/_snaps/demo/reference_plotobject"), recursive = TRUE)
  module
}

writeSnapshotDecisionFile <- function(module, rows, version = 1L) {
  file <- file.path(module, "decisions.json")
  jsonlite::write_json(list(version = version, decisions = rows), file, auto_unbox = TRUE, null = "null")
  file
}

snapshotDecisionRow <- function(module, id, action, comment = "") {
  root <- file.path(module, "tests/testthat/_snaps")
  hash <- function(name) {
    if (file.exists(file.path(root, name)))
      digest::digest(file = file.path(root, name), algo = "sha256")
  }
  candidate <- sub("(\\.[^.]+)$", ".new\\1", id)
  structure <- file.path(dirname(id), "reference_plotobject", sub("\\.svg$", ".rds", basename(id)))
  list(id = id, action = action, comment = comment, baselineHash = hash(id), candidateHash = hash(candidate),
       structureBaselineHash = if (endsWith(id, ".svg")) hash(structure),
       structureCandidateHash = if (endsWith(id, ".svg")) hash(sub("\\.rds$", ".new.rds", structure)))
}

test_that("review promotes new references and plot companions and preserves kept candidates and comments", {
  module <- localSnapshotDecisionModule()
  folder <- file.path(module, "tests/testthat/_snaps/demo")
  writeLines("new table", file.path(folder, "a.table.new.json"))
  writeLines("old HTML", file.path(folder, "b.html"))
  writeLines("new HTML", file.path(folder, "b.new.html"))
  writeLines("old SVG", file.path(folder, "c.svg"))
  writeLines("new SVG", file.path(folder, "c.new.svg"))
  structure <- file.path(folder, "reference_plotobject")
  writeBin(as.raw(0:9), file.path(structure, "c.rds"))
  writeBin(as.raw(1:10), file.path(structure, "c.new.rds"))
  rows <- list(snapshotDecisionRow(module, "demo/a.table.json", "accept", "First table"),
               snapshotDecisionRow(module, "demo/b.html", "keep", "Check the second paragraph\nbefore merging"),
               snapshotDecisionRow(module, "demo/c.svg", "accept", "Axes changed"))
  file <- writeSnapshotDecisionFile(module, rows)

  result <- applySnapshotReview(file, module)
  expect_identical(result$id, vapply(rows, `[[`, "", "id"))
  expect_identical(result$comment, vapply(rows, `[[`, "", "comment"))
  expect_identical(result$action, c("accept", "keep", "accept"))
  expect_identical(result$status, c("accepted", "kept", "accepted"))
  expect_identical(readLines(file.path(folder, "a.table.json")), "new table")
  expect_identical(readLines(file.path(folder, "b.html")), "old HTML")
  expect_identical(readLines(file.path(folder, "b.new.html")), "new HTML")
  expect_identical(readLines(file.path(folder, "c.svg")), "new SVG")
  expect_identical(readBin(file.path(structure, "c.rds"), "raw", 10), as.raw(1:10))
  expect_false(any(file.exists(file.path(folder, c("a.table.new.json", "c.new.svg", "reference_plotobject/c.new.rds")))))
  expect_identical(applySnapshotReview(file, module)$status, c("already accepted", "kept", "already accepted"))
})

test_that("changed or newly appeared references reject the whole batch, including stale keeps", {
  module <- localSnapshotDecisionModule()
  folder <- file.path(module, "tests/testthat/_snaps/demo")
  writeLines("first", file.path(folder, "a.new.html"))
  writeLines("second", file.path(folder, "b.new.svg"))
  first <- snapshotDecisionRow(module, "demo/a.html", "accept")
  second <- snapshotDecisionRow(module, "demo/b.svg", "keep")
  file <- writeSnapshotDecisionFile(module, list(first, second))
  writeLines("appeared", file.path(folder, "reference_plotobject/b.rds"))
  expect_error(applySnapshotReview(file, module), "changed since review.*demo/b.svg")
  expect_false(file.exists(file.path(folder, "a.html")))
  expect_true(file.exists(file.path(folder, "a.new.html")))
  second <- snapshotDecisionRow(module, "demo/b.svg", "accept")
  file <- writeSnapshotDecisionFile(module, list(second))
  writeLines("different", file.path(folder, "b.new.svg"))
  expect_error(applySnapshotReview(file, module), "changed since review")
  expect_false(file.exists(file.path(folder, "b.svg")))
  file <- writeSnapshotDecisionFile(module, list(snapshotDecisionRow(module, "demo/b.svg", "accept")))
  expect_identical(applySnapshotReview(file, module)$status, "accepted")
  expect_identical(readLines(file.path(folder, "reference_plotobject/b.rds")), "appeared")
})

test_that("declines and comment-only reviews keep the files and return distinct statuses with agent notes", {
  module <- localSnapshotDecisionModule()
  folder <- file.path(module, "tests/testthat/_snaps/demo")
  files <- file.path(folder, c("a.svg", "a.new.svg", "reference_plotobject/a.rds",
                               "reference_plotobject/a.new.rds", "b.new.html"))
  for (i in seq_along(files))
    writeBin(as.raw(i), files[i])
  notes <- c("Restore the confidence interval.", "Which row contributes this count?\nPlease check the NA case.")
  rows <- list(snapshotDecisionRow(module, "demo/a.svg", "decline", notes[1L]),
               snapshotDecisionRow(module, "demo/b.html", "comment", notes[2L]))
  file <- writeSnapshotDecisionFile(module, rows)
  result <- applySnapshotReview(file, module)
  expect_identical(result$action, c("decline", "comment"))
  expect_identical(result$status, c("declined", "unresolved"))
  expect_identical(result$comment, notes)
  expect_identical(lapply(files, readBin, what = "raw", n = 1), lapply(seq_along(files), as.raw))
  expect_false(file.exists(file.path(folder, "b.html")))
  expect_identical(applySnapshotReview(file, module), result)
  writeLines("changed", files[4L])
  expect_error(applySnapshotReview(file, module), "changed since review.*demo/a.svg")
  writeBin(as.raw(4), files[4L])
  writeLines("changed", files[5L])
  expect_error(applySnapshotReview(file, module), "changed since review.*demo/b.html")
})

test_that("an SVG-only acceptance retires its reviewed fallback and replays", {
  module <- localSnapshotDecisionModule()
  root <- snapshotReviewRoot(module)
  id <- "demo/a.svg"
  paths <- snapshotReviewPaths(root, id)
  writeLines("old SVG", paths["baseline"])
  writeLines("new SVG", paths["candidate"])
  saveRDS("old structure", paths["structureBaseline"])
  item <- snapshotReviewItem(root, id)
  expect_true(item$removeStructure)
  row <- snapshotDecisionRow(module, id, "decline")
  row$removeStructure <- item$removeStructure
  apply <- function(version = 2L)
    applySnapshotReview(writeSnapshotDecisionFile(module, list(row), version), module)
  expect_identical(apply()$status, "declined")
  expect_identical(readRDS(paths["structureBaseline"]), "old structure")
  row$action <- "accept"
  expect_error(apply(1L), "Invalid plot structure removal")
  saveRDS("different", paths["structureBaseline"])
  expect_error(apply(), "changed since review")
  expect_identical(readLines(paths["baseline"]), "old SVG")
  saveRDS("old structure", paths["structureBaseline"])
  local({
    hashes <- snapshotDecisionHashes
    testthat::local_mocked_bindings(snapshotDecisionHashes = function(root, paths) {
      if (length(paths) == 4L && !file.exists(paths["structureBaseline"]))
        stop("Final review check failed")
      hashes(root, paths)
    })
    expect_error(apply(), "Final review check failed")
  })
  expect_identical(readRDS(paths["structureBaseline"]), "old structure")
  expect_identical(readLines(paths["baseline"]), "old SVG")
  expect_identical(readLines(paths["candidate"]), "new SVG")
  expect_identical(apply()$status, "accepted")
  expect_identical(readLines(paths["baseline"]), "new SVG")
  expect_false(any(file.exists(paths[-1L])))
  expect_identical(apply()$status, "already accepted")
})

test_that("review validates decisions and rediscovers the permitted ids", {
  module <- localSnapshotDecisionModule()
  folder <- file.path(module, "tests/testthat/_snaps/demo")
  writeLines("baseline", file.path(folder, "a.html"))
  row <- snapshotDecisionRow(module, "demo/a.html", "accept")
  apply <- function(rows) applySnapshotReview(writeSnapshotDecisionFile(module, rows), module)
  expect_equal(nrow(apply(list())), 0L)
  expect_error(apply(list(row)), "without a reviewed candidate")
  row$action <- "keep"
  expect_identical(apply(list(row))$status, "kept")
  row$removeStructure <- TRUE
  expect_error(apply(list(row)), "Invalid plot structure removal")
  row$removeStructure <- NULL
  expect_error(apply(list(row, row)), "more than once")
  row$id <- "../a.html"
  expect_error(apply(list(row)), "Unknown snapshot id")
  row$baselineHash <- "abc"
  expect_error(apply(list(row)), "Invalid baselineHash")
  row$structureCandidateHash <- NULL
  expect_error(apply(list(row)), "four file hashes")
})

test_that("a failure partway through a plot acceptance restores both originals and candidates", {
  module <- localSnapshotDecisionModule()
  folder <- file.path(module, "tests/testthat/_snaps/demo")
  files <- file.path(folder, c("a.svg", "a.new.svg", "reference_plotobject/a.rds", "reference_plotobject/a.new.rds"))
  for (i in seq_along(files))
    writeBin(as.raw(i), files[i])
  file <- writeSnapshotDecisionFile(module, list(snapshotDecisionRow(module, "demo/a.svg", "accept")))
  hashes <- snapshotDecisionHashes
  testthat::local_mocked_bindings(snapshotDecisionHashes = function(root, paths) {
    if (length(paths) == 2L && endsWith(paths[1L], "/a.rds"))
      stop("Simulated structure update failure")
    hashes(root, paths)
  })
  expect_error(applySnapshotReview(file, module), "Simulated structure update failure")
  expect_identical(lapply(files, readBin, what = "raw", n = 1), lapply(seq_along(files), as.raw))
  expect_false(any(grepl("jaspTools-review", list.files(folder, recursive = TRUE, all.files = TRUE))))
})

test_that("acceptance leaves read-only references and candidates intact", {
  module <- localSnapshotDecisionModule()
  folder <- file.path(module, "tests/testthat/_snaps/demo")
  files <- file.path(folder, c("a.html", "a.new.html"))
  for (i in seq_along(files))
    writeLines(as.character(i), files[i])
  row <- snapshotDecisionRow(module, "demo/a.html", "accept")
  decision <- writeSnapshotDecisionFile(module, list(row))
  original <- tools::md5sum(files)
  withr::defer(Sys.chmod(files, "0644"))
  for (path in files) {
    expect_true(Sys.chmod(path, "0444"))
    expect_error(applySnapshotReview(decision, module), "Cannot change read-only file")
    expect_identical(tools::md5sum(files), original)
    expect_true(Sys.chmod(path, "0644"))
  }
  expect_false(any(grepl("[.]jaspTools-", list.files(folder, recursive = TRUE, all.files = TRUE))))
})

test_that("linked companion directories cannot redirect an acceptance", {
  module <- localSnapshotDecisionModule()
  folder <- file.path(module, "tests/testthat/_snaps/demo")
  outside <- file.path(module, "outside")
  dir.create(outside)
  writeLines("outside", file.path(outside, "a.rds"))
  writeLines("unrelated", file.path(outside, "notice.new.html"))
  writeLines("new", file.path(folder, "a.new.svg"))
  structure <- file.path(folder, "reference_plotobject")
  unlink(structure, recursive = TRUE)
  linked <- if (.Platform$OS.type == "windows")
    Sys.junction(outside, structure)
  else
    suppressWarnings(file.symlink(outside, structure))
  skip_if_not(linked, "Creating directory symlinks is unavailable")
  expect_identical(snapshotReviewIds(module), "demo/a.svg")
  file <- writeSnapshotDecisionFile(module, list(snapshotDecisionRow(module, "demo/a.svg", "accept")))
  expect_error(applySnapshotReview(file, module), "[Ll]inked|[Uu]nsafe|[Oo]utside")
  expect_false(file.exists(file.path(folder, "a.svg")))
  expect_identical(readLines(file.path(outside, "a.rds")), "outside")
})

test_that("review discovery scopes named test files and their variants", {
  module <- localSnapshotDecisionModule()
  root <- snapshotReviewRoot(module)
  for (dir in c("model A.v2", "windows/model A.v2", "other")) {
    dir.create(file.path(root, dir), recursive = TRUE)
    writeLines("candidate", file.path(root, dir, "notice.new.html"))
  }
  testFile <- file.path(module, "tests", "testthat", "test_model A.v2.r")
  expect_identical(snapshotReviewIds(module, files = testFile),
                   c("model A.v2/notice.html", "windows/model A.v2/notice.html"))
  expect_identical(snapshotReviewIds(module, files = character()), character())
})

test_that("an empty review leaves the directory for future snapshots available", {
  module <- withr::local_tempdir()
  dir.create(file.path(module, "tests", "testthat"), recursive = TRUE)
  root <- snapshotReviewRoot(module)
  expect_error(reviewSnapshots(module, output = root, open = FALSE), "outside the _snaps directory")
  expect_false(file.exists(root))
})
