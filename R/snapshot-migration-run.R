# Preserve the test's UTF-8 BOM, line endings and final newline when applying
# the source planner's line edits.
migrationSourceBytes <- function(lines, original) {
  if (is.raw(lines)) return(lines)
  source <- rawToChar(original)
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  hasBOM <- identical(utils::head(original, 3L), bom)
  hasCRLF <- grepl("\r\n", source, fixed = TRUE, useBytes = TRUE)
  other <- gsub("\r\n", "", source, fixed = TRUE, useBytes = TRUE)
  if (grepl("\r", other, fixed = TRUE, useBytes = TRUE) ||
      (hasCRLF && grepl("\n", other, fixed = TRUE, useBytes = TRUE)))
    stop("Source has mixed or unsupported line endings; review this file manually.", call. = FALSE)
  eol <- if (hasCRLF) "\r\n" else "\n"
  value <- paste(lines, collapse = "\n")
  value <- gsub("\n", eol, enc2utf8(value), fixed = TRUE)
  if (endsWith(source, "\n")) value <- paste0(value, eol)
  c(if (hasBOM) bom, charToRaw(value))
}

# Use the ordinary testthat helper/setup/teardown lifecycle for both passes.
migrationTestResults <- function(path, capture = NULL, phase) {
  # Some module helpers use global options() for their rounding policy.
  withr::local_options(list(jaspRoundToPrecision = getOption("jaspRoundToPrecision"),
                            jaspTools.tableSnapshot.tolerance = getOption("jaspTools.tableSnapshot.tolerance")))
  exports <- getNamespaceExports("jaspTools")
  env <- list2env(mget(exports, asNamespace("jaspTools"), inherits = TRUE), parent = globalenv())
  env$.jaspToolsCaptureTable <- capture
  results <- testthat::test_file(path, env = env, reporter = "silent")
  successes <- character()
  for (test in results) {
    for (result in test$results) {
      if (inherits(result, c("expectation_failure", "expectation_error"))) {
        message <- conditionMessage(result)
        if (nchar(message) > 500L) message <- paste0(substr(message, 1L, 497L), "...")
        stop(phase, " failed in ", test$test, ": ", message, call. = FALSE)
      }
      if (inherits(result, "expectation_success"))
        successes <- c(successes, result$message)
    }
  }
  successes
}

runSnapshotMigration <- function(modulePath, path, plan, sourceHash, allowCustomRounding = FALSE) {
  modulePath <- normalizePath(modulePath, winslash = "/", mustWork = TRUE)
  root <- dirname(snapshotReviewRoot(modulePath))
  path <- file.path(root, basename(path))
  if (!identical(unname(snapshotDecisionHashes(root, path)), sourceHash))
    stop("Test source changed since migration planning: ", basename(path), call. = FALSE)
  original <- readBin(path, "raw", n = file.size(path))
  rewritten <- migrationSourceBytes(plan$text, original)
  ready <- plan$sites$name[plan$sites$status == "ready"]
  files <- vapply(ready, snapshotFilename, "", type = "table", USE.NAMES = FALSE)
  if (!length(files) || anyDuplicated(files))
    stop("Expected uniquely named table snapshots to migrate.", call. = FALSE)
  captured <- list()
  # Tests and module helpers can update their fixtures. Each pass starts with
  # its own copy, just as the committed test will start from the module tree.
  captureTests <- normalizePath(copyMigrationTests(modulePath, withr::local_tempdir()), winslash = "/")
  script <- file.path(captureTests, basename(path))
  scratchSnaps <- file.path(captureTests, "_snaps")
  dir.create(scratchSnaps, showWarnings = FALSE)
  capture <- function(node, name) {
    if (!name %in% ready || name %in% names(captured))
      stop("Unexpected or repeated legacy comparison: ", name, call. = FALSE)
    if (!allowCustomRounding && !is.null(getOption("jaspRoundToPrecision")))
      stop("A custom jaspRoundToPrecision policy is active. Choose the table snapshot tolerance ",
           "and set allowCustomRounding = TRUE to migrate this comparison.", call. = FALSE)
    if (is.null(get_snapshotter()))
      stop("The legacy comparison must run in an active test_that() block.", call. = FALSE)
    file <- snapshotFilename(name, "table")
    location <- normalizePath(dirname(snapshot_relative_path(file)), winslash = "/", mustWork = FALSE)
    context <- substring(location, nchar(scratchSnaps) + 2L)
    parts <- strsplit(context, "/", fixed = TRUE)[[1L]]
    if (!startsWith(location, paste0(scratchSnaps, "/")) || !length(parts) ||
        length(parts) > 2L || any(parts %in% c("", ".", "..", ".jaspTools-review-lock")))
      stop("The test uses an unsupported snapshot directory: ", location, call. = FALSE)
    id <- file.path("_snaps", context, file)
    captured[[name]] <<- list(id = id, json = paste0(snapshotTableJSON(node), "\n"))
    invisible(NULL)
  }

  # testAnalysis() also runs optional expectations (including plots) off CRAN.
  withr::local_envvar(c(NOT_CRAN = "true"))
  writeBin(migrationSourceBytes(plan$capture, original), script)
  tryCatch(migrationTestResults(script, capture, "Legacy test"), error = function(e) {
    if (identical(utils::head(original, 3L), as.raw(c(0xef, 0xbb, 0xbf))) &&
        grepl(":1:1: unexpected", conditionMessage(e), fixed = TRUE))
      stop("testthat could not parse the test's UTF-8 byte-order mark. Save it as UTF-8 without a BOM and retry.",
           call. = FALSE)
    stop(e)
  })
  missing <- setdiff(ready, names(captured))
  if (length(missing))
    stop("Legacy comparison did not run: ", paste(missing, collapse = ", "), call. = FALSE)

  ids <- vapply(captured, `[[`, "", "id", USE.NAMES = FALSE)
  refs <- file.path(root, ids)
  candidates <- snapshotCandidatePath(refs)
  occupied <- !is.na(snapshotDecisionHashes(root, c(refs, candidates)))
  if (any(occupied))
    stop("Existing reference or pending candidate: ", c(refs, candidates)[which(occupied)[1L]], call. = FALSE)
  scratchTests <- normalizePath(copyMigrationTests(modulePath, withr::local_tempdir()), winslash = "/")
  script <- file.path(scratchTests, basename(path))
  scratchRefs <- file.path(scratchTests, ids)
  if (any(file.exists(c(scratchRefs, snapshotCandidatePath(scratchRefs)))))
    stop("A table reference or candidate already exists in the scratch test directory.", call. = FALSE)
  for (i in seq_along(captured))
    writeSnapshotText(captured[[i]]$json, scratchRefs[i])
  # A successful match removes the candidate at the precise path it compared.
  # This distinguishes a match in the captured variant from one in another.
  if (!all(file.create(snapshotCandidatePath(scratchRefs))))
    stop("Could not stage the scratch table candidates for verification.", call. = FALSE)
  hashes <- unname(snapshotDecisionHashes(scratchTests, scratchRefs))
  writeBin(rewritten, script)
  passed <- migrationTestResults(script, phase = "Rewritten test")
  # expect_snapshot_table() records its filename as its success message. Check
  # each one even if an unrelated test legitimately skips on this platform.
  unverified <- files[tabulate(match(passed, files), nbins = length(files)) != 1L]
  if (length(unverified))
    stop("Snapshot expectation did not pass exactly once: ", paste(unverified, collapse = ", "), call. = FALSE)
  if (!identical(unname(snapshotDecisionHashes(scratchTests, scratchRefs)), hashes) ||
      any(file.exists(snapshotCandidatePath(scratchRefs))))
    stop("Table references changed during verification.", call. = FALSE)

  # commitSnapshotDecisions() stages and rolls back a group of files under one
  # root. Copy only the verified source and the newly captured table references.
  stage <- file.path(root, basename(tempfile(".jaspTools-migrate-", tmpdir = root)))
  if (!dir.create(stage))
    stop("Could not create migration staging directory.", call. = FALSE)
  on.exit(unlink(stage, recursive = TRUE, expand = FALSE), add = TRUE)
  destinations <- c(refs, path)
  staged <- file.path(stage, basename(destinations))
  if (!all(file.copy(scratchRefs, staged[seq_along(refs)], copy.mode = FALSE)))
    stop("Could not stage the verified table references.", call. = FALSE)
  writeBin(rewritten, staged[length(staged)])
  after <- c(hashes, digest::digest(rewritten, algo = "sha256", serialize = FALSE))
  before <- c(rep(NA_character_, length(refs)), sourceHash)
  changes <- lapply(seq_along(staged), function(i)
    list(paths = c(destinations[i], staged[i]), hashes = c(before[i], after[i])))
  paths <- c(destinations, candidates)
  expected <- stats::setNames(c(after, rep(NA_character_, length(candidates))), paths)
  unchanged <- c(before, rep(NA_character_, length(candidates)))
  if (!identical(unname(snapshotDecisionHashes(root, paths)), unchanged))
    stop("Test source or snapshot files changed during migration: ", basename(path), call. = FALSE)
  # Clean up newly created empty directories if staging or publication fails.
  snapRoot <- file.path(root, "_snaps")
  lock <- file.path(snapRoot, ".jaspTools-review-lock")
  dirs <- unique(c(snapRoot, lock, dirname(dirname(refs)), dirname(refs)))
  created <- character()
  on.exit({
    for (dir in rev(created)) {
      if (dir.exists(dir) && !snapshotLinkedPaths(dir) && !length(list.files(dir, all.files = TRUE, no.. = TRUE)))
        unlink(dir, recursive = TRUE, expand = FALSE)
    }
  }, add = TRUE)
  # Use the same lock as applySnapshotReview(). A second migration may run its
  # tests in parallel, then retry publication if the other writer holds it.
  for (dir in dirs[dirs == lock | !dir.exists(dirs)]) {
    if (dir.create(dir, showWarnings = FALSE))
      created <- c(created, dir)
    else if (dir == lock)
      stop("Another snapshot review or migration holds the lock at ", lock, ". Retry after it finishes.", call. = FALSE)
    else if (!dir.exists(dir))
      stop("Could not create snapshot directory: ", dir, call. = FALSE)
  }
  commitSnapshotDecisions(root, changes, list(list(id = basename(path), paths = paths, hashes = expected)))
  invisible(NULL)
}
