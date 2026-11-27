#' Migrate existing module table tests to reviewed snapshots
#'
#' Find legacy `expect_equal_tables()` calls whose input can be traced to a
#' complete JASP result table. By default this only reads test source. Use
#' `apply = TRUE` in a configured module testing environment to run the selected
#' files and write their new tests and full-precision references.
#'
#' Migration runs each selected file twice, each time in a fresh copy of the
#' module's `tests/`, `examples/` and `inst/` directories. The first run
#' checks the original inline expectations and captures their full result nodes.
#' The second runs the rewritten file against those references. Publication is
#' per file. Other files can migrate even when one fails. Existing figure and
#' HTML checks run as written. Inspect the returned statuses and review the
#' new references before committing changes in a module.
#'
#' @param modulePaths Character vector of module checkout directories. Defaults
#'   to the modules configured for testing in jaspTools.
#' @param files Optional character vector of exact test filenames, including
#'   the extension. A name selects the matching file in each supplied module.
#' @param apply Run the tests and write verified migrations. Defaults to FALSE.
#' @param allowCustomRounding Opt in to replacing custom legacy
#'   `jaspRoundToPrecision` rules with the snapshot tolerance policy. By default
#'   affected files require manual review. Choose the appropriate snapshot
#'   tolerances before opting in.
#' @return A data frame with module path, file, original line, snapshot name,
#'   status and reason. `ready` denotes a proposed conversion, `migrated` a
#'   verified conversion, and `manual` an unsupported comparison kept in the
#'   test. `blocked` explains why a file could not migrate. `unchanged` means
#'   the file has no legacy table checks.
#' @examples
#' \dontrun{
#' modules <- c("../jaspQualityControl", "../jaspDescriptives")
#' plan <- migrateSnapshotTests(modules)
#' table(plan$status)
#' result <- migrateSnapshotTests(modules, files = "test-msaGaugeLinearity.R", apply = TRUE)
#' subset(result, status %in% c("blocked", "manual"))
#' }
#' @export
migrateSnapshotTests <- function(modulePaths = getModulePathsForTesting(), files = NULL, apply = FALSE,
                                 allowCustomRounding = FALSE) {
  if (!is.logical(apply) || length(apply) != 1L || is.na(apply))
    stop("apply must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(allowCustomRounding) || length(allowCustomRounding) != 1L || is.na(allowCustomRounding))
    stop("allowCustomRounding must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(modulePaths) && (!is.character(modulePaths) || anyNA(modulePaths) || any(!nzchar(modulePaths))))
    stop("Supply module checkout paths in modulePaths.", call. = FALSE)
  if (!is.null(files) && (!is.character(files) || anyNA(files) ||
      any(!grepl("^test[-_].*[.][rR]$", files) | basename(files) != files)))
    stop("Supply exact test filenames such as test-analysis.R in files.", call. = FALSE)

  modules <- unique(normalizePath(as.character(modulePaths), winslash = "/", mustWork = FALSE))
  testFiles <- lapply(modules, function(module) tryCatch({
    root <- dirname(snapshotReviewRoot(module))
    if (file.access(root, 4L) != 0L) stop("Cannot read ", root, call. = FALSE)
    paths <- list.files(root, "^test[-_].*[.][rR]$", full.names = TRUE)
    paths[!dir.exists(paths) & (is.null(files) | basename(paths) %in% files)]
  }, error = identity))
  failed <- vapply(testFiles, inherits, logical(1), "error")
  missing <- setdiff(files, basename(as.character(unlist(testFiles[!failed], use.names = FALSE))))
  if (length(missing) && !any(failed))
    stop("Test file not found in the selected modules: ", paste(missing, collapse = ", "), call. = FALSE)

  result <- list()
  for (i in seq_along(modules)) {
    rows <- tryCatch({
      if (failed[i]) stop(testFiles[[i]])
      migrateModuleTestFiles(modules[i], testFiles[[i]], apply, allowCustomRounding)
    }, error = function(e) list(data.frame(module = modules[i], file = NA_character_,
      line = NA_integer_, name = NA_character_, status = "blocked", reason = conditionMessage(e))))
    result <- c(result, rows)
  }
  if (length(result)) {
    rows <- do.call(rbind, result)
    rownames(rows) <- NULL
    return(rows)
  }
  data.frame(module = character(), file = character(), line = integer(), name = character(),
             status = character(), reason = character())
}

migrateModuleTestFiles <- function(module, paths, apply, allowCustomRounding) {
  if (apply && length(paths) && .isSetupComplete()) {
    previous <- getPkgOption("module.dirs")
    on.exit(setPkgOption("module.dirs", previous), add = TRUE)
    setPkgOption("module.dirs", module)
  }
  issue <- function(status, reason) {
    data.frame(line = NA_integer_, name = NA_character_, status = status, reason = reason)
  }
  helpers <- list.files(file.path(module, "tests", "testthat"), "^(helper|setup).*\\.[rR]$", full.names = TRUE)
  rounders <- basename(Filter(function(file) {
    if (snapshotLinkedPaths(file) || file.access(file, 4L) != 0L) return(FALSE)
    any(grepl("jaspRoundToPrecision", readLines(file, warn = FALSE, encoding = "UTF-8"), fixed = TRUE))
  }, helpers))
  result <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    path <- paths[i]
    sites <- tryCatch({
      checkSnapshotDecisionPaths(dirname(path), path)
      if (apply) hash <- snapshotFileHash(path)
      plan <- snapshotMigrationSource(path)
      sites <- plan$sites
      ready <- sites$status == "ready"
      if (any(ready)) {
        policies <- c(rounders, if (any(grepl("jaspRoundToPrecision", plan$capture, fixed = TRUE))) basename(path))
        if (length(policies) || !is.null(getOption("jaspRoundToPrecision"))) {
          origin <- if (length(policies)) paste(policies, collapse = ", ") else "the active R options"
          sites$reason[ready] <- paste0("Legacy jaspRoundToPrecision in ", origin,
            ". Choose snapshot tolerances before using allowCustomRounding = TRUE.")
          if (!allowCustomRounding) {
            sites$status[ready] <- "manual"
            ready[] <- FALSE
          }
        }
      }
      if (apply && any(ready)) {
        message("Checking ", basename(module), "/", basename(path), " (", sum(ready), " table comparisons)")
        failure <- tryCatch({
          runSnapshotMigration(module, path, plan, hash, allowCustomRounding = allowCustomRounding)
          NULL
        }, error = conditionMessage)
        sites$status[ready] <- if (is.null(failure)) "migrated" else "blocked"
        if (!is.null(failure)) sites$reason[ready] <- failure
      }
      if (nrow(sites)) sites else issue("unchanged", "")
    }, error = function(e) issue("blocked", conditionMessage(e)))
    result[[i]] <- data.frame(module = module, file = basename(path), sites, stringsAsFactors = FALSE)
  }
  result
}

# Copy fixtures rather than linking them: a testthat run can rewrite snapshots.
copyMigrationTests <- function(module, scratch) {
  copyTree <- function(from, to) {
    if (!dir.create(to)) stop("Could not create ", to, call. = FALSE)
    paths <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    linked <- snapshotLinkedPaths(paths)
    if (any(linked))
      stop("Cannot isolate linked test fixture: ", paths[linked][1L], call. = FALSE)
    directories <- dir.exists(paths)
    if (!all(file.copy(paths[!directories], to, copy.mode = FALSE)))
      stop("Could not copy test fixtures from ", from, call. = FALSE)
    for (path in paths[directories])
      copyTree(path, file.path(to, basename(path)))
  }
  # Example-generated tests use test_path("..", "..", "examples", ...) and
  # module tests can read their QML or datasets from inst/.
  roots <- file.path(module, c("tests", "examples", "inst", "DESCRIPTION", "NAMESPACE"))
  roots <- roots[file.exists(roots)]
  if (any(snapshotLinkedPaths(roots)))
    stop("Cannot isolate linked module fixtures under ", module, call. = FALSE)
  for (path in roots[dir.exists(roots)])
    copyTree(path, file.path(scratch, basename(path)))
  if (!all(file.copy(roots[!dir.exists(roots)], scratch, copy.mode = FALSE)))
    stop("Could not copy package metadata for ", module, call. = FALSE)
  file.path(scratch, "tests", "testthat")
}
