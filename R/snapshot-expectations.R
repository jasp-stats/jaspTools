#' Compare a JASP result node with a reviewed snapshot
#'
#' Pass a complete table node to preserve column definitions, cell types,
#' footnotes and grouping. Tables store full-precision numbers. Numeric cell
#' comparisons use [local_table_snapshot_tolerance()] defaults unless overridden
#' for this table or column. When another cell changes, the review candidate
#' keeps the approved values for cells within tolerance. HTML snapshots store
#' the rendered element as UTF-8 markup. Both expectations use the active
#' testthat file's snapshot directory.
#' A missing reference or changed result fails the expectation and leaves a
#' candidate for [reviewSnapshots()]. A matching rerun clears the candidate.
#'
#' @param node A JASP result table, HTML element or report node, respectively.
#'   For HTML, a single string containing the markup is also accepted.
#' @param name A file stem unique within the test file and snapshot type.
#'   Use lower-case ASCII letters, digits, underscores and hyphens.
#' @inheritParams local_table_snapshot_tolerance
#' @param columns Named list of overrides keyed by schema field name. Each
#'   entry supplies `rtol`, `atol`, or both. For example,
#'   `list(estimate = list(atol = 1e-5), p = list(rtol = 0, atol = 0))`.
#' @param refresh Set to `TRUE` to stage every changed value in this table for
#'   review, including differences within tolerance. Accept the candidate
#'   with [applySnapshotReview()] to refresh the reference.
#' @return The testthat expectation, invisibly.
#' @export
expect_snapshot_table <- function(node, name, rtol = NULL, atol = NULL, columns = NULL,
                                  refresh = FALSE) {
  expectResultSnapshot(node, name, "table", rtol, atol, columns, refresh)
}

#' @rdname expect_snapshot_table
#' @export
expect_snapshot_html <- function(node, name) {
  expectResultSnapshot(node, name, "html")
}

#' Preserve reviewed files when an analysis cannot reach its expectations
#'
#' Call at the beginning of a test that runs an analysis. This registers its
#' test file's existing table, HTML, visual and plot structure snapshots with
#' testthat before the analysis can fail or skip. Generated tests include it
#' automatically. Remove obsolete references explicitly when retiring outputs.
#' @return Invisibly returns NULL.
#' @export
preserveSnapshotReferences <- function() {
  # File-level settings can fail before the reporter starts the first test.
  snapshotter <- getOption("testthat.snapshotter")
  if (!is.null(snapshotter$file)) {
    files <- list.files(file.path(snapshotter$snap_dir, snapshotter$file), recursive = TRUE)
    refs <- unique(sub("\\.new(\\.[^./]+)$", "\\1", files))
    refs <- refs[grepl("\\.(table\\.json|html|svg)$|^reference_plotobject/[^/]+\\.rds$", refs)]
    for (file in refs)
      snapshotter$announce_file_snapshot(file)
  }
  invisible(NULL)
}

snapshotHtml <- function(node) {
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  if (string(node))
    return(node)
  if (!is.list(node))
    stop("Expected a JASP HTML node or one markup string.", call. = FALSE)
  if (string(node[["html"]]))
    return(paste(c(node[["topHtml"]], node[["html"]]), collapse = "\n"))
  if (!string(node[["text"]]))
    stop("The JASP HTML node is missing its rendered text.", call. = FALSE)

  escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  attrs <- ""
  for (field in c("class", "maxWidth", "elementType")) {
    value <- node[[field]]
    if (string(value)) {
      attr <- switch(field, class = "class", maxWidth = "style", elementType = "data-element-type")
      if (field == "maxWidth") value <- paste0("max-width:", value)
      attrs <- paste0(attrs, " ", attr, '="', escape(value), '"')
    }
  }
  title <- node[["title"]]
  heading <- if (string(title) && nzchar(title) && title != "hide me")
    paste0("<h2>", escape(title), "</h2>")
  paste(c(paste0("<section", attrs, ">"), heading, node[["text"]], "</section>"), collapse = "\n")
}

snapshotFilename <- function(name, type) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^[a-z0-9][a-z0-9_-]*$", name) ||
      grepl("^(con|prn|aux|nul|com[1-9]|lpt[1-9])$", name))
    stop("Use a portable snapshot name containing lower-case letters, digits, hyphens or underscores.", call. = FALSE)
  paste0(name, switch(type, table = ".table.json", html = ".html", stop("Unknown snapshot type.")))
}

snapshotCandidatePath <- function(path) sub("(\\.[^./]+)$", ".new\\1", path)

snapshotText <- function(path) {
  bytes <- readBin(path, "raw", n = file.size(path))
  value <- rawToChar(bytes)
  Encoding(value) <- "UTF-8"
  gsub("\r\n?", "\n", value)
}

writeSnapshotText <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".snapshot-", tmpdir = dirname(path))
  on.exit(unlink(tmp))
  writeBin(charToRaw(enc2utf8(value)), tmp)
  if (!file.rename(tmp, path))
    stop("Could not write snapshot at ", path, call. = FALSE)
}

storeResultSnapshot <- function(node, name, type, snapshotDir, seed,
                                rtol = NULL, atol = NULL, columns = NULL, refresh = FALSE) {
  file <- snapshotFilename(name, type)
  reference <- file.path(snapshotDir, file)
  candidate <- snapshotCandidatePath(reference)
  value <- if (type == "table") snapshotTableJSON(node) else snapshotHtml(node)
  value <- gsub("\r\n?", "\n", enc2utf8(as.character(value)))
  if (!endsWith(value, "\n")) value <- paste0(value, "\n")
  old <- if (file.exists(reference)) snapshotText(reference)
  matches <- identical(old, value)
  if (type == "table") {
    parse <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)
    oldTable <- if (!is.null(old) && !matches) parse(old)
    # A field removed by this run can still have an override in the test.
    fields <- union(.snapshotFieldNames(node), .snapshotFieldNames(oldTable))
    tolerance <- snapshotTableTolerance(rtol, atol, columns, fields = fields)
    if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh))
      stop("refresh must be TRUE or FALSE.", call. = FALSE)
  }
  detail <- "New snapshot"
  if (!is.null(old)) {
    detail <- "Changed snapshot"
    if (!matches && type == "table") {
      diff <- snapshotTableDiff(oldTable, parse(value),
                                tolerance = if (!refresh) tolerance, merge = TRUE)
      matches <- !length(diff$changes)
      detail <- diff$summary
      numericChanges <- Filter(function(change) !is.null(change$tolerance), diff$changes)
      if (length(numericChanges)) {
        change <- numericChanges[[1L]]
        tol <- change$tolerance
        difference <- if (nzchar(change$delta)) paste("by", change$delta) else
          paste("from", change$old, "to", change$new)
        detail <- paste0(detail, ". ", change$rowLabel, ", ", change$column,
                         " changed ", difference, " (limit ", format(tol$limit),
                         ", rtol ", format(tol$rtol), ", atol ", format(tol$atol), ")")
      }
      if (!matches && !refresh)
        value <- paste0(snapshotTableJSON(diff$merged), "\n")
    }
    if (matches) {
      if (file.exists(candidate) && !file.remove(candidate))
        stop("Could not clear matching candidate ", candidate, call. = FALSE)
      return(NULL)
    }
  } else if (seed && !file.exists(candidate)) {
    writeSnapshotText(value, reference)
    return(NULL)
  }
  writeSnapshotText(value, candidate)
  paste0(detail, " (", file, "). Review with jaspTools::reviewSnapshots().")
}

stageGeneratedSnapshot <- function(node, name, type, snapshotDir) {
  pending <- storeResultSnapshot(node, name, type, snapshotDir, seed = TRUE)
  if (!is.null(pending)) message(pending)
  invisible(NULL)
}

expectResultSnapshot <- function(node, name, type, rtol = NULL, atol = NULL, columns = NULL,
                                 refresh = FALSE) {
  testthat::local_edition(3)
  snapshotter <- get_snapshotter()
  if (is.null(snapshotter))
    stop("Run this expectation in a test file with a testthat snapshot reporter.", call. = FALSE)
  file <- snapshotFilename(name, type)
  id <- file.path(snapshotter$file, file)
  # Older testthat reporters have no saved-file registry. Their current
  # FileSnaps object has the same lifetime (one test file).
  saved <- snapshotter$snap_file_saved
  if (is.null(saved))
    saved <- attr(snapshotter$cur_snaps, "jaspTools.savedFiles", exact = TRUE)
  if (id %in% saved)
    stop("Snapshot name used twice in this test file: ", file, call. = FALSE)
  if (is.null(snapshotter$snap_file_saved))
    attr(snapshotter$cur_snaps, "jaspTools.savedFiles") <- c(saved, id)
  else
    snapshotter$snap_file_saved <- c(saved, id)
  testthat::announce_snapshot_file(name = file)
  pending <- storeResultSnapshot(node, name, type, dirname(snapshot_relative_path(file)),
                                 seed = FALSE, rtol = rtol, atol = atol, columns = columns, refresh = refresh)
  invisible(testthat::expect(is.null(pending), if (is.null(pending)) file else pending))
}
