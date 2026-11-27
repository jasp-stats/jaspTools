#' Browse table, figure and HTML snapshots
#'
#' Writes a standalone HTML review with the reference, candidate and details
#' of each change. Mark acceptances and add comments, then export the decisions
#' and use [applySnapshotReview()] to apply them to the same module.
#'
#' @param modulePath Module root containing `tests/testthat`.
#' @param output Where to write the HTML report. By default a file in the
#'   system temporary directory survives the R session that ran the tests.
#' @param open Open the report in the default browser.
#' @param includeUnchanged Include accepted snapshots that have no candidate.
#' @return The absolute path to the standalone HTML file, invisibly.
#' @export
reviewSnapshots <- function(modulePath = ".", output = NULL, open = interactive(), includeUnchanged = FALSE) {
  ids <- snapshotReviewIds(modulePath, includeUnchanged)
  writeSnapshotReview(modulePath, ids, output, open)
}

snapshotReviewRoot <- function(modulePath) {
  modulePath <- normalizePath(modulePath, winslash = "/", mustWork = TRUE)
  tests <- file.path(modulePath, "tests", "testthat")
  if (!dir.exists(tests))
    stop("No tests/testthat directory under ", modulePath, call. = FALSE)
  root <- file.path(tests, "_snaps")
  for (path in c(dirname(tests), tests, root)) {
    if (file.exists(path) && !identical(normalizePath(path, winslash = "/", mustWork = TRUE), path))
      stop("Redirected snapshot directory: ", path, call. = FALSE)
  }
  root
}

snapshotReviewIds <- function(modulePath, includeUnchanged = FALSE) {
  root <- snapshotReviewRoot(modulePath)
  files <- list.files(root, recursive = TRUE)
  files <- files[grepl("\\.(svg|html)$|\\.table(\\.new)?\\.json$", files)]
  if (!includeUnchanged)
    files <- files[grepl("\\.new\\.[^./]+$", files)]
  sort(unique(sub("\\.new(\\.[^./]+)$", "\\1", files)))
}

snapshotReviewPaths <- function(modulePath, id) {
  root <- snapshotReviewRoot(modulePath)
  base <- file.path(root, id)
  structure <- if (endsWith(id, ".svg")) {
    file.path(dirname(base), "reference_plotobject", sub("\\.svg$", ".rds", basename(base)))
  } else NA_character_
  c(baseline = base, candidate = snapshotCandidatePath(base),
    structureBaseline = structure, structureCandidate = snapshotCandidatePath(structure))
}

snapshotFileHash <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

snapshotReviewItem <- function(modulePath, id) {
  paths <- snapshotReviewPaths(modulePath, id)
  checkSnapshotDecisionPaths(snapshotReviewRoot(modulePath), paths)
  kind <- if (endsWith(id, ".table.json")) "table" else if (endsWith(id, ".svg")) "figure" else "html"
  readSide <- function(path) {
    if (!file.exists(path)) return(list(hash = NULL, value = NULL))
    bytes <- readBin(path, "raw", n = file.size(path))
    hash <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    if (kind == "figure") {
      value <- paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(bytes))
    } else {
      value <- rawToChar(bytes)
      Encoding(value) <- "UTF-8"
      if (kind == "table") value <- jsonlite::fromJSON(value, simplifyVector = FALSE)
    }
    list(hash = hash, value = value)
  }
  old <- readSide(paths[["baseline"]])
  new <- readSide(paths[["candidate"]])
  diff <- if (kind == "table" && !is.null(new$value)) snapshotTableDiff(old$value, new$value)
  list(id = id, kind = kind, baselineHash = old$hash, candidateHash = new$hash,
       structureBaselineHash = snapshotFileHash(paths[["structureBaseline"]]),
       structureCandidateHash = snapshotFileHash(paths[["structureCandidate"]]),
       old = old$value, new = new$value, diff = diff)
}

writeSnapshotReview <- function(modulePath, ids, output, open) {
  items <- lapply(ids, function(id) snapshotReviewItem(modulePath, id))
  data <- .snapshotJSON(list(version = 1L, items = items))
  # The HTML parser recognises closing script tags even inside application/json.
  data <- gsub("<", "\\u003c", as.character(data), fixed = TRUE)
  data <- paste0('<script type="application/json" id="review-data">', data, "</script>")
  assets <- system.file("snapshot-review", package = "jaspTools", mustWork = TRUE)
  template <- readLines(file.path(assets, "index.html"), encoding = "UTF-8", warn = FALSE)
  replacements <- list("/* SNAPSHOT_REVIEW_CSS */" = snapshotText(file.path(assets, "review.css")),
                       "/* SNAPSHOT_REVIEW_JS */" = snapshotText(file.path(assets, "review.js")),
                       "<!-- SNAPSHOT_REVIEW_DATA -->" = data)
  for (marker in names(replacements)) {
    line <- which(trimws(template) == marker)
    if (length(line) != 1L) stop("Missing report template marker ", marker, call. = FALSE)
    template[line] <- replacements[[marker]]
  }
  if (is.null(output))
    output <- tempfile("jasp-snapshot-review-", tmpdir = dirname(tempdir()), fileext = ".html")
  # A report written under _snaps would be discovered as another HTML reference.
  outDir <- normalizePath(dirname(output), winslash = "/", mustWork = TRUE)
  output <- file.path(outDir, basename(output))
  root <- snapshotReviewRoot(modulePath)
  if (startsWith(tolower(output), paste0(tolower(root), "/")))
    stop("Write the review outside the _snaps directory.", call. = FALSE)
  writeSnapshotText(paste(template, collapse = "\n"), output)
  if (open) utils::browseURL(output)
  invisible(output)
}
