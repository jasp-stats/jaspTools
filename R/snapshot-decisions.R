#' Apply exported snapshot review decisions
#'
#' Checks the reviewed file contents in the module's `tests/testthat/_snaps`
#' directory before applying any decisions. Accepting promotes the candidate
#' and any reviewed plot structure candidate together. Declining records the
#' decision and comment for follow-up. A comment alone leaves the decision
#' unresolved. Both actions preserve the reference and candidate files.
#' Earlier exports that use `keep` also preserve those files.
#' An already applied acceptance can be replayed if its files still match.
#'
#' @param decisions Path to the JSON decisions file exported from the reviewer.
#' @param modulePath Path to the module containing the reviewed snapshots.
#' @return A data frame with one row per decision, including the snapshot
#'   `id`, requested `action`, reviewer `comment`, and resulting `status`
#'   (`accepted`, `already accepted`, `declined`, `unresolved`, or `kept`).
#' @export
applySnapshotReview <- function(decisions, modulePath = ".") {
  if (!is.character(decisions) || length(decisions) != 1L || is.na(decisions) ||
      !file.exists(decisions) || dir.exists(decisions))
    stop("Supply the path to an exported snapshot decisions file.", call. = FALSE)
  review <- jsonlite::read_json(normalizePath(decisions), simplifyVector = FALSE)
  if (!is.list(review) || !all(c("version", "decisions") %in% names(review)) ||
      anyDuplicated(names(review)) || !is.numeric(review$version) ||
      !identical(as.numeric(review$version), 1) || !is.list(review$decisions) ||
      !is.null(names(review$decisions)))
    stop("Expected a version 1 JSON object with a decisions array.", call. = FALSE)

  rows <- review$decisions
  hashFields <- c(baseline = "baselineHash", candidate = "candidateHash",
                  structureBaseline = "structureBaselineHash", structureCandidate = "structureCandidateHash")
  required <- c("id", "action", "comment", hashFields)
  statuses <- c(accept = "accepted", decline = "declined", comment = "unresolved", keep = "kept")
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  for (row in rows) {
    if (!all(required %in% names(row)) || anyDuplicated(names(row)) || !string(row$id) ||
        !string(row$action) || !row$action %in% names(statuses) || !string(row$comment))
      stop("Invalid snapshot decision. Supply an id, action, comment and four file hashes.", call. = FALSE)
    for (key in hashFields) {
      hash <- row[[key]]
      if (!is.null(hash) && !(string(hash) && grepl("^[0-9a-f]{64}$", hash)))
        stop("Invalid ", key, " for snapshot ", row$id, ".", call. = FALSE)
    }
    if (row$action == "accept" && is.null(row$candidateHash))
      stop("Cannot accept a snapshot without a reviewed candidate: ", row$id, call. = FALSE)
  }
  action <- vapply(rows, `[[`, "", "action")
  result <- data.frame(id = vapply(rows, `[[`, "", "id"), action = action,
                       comment = vapply(rows, `[[`, "", "comment"), status = unname(statuses[action]),
                       stringsAsFactors = FALSE)
  if (anyDuplicated(result$id))
    stop("A snapshot id occurs more than once in the decisions file.", call. = FALSE)
  if (!nrow(result))
    return(result)

  modulePath <- normalizePath(modulePath, winslash = "/", mustWork = TRUE)
  root <- snapshotReviewRoot(modulePath)
  if (any(result$action == "accept")) {
    lock <- file.path(root, ".jaspTools-review-lock")
    if (!dir.create(lock, showWarnings = FALSE))
      stop("Cannot acquire the snapshot review lock at ", lock, ".", call. = FALSE)
    on.exit({
      if (identical(normalizePath(lock, winslash = "/", mustWork = FALSE), lock) &&
          !length(list.files(lock, all.files = TRUE, no.. = TRUE)))
        unlink(lock, recursive = TRUE, expand = FALSE)
    }, add = TRUE)
  }
  known <- snapshotReviewIds(modulePath, includeUnchanged = TRUE)
  if (any(!result$id %in% known))
    stop("Unknown snapshot id: ", result$id[!result$id %in% known][1L], call. = FALSE)

  changes <- list()
  final <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    paths <- snapshotReviewPaths(modulePath, row$id)
    expected <- vapply(hashFields, function(key) {
      if (is.null(row[[key]])) NA_character_ else row[[key]]
    }, "")
    actual <- snapshotDecisionHashes(root, paths)
    accepted <- expected
    accepted["baseline"] <- expected["candidate"]
    if (!is.na(expected["structureCandidate"]))
      accepted["structureBaseline"] <- expected["structureCandidate"]
    accepted[c("candidate", "structureCandidate")] <- NA_character_
    final[[i]] <- list(id = row$id, paths = paths, hashes = if (row$action == "accept") accepted else expected)
    if (row$action == "accept" && identical(actual, accepted)) {
      result$status[i] <- "already accepted"
      next
    }
    if (!identical(actual, expected))
      stop("Snapshot files have changed since review: ", row$id, call. = FALSE)
    if (row$action == "accept") {
      for (pair in list(c("baseline", "candidate"), c("structureBaseline", "structureCandidate"))) {
        if (!is.na(expected[pair[2L]]))
          changes[[length(changes) + 1L]] <- list(paths = unname(paths[pair]), hashes = unname(expected[pair]))
      }
    }
  }
  if (length(changes))
    commitSnapshotDecisions(root, changes, final)
  result
}

# Check every component, including the parent of a yet-to-be-created reference.
checkSnapshotDecisionPaths <- function(root, paths) {
  for (path in paths[!is.na(paths)]) {
    if (!startsWith(path, paste0(root, "/")) || dir.exists(path))
      stop("Invalid snapshot file path: ", path, call. = FALSE)
    node <- path
    repeat {
      if (utils::file_test("-L", node) ||
          (file.exists(node) && normalizePath(node, winslash = "/", mustWork = TRUE) != node))
        stop("Linked snapshot path: ", node, call. = FALSE)
      if (node == root)
        break
      node <- dirname(node)
    }
  }
}

snapshotDecisionHashes <- function(root, paths) {
  checkSnapshotDecisionPaths(root, paths)
  vapply(paths, function(path) {
    hash <- if (!is.na(path)) snapshotFileHash(path)
    if (is.null(hash)) NA_character_ else hash
  }, "")
}

# Keep originals in their own directories until every replacement succeeds.
commitSnapshotDecisions <- function(root, changes, final) {
  temporary <- character()
  moves <- list()
  committed <- FALSE
  on.exit({
    recover <- character()
    if (!committed) {
      for (move in rev(moves)) {
        from <- move[1L]
        to <- move[2L]
        restored <- tryCatch({
          checkSnapshotDecisionPaths(root, move)
          !file.exists(from) && file.rename(to, from)
        }, error = function(e) FALSE)
        if (!restored)
          recover <- c(recover, to)
      }
    }
    # A failed rollback may leave the only copy of an original in a backup.
    for (path in setdiff(temporary, recover)) {
      tryCatch({
        checkSnapshotDecisionPaths(root, path)
        if (unlink(path, expand = FALSE) != 0L)
          warning("Could not remove temporary snapshot file ", path, ".", call. = FALSE)
      }, error = function(e) warning(conditionMessage(e), call. = FALSE))
    }
    if (length(recover))
      warning("Snapshot rollback requires recovery from: ", paste(recover, collapse = ", "), call. = FALSE)
  })

  temp <- function(path) {
    result <- file.path(dirname(path), basename(tempfile(".jaspTools-review-", tmpdir = dirname(path))))
    temporary <<- c(temporary, result)
    result
  }
  moveFile <- function(from, to, hash) {
    checkSnapshotDecisionPaths(root, c(from, to))
    if (file.exists(to) || !file.rename(from, to))
      stop("Cannot move snapshot file ", from, " to ", to, ".", call. = FALSE)
    moves[[length(moves) + 1L]] <<- c(from, to)
    if (!identical(unname(snapshotDecisionHashes(root, to)), hash))
      stop("Snapshot changed while applying decisions: ", from, call. = FALSE)
  }

  staged <- character(length(changes))
  for (i in seq_along(changes)) {
    change <- changes[[i]]
    checkSnapshotDecisionPaths(root, change$paths)
    staged[i] <- temp(change$paths[1L])
    if (!file.copy(change$paths[2L], staged[i]) ||
        !identical(unname(snapshotDecisionHashes(root, staged[i])), change$hashes[2L]))
      stop("Cannot stage the reviewed candidate ", change$paths[2L], ".", call. = FALSE)
  }
  for (i in seq_along(changes)) {
    change <- changes[[i]]
    if (!identical(unname(snapshotDecisionHashes(root, change$paths)), change$hashes))
      stop("Snapshot changed while applying decisions: ", change$paths[1L], call. = FALSE)
    if (!is.na(change$hashes[1L]))
      moveFile(change$paths[1L], temp(change$paths[1L]), change$hashes[1L])
    moveFile(change$paths[2L], temp(change$paths[2L]), change$hashes[2L])
    moveFile(staged[i], change$paths[1L], change$hashes[2L])
  }
  for (item in final) {
    if (!identical(snapshotDecisionHashes(root, item$paths), item$hashes))
      stop("Snapshot changed while applying decisions: ", item$id, call. = FALSE)
  }
  committed <- TRUE
}
