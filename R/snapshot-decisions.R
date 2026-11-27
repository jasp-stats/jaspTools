#' Apply exported snapshot review decisions
#'
#' Checks the reviewed file contents in the module's `tests/testthat/_snaps`
#' directory before applying any decisions. Accepting promotes the candidate
#' and any reviewed plot structure candidate together. For a figure without
#' a new structure, accepting removes the old structure when the export marks
#' it for removal. Earlier exports keep it. Declining records the
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
      length(review$version) != 1L || !review$version %in% 1:2 || !is.list(review$decisions) ||
      !is.null(names(review$decisions)))
    stop("Expected a version 1 or 2 JSON object with a decisions array.", call. = FALSE)

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
    if ("removeStructure" %in% names(row)) {
      canRemove <- review$version == 2 && isTRUE(row$removeStructure) && endsWith(row$id, ".svg") &&
        !is.null(row$candidateHash) && !is.null(row$structureBaselineHash) && is.null(row$structureCandidateHash)
      if (!canRemove)
        stop("Invalid plot structure removal for snapshot ", row$id, ".", call. = FALSE)
    }
  }
  action <- vapply(rows, `[[`, "", "action")
  result <- data.frame(id = vapply(rows, `[[`, "", "id"), action = action,
                       comment = vapply(rows, `[[`, "", "comment"), status = unname(statuses[action]),
                       stringsAsFactors = FALSE)
  if (anyDuplicated(result$id))
    stop("A snapshot id occurs more than once in the decisions file.", call. = FALSE)
  if (!nrow(result))
    return(result)

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
    paths <- snapshotReviewPaths(root, row$id)
    expected <- vapply(hashFields, function(key) {
      if (is.null(row[[key]])) NA_character_ else row[[key]]
    }, "")
    actual <- snapshotDecisionHashes(root, paths)
    accepted <- expected
    pairs <- list(c("baseline", "candidate"))
    if (!is.na(expected["structureCandidate"]) || isTRUE(row[["removeStructure"]]))
      pairs[[2L]] <- c("structureBaseline", "structureCandidate")
    for (pair in pairs) {
      accepted[pair[1L]] <- expected[pair[2L]]
      accepted[pair[2L]] <- NA_character_
    }
    final[[i]] <- list(id = row$id, paths = paths, hashes = if (row$action == "accept") accepted else expected)
    if (row$action == "accept" && identical(actual, accepted)) {
      result$status[i] <- "already accepted"
      next
    }
    if (!identical(actual, expected))
      stop("Snapshot files have changed since review: ", row$id, call. = FALSE)
    if (row$action == "accept") {
      for (pair in pairs)
        changes[[length(changes) + 1L]] <- list(paths = unname(paths[pair]), hashes = unname(expected[pair]))
    }
  }
  if (length(changes))
    commitSnapshotDecisions(root, changes, final)
  result
}

# Check every component, including the parent of a yet-to-be-created reference.
checkSnapshotDecisionPaths <- function(root, paths) {
  paths <- paths[!is.na(paths)]
  invalid <- !startsWith(paths, paste0(root, "/")) | dir.exists(paths)
  if (any(invalid))
    stop("Invalid snapshot file path: ", paths[invalid][1L], call. = FALSE)
  while (length(paths)) {
    linked <- snapshotLinkedPaths(paths)
    if (any(linked))
      stop("Linked snapshot path: ", paths[linked][1L], call. = FALSE)
    paths <- unique(dirname(paths[paths != root]))
  }
}

snapshotDecisionHashes <- function(root, paths) {
  checkSnapshotDecisionPaths(root, paths)
  vapply(paths, function(path) {
    hash <- snapshotFileHash(path)
    if (is.null(hash)) NA_character_ else hash
  }, "")
}

# Keep originals in their own directories until every replacement succeeds.
commitSnapshotDecisions <- function(root, changes, final) {
  # Renaming a read-only file can succeed even when its backup cannot be removed.
  for (change in changes) {
    checkSnapshotDecisionPaths(root, change$paths)
    existing <- change$paths[!is.na(change$paths) & file.exists(change$paths)]
    readOnly <- which(bitwAnd(file.mode(existing), as.octmode("222")) == 0L | file.access(existing, 2L) != 0L)
    if (length(readOnly))
      stop("Cannot change read-only file ", existing[readOnly[1L]], ".", call. = FALSE)
  }

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

  staged <- rep(NA_character_, length(changes))
  for (i in seq_along(changes)) {
    change <- changes[[i]]
    if (is.na(change$hashes[2L])) next # Remove an obsolete plot structure.
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
    if (!is.na(staged[i])) {
      moveFile(change$paths[2L], temp(change$paths[2L]), change$hashes[2L])
      moveFile(staged[i], change$paths[1L], change$hashes[2L])
    }
  }
  for (item in final) {
    if (!identical(snapshotDecisionHashes(root, item$paths), item$hashes))
      stop("Snapshot changed while applying decisions: ", item$id, call. = FALSE)
  }
  committed <- TRUE
}
