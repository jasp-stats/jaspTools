# JASP tables already have a schema for the results viewer. Keep that shape so
# the snapshot carries the same column order, typed cells and footnote anchors.
snapshotTableJSON <- function(node) {
  if (!is.list(node) || !all(c("schema", "data") %in% names(node)))
    stop("A table snapshot requires a JASP table with schema and data.")

  # fromJSON(simplifyVector = FALSE) preserves arrays as unnamed lists. Also
  # accept atomic vectors for the array-valued fields when constructing in R.
  array <- function(x) if (is.null(x)) NULL else unname(as.list(x))
  for (key in intersect(c("data", "footnotes", "citation"), names(node)))
    node[key] <- list(array(node[[key]]))
  if ("fields" %in% names(node$schema)) {
    fields <- array(node$schema$fields)
    for (i in seq_along(fields)) {
      if (".footnotes" %in% names(fields[[i]]))
        fields[[i]][".footnotes"] <- list(array(fields[[i]][[".footnotes"]]))
    }
    node$schema["fields"] <- list(fields)
  }
  for (i in seq_along(node$data)) {
    notes <- node$data[[i]][[".footnotes"]]
    if (is.list(notes))
      node$data[[i]][".footnotes"] <- list(lapply(notes, array))
  }
  for (i in seq_along(node$footnotes)) {
    for (key in intersect(c("rows", "cols"), names(node$footnotes[[i]])))
      node$footnotes[[i]][key] <- list(array(node$footnotes[[i]][[key]]))
  }
  .snapshotJSON(node, pretty = TRUE)
}

# jsonlite's digits = NA still rounds some doubles. Supply a verbatim JSON
# number with enough significant digits to read back the same binary value.
.snapshotNumber <- function(x) {
  x <- as.double(x)
  number <- sprintf("%.15g", x)
  # JSON parsers and R's as.double() can disagree at a rounding boundary.
  # Check shorter candidates with the same parser that reads the snapshots.
  for (digits in 16:17) {
    parsed <- jsonlite::parse_json(paste0("[", paste(number, collapse = ","), "]"),
      simplifyVector = TRUE)
    retry <- parsed != x
    if (!any(retry)) break
    number[retry] <- sprintf(paste0("%.", digits, "g"), x[retry])
  }
  number[number == "-0"] <- "-0.0"
  number
}

.snapshotJSON <- function(value, pretty = FALSE) {
  orderMembers <- function(x) {
    if (is.list(x)) {
      if (!is.null(names(x)))
        x <- x[order(enc2utf8(names(x)), method = "radix")]
      return(lapply(x, orderMembers))
    }
    x
  }
  # Batch precision checks across the table. Both traversals visit numeric
  # leaves in the same order, including vectors in hand-constructed inputs.
  classes <- c("numeric", "integer")
  values <- rapply(list(value), identity, classes = classes, how = "unlist")
  if (length(values)) {
    if (any(!is.finite(values)))
      stop("Non-finite R numbers cannot be stored as JSON numbers. Use the JASP JSON value.")
    numbers <- unname(.snapshotNumber(values))
    offset <- 0L
    value <- rapply(list(value), function(x) {
      tokens <- numbers[offset + seq_along(x)]
      offset <<- offset + length(x)
      if (length(tokens) == 1L) return(structure(tokens, class = "json"))
      lapply(tokens, function(s) structure(s, class = "json"))
    }, classes = classes, how = "replace")[[1L]]
  }
  enc2utf8(as.character(jsonlite::toJSON(orderMembers(value), auto_unbox = TRUE,
    null = "null", na = "null", pretty = pretty, json_verbatim = TRUE)))
}

.snapshotFieldNames <- function(table) {
  vapply(table$schema$fields, `[[`, character(1), "name", USE.NAMES = FALSE)
}

.snapshotAbsent <- structure(list(), class = "jasp_snapshot_absent")

.snapshotMember <- function(object, key) {
  present <- if (is.numeric(key)) key <= length(object) else key %in% names(object)
  if (present) object[[key]] else .snapshotAbsent
}

.snapshotDisplay <- function(value) {
  if (inherits(value, "jasp_snapshot_absent")) "<absent>" else .snapshotJSON(value)
}

# Locate the first different digit in the displayed decimal expansions (a
# subtraction alone gets this wrong when the change crosses a power of ten).
.snapshotDecimalPlace <- function(x, y) {
  digits <- function(value) {
    parts <- strsplit(.snapshotNumber(abs(value)), "e", fixed = TRUE)[[1L]]
    exponent <- if (length(parts) == 2L) as.integer(parts[2L]) else 0L
    point <- nchar(strsplit(parts[1L], ".", fixed = TRUE)[[1L]][1L])
    chars <- strsplit(gsub(".", "", parts[1L], fixed = TRUE), "", fixed = TRUE)[[1L]]
    stats::setNames(chars, seq_along(chars) - point - exponent)
  }
  a <- digits(x)
  b <- digits(y)
  places <- sort(unique(as.integer(c(names(a), names(b)))))
  left <- a[as.character(places)]
  right <- b[as.character(places)]
  left[is.na(left)] <- "0"
  right[is.na(right)] <- "0"
  differ <- if (sign(x) * sign(y) < 0) left != "0" | right != "0" else left != right
  if (any(differ)) places[which(differ)[1L]] else NULL
}

# Find a common unique row label (or a composite of label columns). Anonymous
# rows and tables with incompatible labels retain their positional comparison.
.snapshotAlignedRows <- function(old, new) {
  a <- old$data
  b <- new$data
  ids <- intersect(.snapshotFieldNames(old), .snapshotFieldNames(new))
  labels <- function(rows, id) {
    vapply(rows, function(row) {
      x <- row[[id]]
      if (is.character(x) && length(x) == 1L && !is.na(x)) x else ""
    }, character(1))
  }
  pairs <- lapply(ids, function(id) list(a = labels(a, id), b = labels(b, id)))
  pairs <- Filter(function(p) length(p$a) && length(p$b) &&
    all(nzchar(p$a)) && all(nzchar(p$b)), pairs)
  best <- 0L
  keyA <- seq_along(a)
  keyB <- seq_along(b)
  labelA <- if (length(a)) paste("Row", seq_along(a)) else character()
  labelB <- if (length(b)) paste("Row", seq_along(b)) else character()
  partsA <- partsB <- NULL
  for (p in pairs) {
    # Length prefixes make a composite unambiguous even for labels containing
    # punctuation or the separator used for the human-readable row label.
    compositeA <- paste0(partsA, nchar(p$a), ":", p$a)
    compositeB <- paste0(partsB, nchar(p$b), ":", p$b)
    joinedA <- if (is.null(partsA)) p$a else paste(joinedA, p$a, sep = " / ")
    joinedB <- if (is.null(partsB)) p$b else paste(joinedB, p$b, sep = " / ")
    partsA <- compositeA
    partsB <- compositeB
    for (key in list(list(p$a, p$b, p$a, p$b), list(partsA, partsB, joinedA, joinedB))) {
      if (anyDuplicated(key[[1L]]) || anyDuplicated(key[[2L]])) next
      score <- sum(key[[1L]] %in% key[[2L]])
      if (score > best) {
        best <- score
        keyA <- key[[1L]]
        keyB <- key[[2L]]
        labelA <- key[[3L]]
        labelB <- key[[4L]]
      }
    }
    if (best == min(length(a), length(b))) break
  }
  oldIndex <- match(keyB, keyA)
  newIndex <- match(keyA, keyB)
  # Retain the candidate's order; insert a removed row before its next
  # surviving old neighbor, or at the end if there is no such neighbor.
  anchor <- length(b) + 1L
  for (i in rev(seq_along(newIndex))) {
    if (is.na(newIndex[i])) newIndex[i] <- anchor else anchor <- newIndex[i]
  }
  removed <- which(!keyA %in% keyB)
  alignment <- list()
  for (j in seq_len(length(b) + 1L)) {
    for (i in removed[newIndex[removed] == j])
      alignment[[length(alignment) + 1L]] <- list(oldRow = i, newRow = NULL, rowLabel = labelA[i])
    if (j <= length(b))
      alignment[[length(alignment) + 1L]] <- list(
        oldRow = if (!is.na(oldIndex[j])) oldIndex[j], newRow = j, rowLabel = labelB[j])
  }
  alignment
}

snapshotTableDiff <- function(old, new, tolerance = NULL, merge = FALSE) {
  changes <- list()
  rows <- .snapshotAlignedRows(old, new)
  record <- function(kind, oldValue, newValue, column = "", row = NULL, path = "", allowance = NULL) {
    delta <- ""
    place <- NULL
    if (is.numeric(oldValue) && length(oldValue) == 1L &&
        is.numeric(newValue) && length(newValue) == 1L && oldValue != newValue) {
      difference <- as.double(newValue) - as.double(oldValue)
      delta <- if (is.finite(difference)) sprintf("%+.7g", difference) else
        paste0(sprintf("%+.7g", newValue / 1e308 - oldValue / 1e308), "e+308")
      place <- .snapshotDecimalPlace(oldValue, newValue)
    }
    change <- list(
      kind = kind, column = column, oldRow = row$oldRow, newRow = row$newRow,
      rowLabel = if (is.null(row)) "" else row$rowLabel, path = path,
      old = .snapshotDisplay(oldValue), new = .snapshotDisplay(newValue),
      delta = delta, decimalPlace = place)
    if (!is.null(allowance)) change$tolerance <- allowance
    changes[[length(changes) + 1L]] <<- change
  }
  compare <- function(a, b, kind, column = "", row = NULL, path = "", precision = NULL) {
    if (identical(a, b, num.eq = FALSE)) return(b)
    allowance <- NULL
    if (is.numeric(a) && is.numeric(b) && length(a) == 1L && length(b) == 1L) {
      if (identical(as.double(a), as.double(b), num.eq = FALSE)) return(b)
      if (!is.null(precision) && precision$type != "mixed") {
        result <- .snapshotNumericComparison(as.double(a), as.double(b), precision)
        if (result$within) return(a)
        allowance <- result$tolerance
      }
    }
    if (is.list(a) && is.list(b) && !inherits(a, "jasp_snapshot_absent") &&
        !inherits(b, "jasp_snapshot_absent") && is.null(names(a)) == is.null(names(b))) {
      keys <- if (is.null(names(a))) seq_len(max(length(a), length(b))) else union(names(a), names(b))
      for (key in keys) {
        child <- if (is.numeric(key)) paste0(path, "[", key, "]") else if (nzchar(path)) paste0(path, ".", key) else key
        childPrecision <- .snapshotChildPrecision(precision, a, b, key)
        value <- compare(.snapshotMember(a, key), .snapshotMember(b, key), kind, column, row, child, childPrecision)
        if (merge && !is.null(childPrecision) && !inherits(value, "jasp_snapshot_absent"))
          b[key] <- list(value)
      }
    } else {
      record(kind, a, b, column, row, path, allowance)
    }
    b
  }
  if (is.null(old) || is.null(new)) {
    if (!identical(old, new))
      record(if (is.null(old)) "table_added" else "table_removed",
        if (is.null(old)) .snapshotAbsent else old$title,
        if (is.null(new)) .snapshotAbsent else new$title)
  } else {
    # Keep empty arrays, empty objects, null and absent members distinct even
    # when there are no columns or rows to align and inspect.
    shape <- function(x) if (is.list(x) && !inherits(x, "jasp_snapshot_absent")) x[0L] else x
    for (slot in c("schema", "data"))
      compare(shape(.snapshotMember(old, slot)), shape(.snapshotMember(new, slot)),
        "table_metadata", path = slot)
    compare(shape(.snapshotMember(old$schema, "fields")), shape(.snapshotMember(new$schema, "fields")),
      "table_metadata", path = "schema.fields")
    oldCols <- .snapshotFieldNames(old)
    newCols <- .snapshotFieldNames(new)
    common <- intersect(oldCols, newCols)
    precisions <- list()
    for (id in union(oldCols, newCols)) {
      oldFields <- old$schema$fields[oldCols == id]
      newFields <- new$schema$fields[newCols == id]
      if (!is.null(tolerance) && length(oldFields) == 1L && length(newFields) == 1L)
        precisions[[id]] <- .snapshotColumnPrecision(tolerance, id, oldFields[[1L]], newFields[[1L]])
      for (i in seq_len(max(length(oldFields), length(newFields)))) {
        if (i > length(oldFields))
          record("column_added", .snapshotAbsent, newFields[[i]], id)
        else if (i > length(newFields))
          record("column_removed", oldFields[[i]], .snapshotAbsent, id)
        else
          compare(oldFields[[i]], newFields[[i]], "column_metadata", id)
      }
    }
    if (!identical(oldCols[oldCols %in% common], newCols[newCols %in% common]))
      record("column_order", as.list(oldCols), as.list(newCols))
    for (slot in setdiff(union(names(old), names(new)), c("data", "schema")))
      compare(.snapshotMember(old, slot), .snapshotMember(new, slot), "table_metadata", path = slot)
    for (slot in setdiff(union(names(old$schema), names(new$schema)), "fields"))
      compare(.snapshotMember(old$schema, slot), .snapshotMember(new$schema, slot),
        "table_metadata", path = paste0("schema.", slot))
    matched <- Filter(function(r) !is.null(r$oldRow) && !is.null(r$newRow), rows)
    if (is.unsorted(vapply(matched, `[[`, integer(1), "oldRow"))) {
      keys <- vapply(matched, `[[`, character(1), "rowLabel")
      record("row_order", as.list(keys[order(vapply(matched, `[[`, integer(1), "oldRow"))]), as.list(keys))
    }
    for (row in rows) {
      i <- row$oldRow
      j <- row$newRow
      if (is.null(i)) {
        record("row_added", .snapshotAbsent, new$data[[j]], row = row)
      } else if (is.null(j)) {
        record("row_removed", old$data[[i]], .snapshotAbsent, row = row)
      } else {
        a <- old$data[[i]]
        b <- new$data[[j]]
        if (is.null(names(a)) || is.null(names(b))) {
          compare(a, b, "row_metadata", row = row)
          next
        }
        for (id in common) {
          value <- compare(.snapshotMember(a, id), .snapshotMember(b, id), "cell", id, row,
                           precision = precisions[[id]])
          if (merge && !is.null(precisions[[id]]) && !inherits(value, "jasp_snapshot_absent"))
            b[id] <- list(value)
        }
        if (merge) new$data[[j]] <- b
        oldNotes <- .snapshotMember(a, ".footnotes")
        newNotes <- .snapshotMember(b, ".footnotes")
        anchored <- union(names(oldNotes), names(newNotes))
        if (length(anchored)) {
          for (id in anchored)
            compare(.snapshotMember(oldNotes, id), .snapshotMember(newNotes, id),
              "cell", id, row, path = ".footnotes")
        } else {
          compare(oldNotes, newNotes, "row_metadata", row = row, path = ".footnotes")
        }
        for (id in setdiff(union(names(a), names(b)), c(oldCols, newCols, ".footnotes")))
          compare(.snapshotMember(a, id), .snapshotMember(b, id), "row_metadata", row = row, path = id)
      }
    }
  }
  kinds <- vapply(changes, `[[`, character(1), "kind")
  counts <- table(kinds)
  summary <- if (!length(changes)) "No changes" else
    paste(paste(gsub("_", " ", names(counts), fixed = TRUE), as.integer(counts), sep = ": "), collapse = ", ")
  diff <- list(summary = summary, changes = changes, rows = rows)
  if (merge) diff["merged"] <- list(new)
  diff
}
