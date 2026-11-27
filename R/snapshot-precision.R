#' Set numeric table snapshot tolerances for a test file
#'
#' Call at the top of a test file to set the defaults for its
#' [expect_snapshot_table()] calls, or inside a test to scope them to that test.
#' Each comparison uses the larger of the absolute allowance and the relative
#' allowance times the larger magnitude of the two values. Declared p-values
#' use an absolute allowance of zero unless the column has an explicit
#' override. Set both tolerances to zero to require exact numeric matches.
#'
#' Fields with a missing or unrecognized schema type require an exact match,
#' even with a column override. In a `mixed` field, the cell must declare
#' `type = "number"` or `type = "pvalue"` for its `value` to use a tolerance.
#' A bare numeric value in a mixed field always compares exactly.
#'
#' @param rtol Nonnegative finite relative tolerance. The package default is
#'   `1e-8`. `NULL` inherits the current setting.
#' @param atol Nonnegative finite absolute tolerance. The package default is
#'   `1e-12`. `NULL` inherits the current setting.
#' @param .local_envir Environment controlling the lifetime of the settings.
#' @return Invisibly returns `NULL`.
#' @export
local_table_snapshot_tolerance <- function(rtol = NULL, atol = NULL, .local_envir = parent.frame()) {
  policy <- snapshotTableTolerance(rtol, atol)
  withr::local_options(list(jaspTools.tableSnapshot.tolerance = policy),
                       .local_envir = .local_envir)
  invisible(NULL)
}

snapshotTableTolerance <- function(rtol = NULL, atol = NULL, columns = NULL, fields = NULL) {
  valid <- function(value, label) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0)
      stop(label, " must be a single finite nonnegative number.", call. = FALSE)
    as.double(value)
  }
  defaults <- getOption("jaspTools.tableSnapshot.tolerance", list(rtol = 1e-8, atol = 1e-12))
  rtol <- valid(if (is.null(rtol)) defaults$rtol else rtol, "rtol")
  atol <- valid(if (is.null(atol)) defaults$atol else atol, "atol")
  if (!is.null(columns)) {
    ids <- names(columns)
    if (!is.list(columns) || length(ids) != length(columns) || anyNA(ids) ||
        any(!nzchar(ids)) || anyDuplicated(ids))
      stop("columns must be a list with unique, nonempty column names.", call. = FALSE)
    unknown <- setdiff(ids, fields)
    if (!is.null(fields) && length(unknown))
      stop("Unknown snapshot column: ", paste(unknown, collapse = ", "), call. = FALSE)
    for (id in ids) {
      spec <- columns[[id]]
      keys <- names(spec)
      if (!(is.list(spec) || is.numeric(spec)) || !length(keys) || length(keys) != length(spec) ||
          !all(keys %in% c("rtol", "atol")) || anyDuplicated(keys))
        stop("Specify rtol, atol or both for snapshot column ", id, ".", call. = FALSE)
      columns[[id]] <- lapply(keys, function(key) valid(spec[[key]], paste(id, key)))
      names(columns[[id]]) <- keys
    }
  }
  list(rtol = rtol, atol = atol, columns = columns)
}

.snapshotColumnPrecision <- function(policy, id, oldField, newField) {
  type <- oldField[["type"]]
  if (!identical(type, newField[["type"]]) || length(type) != 1L ||
      !type %in% c("number", "pvalue", "mixed"))
    return(NULL)
  column <- policy$columns[[id]]
  rtol <- if (is.null(column$rtol)) policy$rtol else column$rtol
  atol <- if (is.null(column$atol)) policy$atol else column$atol
  pvalueAtol <- if (is.null(column$atol)) 0 else column$atol
  list(type = type, rtol = rtol, atol = if (type == "pvalue") pvalueAtol else atol,
       pvalueAtol = pvalueAtol)
}

# Array members inherit the type of their enclosing value. In typed JASP
# objects only the 'value' member contains results; all siblings are metadata.
.snapshotChildPrecision <- function(precision, a, b, key) {
  if (is.null(precision) || is.numeric(key)) return(precision)
  if (key != "value" || !identical(a[["type"]], b[["type"]])) return(NULL)
  type <- a[["type"]]
  if (is.null(type)) return(precision)
  if (length(type) != 1L || !type %in% c("number", "pvalue", "mixed")) return(NULL)
  precision$type <- type
  if (type == "pvalue") precision$atol <- precision$pvalueAtol
  precision
}

.snapshotNumericComparison <- function(a, b, precision) {
  scale <- max(abs(a), abs(b))
  limit <- max(precision$atol, precision$rtol * scale)
  difference <- abs(b - a)
  # A pair of opposite-sign finite doubles can overflow on subtraction.
  # When both sides overflow, evaluate the same inequality in units of scale.
  within <- if (is.infinite(difference) && is.infinite(limit))
    abs(b / scale - a / scale) <= max(precision$atol / scale, precision$rtol)
  else difference <= limit
  # At zero tolerance preserve the exact snapshot's distinction between -0 and 0.
  if (precision$atol == 0 && precision$rtol == 0) within <- FALSE
  if (!is.finite(limit)) limit <- sprintf("%.7g * %.7g", precision$rtol, scale)
  list(within = within, tolerance = list(rtol = precision$rtol, atol = precision$atol, limit = limit))
}
