# Work from the R parse tree, but edit only the affected source ranges. In
# particular, never deparse a whole test file or evaluate it while planning.
snapshotMigrationSource <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # The runtime restores the original BOM when writing either version.
  if (length(lines) && startsWith(lines[1L], "\uFEFF")) lines[1L] <- substring(lines[1L], 2L)
  sites <- data.frame(line = integer(), name = character(), status = character(), reason = character())
  unchanged <- list(text = lines, capture = lines, sites = sites)
  if (!any(grepl("expect_equal_tables", lines, fixed = TRUE))) return(unchanged)
  parsed <- parse(text = lines, keep.source = TRUE, encoding = "UTF-8")
  p <- utils::getParseData(parsed)
  targets <- which(p$token == "SYMBOL_FUNCTION_CALL" & p$text == "expect_equal_tables")
  if (!length(targets)) return(unchanged)

  parents <- match(p$parent, p$id, nomatch = 0L)
  groups <- split(seq_len(nrow(p)), factor(parents, levels = 0:nrow(p)))
  children <- function(i) groups[[i + 1L]]
  exprs <- function(i) {
    rows <- children(i)
    rows[p$token[rows] == "expr"]
  }
  # R's parser expands tabs to eight-column tab stops in source positions.
  source <- paste(lines, collapse = "\n")
  offsets <- c(0, cumsum(nchar(lines) + 1))
  position <- function(line, col) {
    s <- lines[[line]]
    if (grepl("\t", s, fixed = TRUE)) {
      chars <- strsplit(s, "", fixed = TRUE)[[1L]]
      ends <- Reduce(function(pos, ch) pos + if (ch == "\t") 8L - pos %% 8L else 1L,
                     chars, init = 0L, accumulate = TRUE)
      col <- findInterval(col - 1L, ends)
    }
    offsets[[line]] + col
  }
  start <- function(i) position(p$line1[i], p$col1[i])
  end <- function(i) position(p$line2[i], p$col2[i])
  piece <- function(i) substr(source, start(i), end(i))
  value <- function(i) parse(text = piece(i), keep.source = FALSE, encoding = "UTF-8")[[1L]]
  # Return the parse rows of arguments matched by R, including named and
  # reordered arguments. Using row identifiers also handles repeated values.
  arguments <- function(i, definition) {
    bits <- children(i)
    args <- exprs(i)[-1L]
    if (!length(args) || sum(p$token[bits] == "','") != length(args) - 1L) return(NULL)
    idx <- match(args, bits)
    named <- p$token[bits[idx - 1L]] == "EQ_SUB"
    labels <- character(length(args))
    labels[named] <- vapply(bits[idx[named] - 2L], function(key) as.character(value(key)), "")
    call <- as.call(c(list(as.name(".f")), stats::setNames(as.list(args), labels)))
    tryCatch(match.call(definition, call), error = function(e) NULL)
  }
  symbol <- function(x, name) identical(x, as.name(name))
  isFunction <- function(x, name, pkg) {
    symbol(x, name) || (is.call(x) && length(x) == 3L &&
      as.character(x[[1L]])[1L] %in% c("::", ":::") &&
      symbol(x[[2L]], pkg) && symbol(x[[3L]], name))
  }
  # Only a fixed baseline may become a snapshot. Leave comparisons against
  # another computation, a subset or transformed rows in their original form.
  literal <- function(x) {
    if (is.null(x) || is.atomic(x)) return(TRUE)
    if (!is.call(x) || !is.symbol(x[[1L]])) return(FALSE)
    op <- as.character(x[[1L]])
    if (op %in% c("+", "-")) return(length(x) == 2L && is.numeric(x[[2L]]))
    op %in% c("list", "c") && all(vapply(as.list(x)[-1L], literal, logical(1)))
  }
  dataSelector <- function(x) {
    is.call(x) && length(x) == 3L &&
      ((symbol(x[[1L]], "$") && symbol(x[[3L]], "data")) ||
       (symbol(x[[1L]], "[[") && identical(x[[3L]], "data")))
  }
  # Limit alias folding to ordinary option/data setup. An unknown helper,
  # delayed expression or lookup by name might observe the old rows later.
  ordinarySetup <- function(x, alias, generated = FALSE) {
    if (is.symbol(x)) return(!identical(x, alias))
    if (!is.call(x)) return(TRUE)
    fun <- x[[1L]]
    if (generated && (symbol(fun, "<-") || symbol(fun, "="))) {
      # Generated setup writes these locals. Rebinding a function (e.g. to
      # get()) could make an otherwise ordinary call read table indirectly.
      locals <- c("jaspFile", "opts", "dataset", "results", "plotName", "testPlot")
      if (length(x) != 3L || !is.symbol(x[[2L]]) || !as.character(x[[2L]]) %in% locals) return(FALSE)
    }
    if (generated && (identical(fun, quote(testthat::test_path)) ||
                      identical(fun, quote(jaspSyntax::readDatasetFromJaspFile)) ||
                      identical(fun, quote(jaspTools::expect_equal_plots))))
      return(all(vapply(as.list(x)[-1L], ordinarySetup, logical(1), alias = alias, generated = TRUE)))
    if (is.call(fun) && length(fun) == 3L && symbol(fun[[1L]], "::") &&
        symbol(fun[[2L]], "jaspTools")) fun <- fun[[3L]]
    if (!is.symbol(fun)) return(FALSE)
    name <- as.character(fun)
    allowed <- c("<-", "=", "$", "[[", "(", "+", "-", "*", "/", ":", "c", "list", "data.frame",
                 "set.seed", "analysisOptions", "runAnalysis", "runAnalysisFromJaspFile")
    if (!name %in% allowed && !grepl("^init[A-Za-z0-9]*Options$", name)) return(FALSE)
    all(vapply(as.list(x)[-1L], ordinarySetup, logical(1), alias = alias, generated = generated))
  }
  firstToken <- function(i) p$token[children(i)[1L]]
  # A test_that body (and its if-branches) executes a site at most once.
  # Functions, loops, quoted expressions and custom wrappers require review.
  testContext <- function(i) {
    parent <- parents[i]
    while (parent && firstToken(parent) %in% c("'{'", "IF")) {
      if (firstToken(parent) == "IF" && i == exprs(parent)[1L]) return(NULL)
      i <- parent
      parent <- parents[parent]
    }
    if (!parent || parents[parent] || firstToken(parent) != "expr") return(NULL)
    fun <- exprs(parent)[1L]
    if (!isFunction(value(fun), "test_that", "testthat")) return(NULL)
    args <- arguments(parent, function(desc, code) NULL)
    if (!identical(args$code, i) || is.null(args$desc)) return(NULL)
    desc <- value(args$desc)
    if (is.character(desc) && length(desc) == 1L) return(list(desc = desc, body = i))
    NULL
  }
  # Use a local node binding when the original rows were assigned. Retaining
  # the row binding preserves other expectations that inspect it later. Bind
  # before extracting data so a custom producer is evaluated exactly once.
  nodeVar <- "snapshotTableNode"
  while (any(grepl(nodeVar, lines, fixed = TRUE))) nodeVar <- paste0(nodeVar, "_")
  targetStarts <- sort(vapply(targets, start, numeric(1)))
  generated <- any(grepl("This test file was auto-generated from a JASP example file.", lines, fixed = TRUE))
  # In an unmodified generated test, each write to table supplies exactly
  # one expectation. Check the whole body, including setup and plot checks,
  # so a custom call cannot register a deferred read or look up table by name.
  generatedTableBody <- function(test) {
    if (!is.call(test) || length(test) != 3L || !isFunction(test[[1L]], "test_that", "testthat") ||
        !is.call(test[[3L]]) || !symbol(test[[3L]][[1L]], "{")) return(FALSE)
    pendingTable <- FALSE
    for (stmt in as.list(test[[3L]])[-1L]) {
      if (!is.call(stmt)) return(FALSE)
      if (pendingTable) {
        if (!identical(stmt[[1L]], quote(jaspTools::expect_equal_tables)) ||
            length(stmt) != 3L || !is.null(names(stmt)) || !symbol(stmt[[2L]], "table") ||
            !literal(stmt[[3L]])) return(FALSE)
        pendingTable <- FALSE
      } else if (length(stmt) == 3L && symbol(stmt[[1L]], "<-") && symbol(stmt[[2L]], "table")) {
        if (!dataSelector(stmt[[3L]]) || !ordinarySetup(stmt[[3L]], quote(table), generated = TRUE)) return(FALSE)
        pendingTable <- TRUE
      } else if (!ordinarySetup(stmt, quote(table), generated = TRUE)) return(FALSE)
    }
    !pendingTable
  }
  generatedBodies <- if (generated) {
    tests <- exprs(0L)[vapply(parsed, generatedTableBody, logical(1))]
    vapply(tests, function(j) exprs(j)[3L], integer(1))
  }
  # Test files often identify the analysis in context() and describe each
  # output or scenario in test_that(). Use only literal source labels.
  fileTitle <- sub("^test[-_]", "", tools::file_path_sans_ext(basename(path)))
  fileTitle <- sub("^(other|verified|library)-", "", fileTitle)
  headers <- list()
  for (j in which(p$token == "SYMBOL_FUNCTION_CALL" & p$text == "context")) {
    call <- parents[parents[j]]
    if (!parents[call] && isFunction(value(parents[j]), "context", "testthat")) {
      args <- exprs(call)
      if (length(args) == 2L && is.character(title <- value(args[2L])))
        headers[[as.character(p$line1[call])]] <- sub("\\s+--\\s+.*$", "", title)
    }
  }
  used <- character()
  current <- which(p$token == "SYMBOL_FUNCTION_CALL" & p$text == "expect_snapshot_table")
  for (i in current) {
    args <- arguments(parents[parents[i]], expect_snapshot_table)
    if (!is.null(args$name)) {
      name <- value(args$name)
      if (is.character(name)) used <- c(used, name)
    }
  }
  captureEdits <- finalEdits <- list()
  edit <- function(i, text) list(from = start(i), to = end(i), text = text)
  comments <- vapply(which(p$token == "COMMENT"), start, numeric(1))
  containsComment <- function(from, to) any(comments >= from & comments <= to)
  indentation <- function(i) sub("^([ \t]*).*", "\\1", lines[[p$line1[i]]])
  plan <- function(i) {
    manual <- function(reason) list(status = "manual", name = NA_character_, reason = reason)
    context <- testContext(i)
    if (is.null(context)) return(manual("Requires a standalone comparison in a single test_that() case."))
    args <- arguments(i, function(test, ref, label = NULL) NULL)
    if (is.null(args$test) || is.null(args$ref))
      return(manual("Could not identify the legacy test and reference arguments."))
    if (!is.null(args$label) && !literal(value(args$label)))
      return(manual("The legacy comparison has a computed label."))
    # Preserve comments embedded in the legacy reference for a human editor.
    if (containsComment(start(i), end(i)))
      return(manual("Contains comments within the legacy comparison."))

    actual <- value(args$test)
    ref <- value(args$ref)
    # Resolve only neighboring, unconditional assignments in the same block.
    # Intervening code could have replaced the node or mutated its rows.
    prev <- integer()
    block <- parents[i]
    if (firstToken(block) == "'{'") {
      siblings <- exprs(block)
      prior <- rev(utils::head(siblings, match(i, siblings) - 1L))
      prev <- utils::head(prior, 2L)
    }
    wanted <- c(if (is.symbol(actual)) as.character(actual), if (is.symbol(ref)) as.character(ref))
    assigned <- list()
    for (j in prev) {
      bits <- children(j)
      if (length(bits) != 3L || !p$token[bits[2L]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN") ||
          !p$text[bits[2L]] %in% c("<-", "=")) break
      lhs <- value(bits[1L])
      if (!is.symbol(lhs) || !as.character(lhs) %in% setdiff(wanted, names(assigned))) break
      assigned[[as.character(lhs)]] <- bits[3L]
    }
    refSymbol <- if (is.symbol(ref)) as.character(ref)
    refRow <- if (!is.null(refSymbol)) assigned[[refSymbol]]
    if (!is.null(refRow)) ref <- value(refRow)
    if (!literal(ref)) return(manual("Reference is computed or its fixed values cannot be traced."))

    rows <- args$test
    alias <- is.symbol(actual)
    if (alias) {
      rows <- assigned[[as.character(actual)]]
      if (is.null(rows)) return(manual("Cannot trace the rows to an adjacent, unmodified table node."))
    }
    if (!dataSelector(value(rows))) return(manual("Expected a whole table's $data or [[\"data\"]] rows."))
    node <- exprs(rows)[1L]
    nodeText <- piece(node)
    nodeExpr <- value(node)
    # Shared fixed references can still serve other checks. Remove a private
    # literal declaration only when this comparison ends its test's local use.
    removeRef <- NULL
    if (!is.null(refRow)) {
      definition <- parents[refRow]
      occurrences <- which(p$token == "SYMBOL" & p$text == refSymbol &
                             p$line1 >= p$line1[context$body] & p$line2 <= p$line2[context$body])
      local <- vapply(occurrences, function(j) start(j) >= start(context$body) &&
                        end(j) <= end(context$body), logical(1))
      if (sum(local) == 2L) {
        later <- siblings[seq.int(match(definition, siblings), length(siblings))]
        if (block != context$body || !all(later %in% c(definition, i, if (alias) parents[rows])))
          return(manual("Review the scope of the single-use reference declaration before removing it."))
        from <- offsets[p$line1[definition]] + 1L
        to <- offsets[p$line2[definition] + 1L]
        before <- substr(source, from, start(definition) - 1L)
        after <- substr(source, end(definition) + 1L, to - 1L)
        if (containsComment(start(definition), end(definition)) || nzchar(trimws(before)) ||
            (nzchar(trimws(after)) && !startsWith(trimws(after), "#")))
          return(manual("The single-use reference shares its source with comments or other code."))
        removeRef <- if (nzchar(trimws(after))) edit(definition, "") else
          list(from = from, to = to, text = "")
        finalEdits <<- c(finalEdits, list(removeRef))
      }
    }
    leaf <- if (is.symbol(nodeExpr)) as.character(nodeExpr) else ""
    if (is.call(nodeExpr) && length(nodeExpr) == 3L && is.symbol(nodeExpr[[1L]]) &&
        as.character(nodeExpr[[1L]]) %in% c("$", "[["))
      if (is.symbol(nodeExpr[[3L]]) || is.character(nodeExpr[[3L]]) || is.numeric(nodeExpr[[3L]]))
        leaf <- as.character(nodeExpr[[3L]])
    label <- if (!is.null(args$label)) value(args$label)
    priorHeaders <- headers[as.integer(names(headers)) <= p$line1[i]]
    title <- if (length(priorHeaders)) priorHeaders[[length(priorHeaders)]] else fileTitle
    multi <- diff(findInterval(c(start(context$body) - 1L, end(context$body)), targetStarts)) > 1L
    stem <- migrationSnapshotStem(context$desc, label, leaf, title, generated, multi)
    name <- uniqueSnapshotName(stem, used)
    used <<- c(used, name)
    nameText <- encodeString(name, quote = '"')
    check <- function(fun, node) paste0(fun, "(", node, ", ", nameText, ")")
    captureNode <- check(".jaspToolsCaptureTable", nodeVar)
    if (alias) {
      # The binding stays at the original extraction point. Keep both the
      # timing of the producer and the exact value available to subsequent R.
      bind <- edit(node, paste0("(", nodeVar, " <- ", nodeText, ")"))
      captureEdits <<- c(captureEdits, list(bind))
      old <- piece(i)
      assignment <- parents[rows]
      remaining <- exprs(block)
      if (!is.null(removeRef)) remaining <- setdiff(remaining, definition)
      private <- block %in% generatedBodies && symbol(actual, "table")
      if (!private && block == context$body && identical(utils::tail(remaining, 2L), c(assignment, i)))
        private <- all(vapply(utils::head(remaining, -2L), function(j) ordinarySetup(value(j), actual), logical(1)))
      if (private && !containsComment(end(node) + 1L, end(rows))) {
        # This row assignment has no other consumer and can hold the node.
        finalEdits <<- c(finalEdits, list(edit(rows, nodeText)))
        target <- piece(args$test)
      } else {
        # Other code can still read the rows. Keep that assignment as written
        # and give the complete node its own local variable.
        insert <- list(from = start(assignment), to = start(assignment) - 1L,
                       text = paste0(nodeVar, " <- ", nodeText, "\n", indentation(assignment)))
        finalEdits <<- c(finalEdits, list(insert, edit(node, nodeVar)))
        target <- nodeVar
      }
    } else {
      # Inject the binding into the original test argument so evaluation
      # order, named arguments and the legacy expectation itself are retained.
      old <- paste0(substr(source, start(i), start(node) - 1L),
                    "(", nodeVar, " <- ", nodeText, ")",
                    substr(source, end(node) + 1L, end(i)))
      target <- nodeText
    }
    indent <- indentation(i)
    # An unqualified call could name a module-specific expectation, or its
    # apparent literal baseline could use a locally redefined constructor.
    # Resolve these in the actual test scope before treating it as a fixed
    # comparison under jaspTools' rules.
    functions <- intersect(all.names(ref, unique = TRUE), c("list", "c", "+", "-"))
    qualified <- if (length(functions)) paste0("base::`", functions, "`")
    if (is.symbol(value(exprs(i)[1L]))) {
      functions <- c(functions, "expect_equal_tables")
      qualified <- c(qualified, "jaspTools::expect_equal_tables")
    }
    guards <- if (length(functions)) paste0("if (!base::identical(`", functions, "`, ", qualified,
      ")) base::stop(\"Custom ", functions, "() requires manual migration.\")")
    validation <- paste0("{\n", paste0(indent, "  ", c(guards, old, captureNode), collapse = "\n"),
                         "\n", indent, "}")
    captureEdits <<- c(captureEdits, list(edit(i, validation)))
    finalEdits <<- c(finalEdits, list(edit(i, check("jaspTools::expect_snapshot_table", target))))
    list(status = "ready", name = name, reason = "")
  }
  for (target in targets) {
    fun <- parents[target]
    if (!isFunction(value(fun), "expect_equal_tables", "jaspTools")) next
    i <- parents[fun]
    result <- plan(i)
    sites[nrow(sites) + 1L, ] <- list(p$line1[i], result$name, result$status, result$reason)
  }
  if (!length(finalEdits)) return(list(text = lines, capture = lines, sites = sites))

  # Register pre-existing references even when a top-level analysis fails
  # before the first test body executes. Keep file headers and shebangs.
  top <- exprs(0L)[1L]
  if (!identical(value(top), quote(jaspTools::preserveSnapshotReferences()))) {
    guard <- list(from = start(top), to = start(top) - 1L, text = "jaspTools::preserveSnapshotReferences()\n")
    finalEdits <- c(list(guard), finalEdits)
    captureEdits <- c(list(guard), captureEdits)
  }
  rewrite <- function(edits) {
    from <- vapply(edits, "[[", numeric(1), "from")
    to <- vapply(edits, "[[", numeric(1), "to")
    order <- order(from, to)
    gaps <- substring(source, c(1L, to[order] + 1L), c(from[order] - 1L, nchar(source)))
    gaps[seq_along(edits)] <- paste0(utils::head(gaps, -1L), vapply(edits[order], "[[", "", "text"))
    # The sentinel retains any trailing blank source lines.
    out <- strsplit(paste0(paste0(gaps, collapse = ""), "\n."), "\n", fixed = TRUE)[[1L]]
    utils::head(out, -1L)
  }
  list(text = rewrite(finalEdits), capture = rewrite(captureEdits), sites = sites)
}

migrationSnapshotStem <- function(description, label, key, title, generated, multi) {
  label <- snapshotNameSlug(label)
  if (nzchar(label)) return(label)

  key <- snapshotKeySlug(key)
  if (key %in% c("node", "result", "results", "table")) key <- ""
  title <- snapshotNameSlug(title)
  desc <- sub("^[0-9]+(?:[.][0-9]+)+\\s+", "", description, perl = TRUE)
  desc <- snapshotNameSlug(desc)
  # Generated files share a test case across all outputs from an analysis.
  if (grepl("-analysis-[0-9]+-results-match$", desc)) {
    analysis <- sub("^.*-(analysis-[0-9]+)-results-match$", "\\1", desc)
    return(paste(analysis, if (nzchar(key)) key else "table", sep = "-"))
  }
  if (generated && endsWith(desc, "-results-match") && nzchar(key)) return(key)
  if (grepl("^main-table-(results-)?match(es)?-for-", desc)) {
    desc <- sub("^main-table-(results-)?match(es)?-for-", "", desc)
    if (nzchar(title) && endsWith(title, desc)) return(title)
  } else if (grepl("^(main-)?table-(results-)?match(es)?($|-)", desc)) {
    return(if (nzchar(title)) title else if (nzchar(key)) key else "table")
  }
  desc <- sub("(-(table|results|table-results))?-match(es)?$", "", desc)
  desc <- sub("-is-consistent-with-.*$", "", desc)
  if (nzchar(desc) && !desc %in% c("main-table", "table", "results")) {
    if (multi && nzchar(key) && !grepl(key, desc, fixed = TRUE)) return(paste(desc, key, sep = "-"))
    return(desc)
  }
  if (nzchar(key)) key else if (nzchar(title)) title else "table"
}
