# Keep names readable on disk, including titles with accents or camel-case keys.
snapshotNameSlug <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return("")
  x <- stringi::stri_trans_general(gsub("</?[A-Za-z][^>]*>", "", x), "NFKC; Any-Latin; Latin-ASCII")
  x <- gsub("([A-Z])([A-Z][a-z])", "\\1-\\2", x)
  x <- gsub("([a-z0-9])([A-Z])", "\\1-\\2", x)
  x <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("\\bttest(s?)\\b", "t-test\\1", x)
  gsub("^-|-$", "", x)
}

snapshotKeySlug <- function(key) {
  key <- as.character(key)
  parts <- strsplit(key, "_", fixed = TRUE)[[1L]]
  parents <- utils::head(parts, -1L)
  wrappers <- which(grepl("([Cc]ontainer|Summary)$", parents))
  if (length(wrappers)) {
    # Keep path components that identify the model, subgroup or other case.
    last <- max(wrappers)
    cases <- parents[seq_len(last - 1L)]
    cases <- cases[!grepl("([Cc]ontainer|Summary|Results)$", cases)]
    cases <- sub("([A-Za-z])([0-9]+)$", "\\1-\\2", cases)
    leaf <- parts[-seq_len(last)]
    if (grepl("^(container|table)([A-Z0-9]|$)", leaf[1L])) {
      # A generic child such as containerS_tableS needs its parent for context.
      group <- sub("([Cc]ontainer|Summary)$", "", parents[wrappers[1L]])
      leaf <- sub("^(container|table)(?=[A-Z0-9]|$)", "", leaf, perl = TRUE)
      leaf <- c(group, unique(leaf[nzchar(leaf)]))
    }
    key <- paste(c(cases, leaf), collapse = "-")
  }
  sub("-table$", "", snapshotNameSlug(key))
}

shortenSnapshotName <- function(stem, limit) {
  if (nchar(stem) <= limit) return(stem)
  words <- strsplit(stem, "-", fixed = TRUE)[[1L]]
  # Keep both the initial context and the final result name when space allows.
  while (length(words) > 1L && nchar(stem) > limit) {
    words <- words[-ceiling(length(words) / 2)]
    stem <- paste(words, collapse = "-")
  }
  substr(stem, 1L, limit)
}

uniqueSnapshotName <- function(stem, used) {
  stem <- shortenSnapshotName(stem, 80L)
  if (grepl("^(con|prn|aux|nul|com[1-9]|lpt[1-9])$", stem))
    stem <- paste0(stem, "-snapshot")
  name <- stem
  n <- 1L
  while (name %in% used) {
    n <- n + 1L
    name <- paste0(shortenSnapshotName(stem, 79L - nchar(n)), "-", n)
  }
  name
}
