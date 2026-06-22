# Minimal local replacements for non-dataset JASP Rcpp bridge callbacks.
#
# The active contract is intentionally narrow: jaspTools only provides fallback
# objects that jaspBase::runWrappedAnalysis()/runJaspResults() can request
# through jaspBase:::.fromRCPP() while running outside Desktop. Dataset loading,
# option parsing, option encoding, column encoding/decoding, and computed-column
# mutation belong to jaspSyntax/SyntaxInterface or Desktop itself.
#
# The bridge objects are kept in the jaspTools namespace because
# jaspBase:::.fromRCPP() resolves hidden functions via getAnywhere().
.rbridgeNativeSymbols <- function() {
  c(
    ".baseCitation",
    ".ppi",
    ".requestTempFileNameNative",
    ".requestTempRootNameNative",
    ".requestStateFileNameNative",
    ".imageBackground"
  )
}

.insertRbridgeIntoEnv <- function(env) {
  namespace <- asNamespace("jaspTools")
  for (symbol in .rbridgeNativeSymbols()) {
    assign(symbol, get(symbol, envir = namespace, inherits = FALSE), envir = env)
  }

  invisible(env)
}

.snapshotRbridgeEnv <- function(env, symbols = .rbridgeNativeSymbols()) {
  state <- lapply(symbols, function(symbol) {
    if (!exists(symbol, envir = env, inherits = FALSE))
      return(list(exists = FALSE, locked = FALSE, value = NULL))

    list(
      exists = TRUE,
      locked = bindingIsLocked(symbol, env),
      value = get(symbol, envir = env, inherits = FALSE)
    )
  })
  names(state) <- symbols
  state
}

.restoreRbridgeEnv <- function(env, state) {
  if (!is.list(state))
    return(invisible(FALSE))

  for (symbol in names(state)) {
    previous <- state[[symbol]]
    currentlyExists <- exists(symbol, envir = env, inherits = FALSE)
    currentlyLocked <- currentlyExists && bindingIsLocked(symbol, env)

    if (currentlyLocked)
      unlockBinding(symbol, env)

    if (isTRUE(previous$exists)) {
      assign(symbol, previous$value, envir = env)
      if (isTRUE(previous$locked))
        lockBinding(symbol, env)
    } else if (currentlyExists) {
      rm(list = symbol, envir = env)
    }
  }

  invisible(TRUE)
}

.ppi <- 192

.baseCitation <- "x"

.requestTempFileNameNative <- function(...) {
  root <- getTempOutputLocation("html")
  dir.create(file.path(root, "plots"), recursive = TRUE, showWarnings = FALSE)
  numPlots <- length(list.files(file.path(root, "plots")))
  list(
    root = root,
    relativePath = file.path("plots", paste0(numPlots + 1, ".png"))
  )
}

.requestTempRootNameNative <- function() {
  root <- getTempOutputLocation("html")
  dir.create(file.path(root, "plots"), recursive = TRUE, showWarnings = FALSE)
  list(root = root, relativePath = "")
}

.requestStateFileNameNative <- function() {
  stateFile <- .runStateFilePath()
  if (!file.exists(stateFile))
    .resetRunStateFile()

  list(
    root = dirname(stateFile),
    relativePath = basename(stateFile)
  )
}

.runStateFilePath <- function() {
  root <- getTempOutputLocation("state")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.path(root, "state")
}

.resetRunStateFile <- function() {
  stateFile <- .runStateFilePath()
  state <- NULL
  save(state, file = stateFile, compress = FALSE)
  invisible(stateFile)
}

.imageBackground <- function(...) return("white")
