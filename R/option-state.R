isPreparedOptions <- function(x) {
  isTRUE(attr(x, "jaspTools.preparedOptions", exact = TRUE))
}

markPreparedOptions <- function(x) {
  attr(x, "jaspTools.preparedOptions") <- TRUE
  x
}
