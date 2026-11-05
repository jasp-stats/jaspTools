# The options of analysisOptions(<analysis name>) are the defaults of an EMPTY form. Some defaults depend on
# other options (`checked: secondaryFactor.count === 0`, `defaultValue: (secondaryFactor.count === 0) ? 0.1 : 0.2`),
# and the form keeps an option value it is given. So runAnalysis only gives the form the options a test set: the
# form gives the others their default, as in JASP. The list remembers which options were set through `$<-`, `[[<-`
# and `[<-` (and so utils::modifyList). A list that does not (from a .jasp file, a json string, or built by hand)
# is sent as it is.

formDefaultOptions <- function(options) {
  attr(options, "setOptions") <- character(0)
  class(options) <- c("jaspFormOptions", "list")
  options
}

.setFormOptions <- function(x, names, assign) {
  cls <- class(x)
  set <- attr(x, "setOptions")
  x   <- assign(unclass(x)) # unclass keeps the other attributes
  attr(x, "setOptions") <- union(set, names[!is.na(names) & nzchar(names)])
  class(x) <- cls
  x
}

.optionNames <- function(x, i) {
  if (is.character(i))	return(i)
  if (missing(i))		return(names(x))
  names(x)[i]
}

#' @export
`$<-.jaspFormOptions` <- function(x, name, value) {
  .setFormOptions(x, name, function(l) { l[[name]] <- value; l })
}

#' @export
`[[<-.jaspFormOptions` <- function(x, i, value) {
  # options[["a"]][["b"]] <- v reaches here with i = "a"; a recursive index c("a", "b") sets option "a"
  .setFormOptions(x, .optionNames(x, i[1L]), function(l) { l[[i]] <- value; l })
}

#' @export
`[<-.jaspFormOptions` <- function(x, i, value) {
  if (missing(i))
    return(.setFormOptions(x, names(x), function(l) { l[] <- value; l }))
  .setFormOptions(x, .optionNames(x, i), function(l) { l[i] <- value; l })
}

#' @export
print.jaspFormOptions <- function(x, ...) {
  print(formOptionsAsList(x, onlySet = FALSE), ...)
  invisible(x)
}

# The options as the plain named list runAnalysis gives the form: with `onlySet`, those a test set.
formOptionsAsList <- function(options, onlySet = TRUE) {
  if (!inherits(options, "jaspFormOptions"))
    return(options)

  set     <- attr(options, "setOptions")
  options <- unclass(options)
  attributes(options) <- list(names = names(options))

  if (onlySet)
    options <- options[intersect(names(options), set)]

  options
}

# analysisOptions() of a .jasp file gives a variables option as `x` and `x.types`, as the analysis gets it, and a test
# sets the type of a variable the same way (options$variables.types <- "scale"). The form reads the types only inside the
# option ({value, types}), else each variable gets the type of its column: so they are put back there. Types that follow
# the structure of the rows (e.g. list(list(components = "scale"))) are sent as they are.
typesIntoOptionValues <- function(options) {
  for (typesName in grep("[.]types$", names(options), value = TRUE)) {
    name  <- sub("[.]types$", "", typesName)
    value <- options[[name]]
    types <- options[[typesName]]

    plainTypes <- length(types) > 0 && is.null(names(types)) &&
      all(vapply(types, function(type) is.character(type) && is.null(names(type)), logical(1)))

    if (is.null(value) || !is.null(names(value)) || !plainTypes)
      next

    options[[name]]      <- list(value = value, types = types)
    options[[typesName]] <- NULL
  }
  options
}
