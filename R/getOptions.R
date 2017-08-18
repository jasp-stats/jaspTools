.analysisOptionsFromFile <- function(analysis) {

  file <- paste0(.getPkgOption("json.dir"), analysis, ".json")
  analysisOpts <- try(rjson::fromJSON(file = file), silent=TRUE)

  if (inherits(analysisOpts, "try-error")) {
    stop("The analysis you supplied could not be found.
         Please ensure that (1) its name matches the main R function
         and (2) your working directory is set properly.")
  }

  if ("options" %in% analysisOpts) {
    return(analysisOpts[["options"]])
  } else {
    stop("The json file was found, but it appears to be invalid")
  }

}

.analysisOptionsFromQt <- function(x) {

  json <- try(rjson::fromJSON(x), silent=TRUE)

  if (inherits(json, "try-error")) {
    stop("Your json is invalid, please copy the entire message
          including the outer braces { } that was send to R in the Qt terminal.
          Remember to use single quotes around the message.")
  } else {
    return(json[["options"]])
  }

}

#' @export
analysisOptions <- function(analysis=NULL, qt=NULL, hint=FALSE) {

  if (is.null(analysis) && is.null(qt)) {
    stop("Please specify the analysis name or copy the json from Qt")
  }

  if (! is.null(analysis) && ! is.null(qt)) {
    warning("Both analysis name and qt output are specified, ignoring the analysis name")
  }

  options <- NULL

  if (! is.null(qt)) {
    options <- .analysisOptionsFromQt(qt)
  }

  if (is.null(options) && ! is.null(analysis)) {
    rawOptions <- .analysisOptionsFromFile(analysis)
    options <- .fillOptions(rawOptions, hint)
  }

  return(options)
}

.fillOptions <- function(options, hint=FALSE) {

  for (i in 1:length(options)) {
    option <- options[[i]]
    if ("default" %in% names(option)) {
      options[[option[["name"]]]] <- option[["default"]]
    } else {
      if (option[["type"]] == "Table" && hint) {
        template <- option[["template"]]
        options[[option[["name"]]]] <- list(list())
        for (j in 1:length(template)) {
          name <- template[[j]][["name"]]
          value <- .optionTypeToValue(template[[j]], hint)
          options[[option[["name"]]]][[1]][[name]] <- value
        }
      } else {
        options[[option[["name"]]]] <- .optionTypeToValue(option, hint)
      }
    }
  }

  return(options)
}

.optionTypeToValue <- function(option, hint=FALSE) {
  switch(option[["type"]],
         Boolean =
           FALSE,

         Integer =
           if (hint) {
             "%420%"
           } else {
             ""
           },

         IntegerArray =
           if (hint) {
             c("%25%", "%95%")
           } else {
             list()
           },

         List =
           option[["options"]][[1]],

         Number =
           option[["value"]],

         Table =
           list(),

         String =
           if (hint) {
             "%SomeString%"
           } else {
             ""
           },

         Term =
           if (hint) {
             "%variable1%"
           } else {
             ""
           },

         Terms =
           if (hint) {
             list(c("%variable1%"),
                  c("%variable2%"),
                  c("%variable1%", "%variable3%"))
           } else {
             list()
           },

         Variable =
           if (hint) {
             "%variable1%"
           } else {
             ""
           },

         Variables =
           if (hint) {
             c("%variable1%", "%variable2%")
           } else {
             list()
           },

         VariablesGroups =
           if (hint) {
             list(c("%variable1%", "%variable2%"),
                  c("%variable3%", "%variable4%"))
           } else {
             list()
           },

         NULL
  )
}
