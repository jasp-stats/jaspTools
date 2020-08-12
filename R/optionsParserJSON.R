analysisOptionsFromJSONString <- function(x) {
  json <- try(rjson::fromJSON(x)) # jsonlite can't deal with \n in strings.. rjson can.
  if (inherits(json, "try-error"))
    stop("There was a problem parsing the JSON string, cannot create the options list")

  if (!"options" %in% names(json))
    stop("There is no \"options\" field in your JSON string, cannot create options list")

  options <- json[["options"]]
  if (!is.null(names(options)) && ".meta" %in% names(options))
    options[[".meta"]] <- NULL

  return(options)
}
