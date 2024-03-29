#' View the configuration of jaspTools.
#'
#' There are a number of package options you can adjust. These are printed when
#' you call \code{viewPkgOptions}. You can adjust these values through
#' \code{setPkgOption}.
#'
#' @details \code{language}:
#' The language that the analysis runs in (passed to R environment variable "LANG"). Defaults to "en" to avoid translation of strings.
#' See https://www.gnu.org/software/gettext/manual/html_node/Usual-Language-Codes.html
#'
#' @details \code{data.dirs}:
#' The directories where datafiles can be found so they may be used by \code{runAnalysis}.
#'
#' @details \code{html.dir}:
#' The directory where the javascript, css and .html files can be found to create output identical to JASP. Used by \code{view}.
#'
#' @details \code{view.in.rstudio}:
#' This option specifies where the html output of \code{runAnalysis} will be shown: inside RStudio or in a separate webbrowser.
#'
#' @details \code{reinstall.modules}:
#' When you run an analysis or test it, jaspTools calls the *installed* version of the module.
#' This option specifies if the installed version should be reinstalled automatically when you make any changes to your module.
#'
#' @details \code{module.dirs}:
#' The directories that hold the source for the JASP module(s) you are working on.
#' These module directories are used to find the R functions etc. in \code{runAnalysis} and the various testing functions.
#'
#' @return A print of the configurable options.
#' @export viewPkgOptions
viewPkgOptions <- function() {
  pkgOptions <- .pkgenv[["pkgOptions"]]
  if (length(pkgOptions) == 0)
    stop("No package options to show. Did you run `setupJaspTools()` yet?")

  for (i in seq_along(names(pkgOptions))) {
    name  <- names(pkgOptions)[i]
    value <- getPkgOption(name)
    message(name, " = ", paste(value, collapse = ", "), "\n")
  }

  message("\nA description of these options can be found at `?viewPkgOptions`")
}


#' Change the value of an option in jaspTools.
#'
#' Sets a package option to a new value (to see what is available run
#' \code{?viewPkgOptions}). Value changes are automatically
#' incorporated when any jaspTools function is called.
#'
#'
#' @param name String name of the option.
#' @param value Value the option should be set to.
#' @examples
#'
#' setPkgOption("module.dirs", c("~/Documents/Github/Regression", "~/Document/Github/Frequencies"))
#'
#' @export setPkgOption
setPkgOption <- function(name, value) {
  if (!.isSetupComplete())
    stop("jaspTools is not configured yet. Did you run `setupJaspTools()`?")

  if (length(name) > 1)
    stop("Please only set one option at a time")

  if (!name %in% names(.pkgenv[["pkgOptions"]]))
    stop(name, " is not a valid option to set")

  # set relative paths to absolute paths to ensure they will work if the wd changes
  if (any(endsWith(name, c(".dir", ".dirs")))) {
    for (i in seq_along(value)) {
      if (length(value) == 1L && (is.null(value) || value == "")) # allow users to reset paths to null values
        next

      if (!dir.exists(value[i])) # if the value is not a null value it should be a valid path
        stop("Directory ", value[i], " does not exist")

      value[i] <- gsub("[\\/]$", "", normalizePath(value[i])) # normalize path and strip trailing slashes
    }
  }

  .pkgenv[["pkgOptions"]][name] <- list(value)

  message("`", name, "` -> ", paste(value, collapse = ", "))
}

getPkgOption <- function(name) {
  if (!.isSetupComplete())
    stop("jaspTools is not configured yet. Did you run `setupJaspTools()`?")

  return(.pkgenv[["pkgOptions"]][[name]])
}

# ... and the internally accessible options
.setInternal <- function(name, value) {
  .pkgenv[["internal"]][[name]] <- value
}

.getInternal <- function(name) {
  if (! name %in% names(.pkgenv[["internal"]]))
    stop(paste("Could not locate internal variable", name))
  return(.pkgenv[["internal"]][[name]])
}
