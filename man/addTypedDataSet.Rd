% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{addTypedDataSet}
\alias{addTypedDataSet}
\title{Add types for a dataset.}
\usage{
addTypedDataSet(name, lst)
}
\arguments{
\item{name}{The path to the dataset on disk.}

\item{lst}{A list of column names and their types.}
}
\description{
Add types for a dataset.
}
\details{
Note that this has already been done for debug.csv and test.csv.
This is an alternative interface to specify types in jaspTools.
The usual way is to specify a key in the options object with the types.
For example, if \code{options[["variables"]] == c("contNormal", "facFive", "contBinom")}
then one could indicate the types by writing
\code{options[["variables.types"]] == c("scale", "ordinal", "nominal")}.
With \code{addTypedDataSet} this becomes:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{addTypedDataSet(
 "test.csv",
 list(
   contNormal   = "scale",
   facFive      = "ordinal",
   contBinom    = "nominal"
 )
)
}\if{html}{\out{</div>}}

the main benefit is that this only needs to be done once,
instead of being repeated for each options object.
}
\examples{
addTypedDataSet(
 "test.csv",
 list(
   V1           = "scale",
   contNormal   = "scale",
   contGamma    = "scale",
   contBinom    = "nominal"
 )
)
}
