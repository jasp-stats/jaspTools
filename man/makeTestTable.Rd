\name{makeTestTable}
\alias{makeTestTable}
\title{Aids in the creation of tests for tables.}
\usage{
makeTestTable(rows)
}
\arguments{
\item{rows}{A list with lists of rows (i.e., a JASP table).}
}
\value{
Copy-paste ready output which may serve as the reference to test tables against.}
\description{
This function is designed to make it easier to create unit tests for tables. It strips off attributes and flattens the structure until a list remains with dimension 1. Output is then produced which can be immediately placed in the test file.}
\examples{
options <- JASPTools::analysisOptions("BinomialTest")
options[["variables"]] <- "contBinom"
results <- JASPTools::run("BinomialTest", "debug", options, view=FALSE)
JASPTools::makeTestTable(results[["results"]][["binomial"]][["data"]])
}
