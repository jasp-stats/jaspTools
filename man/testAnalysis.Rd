\name{testAnalysis}
\alias{testAnalysis}
\title{Test a specific JASP analysis.}
\usage{
testAnalysis(analysis)
}
\arguments{
\item{analysis}{String name of the analysis to test.}
}
\description{
Tests a specific R analysis found under JASP-Tests. Useful to perform before making a pull request, to prevent failing builds.
}
\examples{
JASPTools::testAnalysis("AnovaBayesian")
}
