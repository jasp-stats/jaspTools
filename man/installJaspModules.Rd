% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkg-setup.R
\name{installJaspModules}
\alias{installJaspModules}
\title{Install all modules and R packages from jasp-stats}
\usage{
installJaspModules(onlyIfMissing = FALSE, quiet = FALSE)
}
\arguments{
\item{onlyIfMissing}{Boolean. Should JASP reinstall everything or should it only install packages that are not installed on your system?}

\item{quiet}{Boolean. Should the installation procedure produce output?}
}
\description{
This function downloads all JASP modules locally and then installs them, to ensure all dependencies between JASP modules are resolved correctly.
}