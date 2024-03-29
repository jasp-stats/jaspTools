% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkg-setup.R
\name{setupJaspTools}
\alias{setupJaspTools}
\title{Setup the jaspTools package.}
\usage{
setupJaspTools(
  pathJaspDesktop = NULL,
  installJaspModules = FALSE,
  installJaspCorePkgs = TRUE,
  quiet = FALSE,
  force = TRUE
)
}
\arguments{
\item{pathJaspDesktop}{(optional) Character path to the root of jasp-desktop if present on the system.}

\item{installJaspModules}{(optional) Boolean. Should jaspTools install all the JASP analysis modules as R packages (e.g., jaspAnova, jaspFrequencies)?}

\item{installJaspCorePkgs}{(optional) Boolean. Should jaspTools install jaspBase, jaspResults and jaspGraphs?}

\item{quiet}{(optional) Boolean. Should the installation of R packages produce output?}

\item{force}{(optional) Boolean. Should a fresh installation of jaspResults, jaspBase, jaspGraphs and the JASP analysis modules proceed if they are already installed on your system? This is ignored if installJaspCorePkgs = FALSE.}
}
\description{
Ensures that analyses can be run, tested and debugged locally by fetching all of the basic dependencies.
This includes fetching the data library and html files and installing jaspBase and jaspGraphs.
If no parameters are supplied the function will interactively ask for the location of these dependencies.
}
