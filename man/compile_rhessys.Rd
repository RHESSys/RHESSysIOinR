% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compile_rhessys.R
\name{compile_rhessys}
\alias{compile_rhessys}
\title{compile_rhessys}
\usage{
compile_rhessys(
  location,
  delete_objs = TRUE,
  destination = NULL,
  make_args = NULL,
  ignore.stdout = FALSE
)
}
\arguments{
\item{location}{The file patch to where the rhessys makefile or folder is}

\item{delete_objs}{TRUE/FALSE to delete objects (before and after)}

\item{destination}{Optional input of where to move resulting RHESSys executable}

\item{make_args}{Arguments passed to the end of the make commandline call (examples: "wmfire='T'", "clean", or "clobber", )}

\item{ignore.stdout}{Passed through to system()}
}
\description{
Compiles rhessys, with options to delete objects and move resulting RHESSys executable
}
\author{
Will Burke
}
