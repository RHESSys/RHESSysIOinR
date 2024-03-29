% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IOin_tec_std.R
\name{IOin_tec_std}
\alias{IOin_tec_std}
\title{IOin_tec_std}
\usage{
IOin_tec_std(start, end, output_state = TRUE)
}
\arguments{
\item{start}{start date of run}

\item{end}{End date of run - the last entire day to be run}

\item{output_state}{TRUE/FALSE if an output_current_state tec event should be scheduled at the end of the simulation}
}
\description{
Input function to construct a dataframe of standard tec events, including start, grow start, and output state
}
\author{
Will Burke
}
