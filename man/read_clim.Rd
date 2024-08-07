% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_clim.R
\name{read_clim}
\alias{read_clim}
\title{read_clim}
\usage{
read_clim(clim_in, dates_out = FALSE, return_base = FALSE)
}
\arguments{
\item{clim_in}{Climate file - prefix will return all matching data, including suffix returns just that time series
(e.g. 'site.rain' only return the precipitation time series).}

\item{dates_out}{Should start and end dates be output?}

\item{return_base}{Should info from the basestation only be returned. If FALSE, data will be returned.}
}
\description{
Read in rhessys formatted climate to R - works for any daily input, can have mismatched dates, missing values will be filled by NA. 
Works for both standard and netcdf clim, if you specify a basestation.
}
\author{
Will Burke
}
