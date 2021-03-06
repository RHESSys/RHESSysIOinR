% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/watbal_basin.R
\name{watbal_basin}
\alias{watbal_basin}
\title{watbal_basin.R}
\usage{
watbal_basin(bd)
}
\arguments{
\item{bd}{The basin daily outputs from rhessys, most easily retrieved via `readin_rhessys_output()`}
}
\description{
Water balance for basin daily RHESSys output. To run, if your basin output is called "bd" type "bd=watbal(bd)".
This will add a number of fields to your basin output file, the last of which will be called "watbal".
This is your water balance error. You should see a minor numerical error here even if your water balances
(on the order of 10^-6).  If your watbal values are negative then water inputs are less than water outputs, and vice versa.
}
