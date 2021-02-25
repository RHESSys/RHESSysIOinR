#' RHESSysIOinR
#'
#' RHESSysIOinR contains functions for running [RHESSys](https://github.com/RHESSys/RHESSys) in R
#' and processing output. The objective of this package is to clearly and efficiently produce R code
#' that can be used to setup RHESSys, conduct calibrations and simulations, and process output. This
#' package supports a long(er)-term goal of having experiments involving RHESSys be reproducible.
#'
#' @docType package
#' @name RHESSysIOinR
#'
# Import package operators
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @importFrom stats runif weighted.mean
#' @importFrom utils read.csv read.table write.table

# from data.table docs: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())
