
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RHESSysIOinR

## Overview

RHESSysIOinR contains functions for running
[RHESSys](https://github.com/RHESSys/RHESSys) in R and processing
output. The objective of this package is to clearly and efficiently
produce R code that can be used to setup RHESSys, conduct calibrations
and simulations, and process output. This package supports a
long(er)-term goal of having experiments involving RHESSys be
reproducible. \#OpenScience

## Installation

You can install RHESSysIOinR directly from R

``` r
# install.packages("devtools")
devtools::install_github("RHESSys/RHESSysIOinR")
```

## Contents

### run\_rhessys()

The `run_rhessys()` function is the core method used to setup and run
RHESSys. Documentation on how to set up and use the function is
contained in the package.

### Utilities and helper funcitons

<!-- If you add to the package, add important exported functions here -->

<!-- this could be in a table, but bulleted list also looks good, idk -->

  - `build_redefine()`: generates redefine worldfiles
  - `cal.wyd()`: calculates wateryear day
  - `change_def_file()`: create new def file based on existing file and
    input pars
  - `clim_auto()`: helper to simplify climate input for `run_rhessys()`
  - `evaluation()`: used to evaluate model output
  - `input_tec()`: helper to simplify input of tec events
  - `mkdate()`: generates wy, yd, and wyd based on day, month, year
  - `read_clim()`: reads rhessys formatted climate data into R
  - `readin_rhessys_output()`: reads rhessys output into R, includes
    some subsetting and simply aggregation
  - `readin_rhessys_output_old()`: legacy method to read in rhessys
    output
  - `tec_repeat()`: generates repeating tec events for input into
    `run_rhessys()`, can also copy+rename a redefine file as needed
  - `write_sample_clim()`: generate new climate based on existing
    climate
