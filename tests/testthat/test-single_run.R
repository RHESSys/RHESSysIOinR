# this is to test input generation and running of a single rhessys simulation
library(RHESSysIOinR)
library(testthat)

# TODO create reference objects to compare to?
# TODO find most efficient way to download and install rhessys - I think using same command line from vignette:
# TODO figure out a way to standardize which version of rhessys to test with

# custom expects
expect_file_exists = function(path) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(path), arg = "path")
  # 2. Call expect()
  expect(
    file.exists(act$val),
    sprintf("%s file does not exist.", act$lab)
  )
  # 3. Invisibly return the value
  invisible(act$val)
}

expect_file_sizeKB_gt = function(path, size_KB) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(path), arg = "path")
  # 2. Call expect()
  expect(
    (file.size(act$val) / 1024) > size_KB,
    sprintf("%s file is not greater than %f KB.", act$lab, size_KB)
  )
  # 3. Invisibly return the value
  invisible(act$val)
}


# PATHS - just setting wd to extdata at start and not doing any system.file() idk if its a problem
#withr::local_dir(new = system.file("extdata", package = "RHESSysIOinR"))
setwd("~/Repos/RHESSysIOinR/inst/extdata/")
setwd(system.file("extdata/", package = "RHESSysIOinR"))

# no support for shallow clone depth so a little slow checking the commits
gert::git_clone(url = "https://github.com/RHESSys/RHESSys",
                path = "./rh_dev",
                branch = "develop")

# this has a bonus effect of testing the compile_rhessys function:
compile_rhessys(location = "rh_dev/")

# find the rhessys bin
rh_ver = dir(path = "rh_dev/rhessys/", pattern = "^rhessys\\d+",recursive = F)

test_that("compile_rhessys works + rhessys compiles via R system", {
  expect_gt(length(rh_ver), 0)
})

rh_path = file.path("rh_dev/rhessys/", rh_ver)


# with new testthat 3 use withr:: -- https://testthat.r-lib.org/articles/test-fixtures.html
# for file cleanup use local_file()
# for working dir change local_dir()

input_rhessys = IOin_rhessys_input(
  version = rh_path,
  tec_file = "tecfiles/w8TC.tec",
  world_file = "worldfiles/w8TC.world",
  world_hdr_prefix = "w8TC",
  flowtable = "flowtables/w8TC.flow",
  start = "1998 10 1 1",
  end = "2000 10 1 1",
  output_folder = "out/",
  output_prefix = "w8TC",
  commandline_options = c("-g -b")
)

input_tec_data = IOin_tec_std(start = "1998 10 1 1",
                              end = "2000 10 1 1",
                              output_state = TRUE)

input_hdr = IOin_hdr(
  basin = "defs/basin.def",
  hillslope = "defs/hill.def",
  zone = "defs/zone.def",
  soil = "defs/soil_sandyloam.def",
  landuse = "defs/lu_undev.def",
  stratum = "defs/veg_douglasfir.def",
  basestations = "clim/w8_base"
)

# test_that("Input core RHESSys info can be generated", {
#
# })
#
# test_that("Input tec file can be generated", {
#
# })
#
# test_that("Input header file can be generated", {
#
# })

run_rhessys_single(input_rhessys = input_rhessys,
                   hdr_files = input_hdr,
                   tec_data = input_tec_data)

test_that("Simplest RHSSys run options run successfully (core rhessys info, tec data, hdr data)", {
  # check there is output file and they aren't empty
  expect_file_exists(path = "out/w8TC_basin.daily")
  expect_file_sizeKB_gt(path = "out/w8TC_basin.daily", size_KB = 5)
  expect_file_exists(path = "out/w8TC_grow_basin.daily")
  expect_file_sizeKB_gt(path = "out/w8TC_grow_basin.daily", size_KB = 5)
})


