# This eventually needs to morph into a vignette example of some sort.
# Currently this is a testing input file for Ryan Bart

library(RHESSysIOinR)
library(tidyverse)

# Note: Need to change directory!! This example uses fire_effects_model directory.

# ---------------------------------------------------------------------
# Model inputs

# Define path to a pre-selected df containing parameter sets
input_preexisting_table <- NULL
#input_preexisting_table <- file.path(input_rhessys$output_folder, paste(input_rhessys$output_filename, "_parameter_sets.txt", sep=""))
# Processing options
parameter_method <- "lhc"
output_method <- "R" # alternatively "awk"
# RHESSys Inputs

input_rhessys <- list()
#input_rhessys$rhessys_version <- "/Users/christina/Desktop/LocalModel/rhessys_git/rhessys/rhessys5.20.1.thin" # "bin/rhessys5.20.fire_off"
input_rhessys$rhessys_version <- "/Users/naomitague/Desktop/model/RHESSys/rhessys/rhessys6.0"
input_rhessys$tec_file <- "sim/tecfiles/tec.test"
input_rhessys$world_file <- "sim/worldfiles/coldstream.3p.world.su100"
input_rhessys$world_hdr_prefix <- "ptest"
input_rhessys$flow_file <- "sim/flowtables/coldstream.3p.flow"
input_rhessys$start_date <- "1985 8 1 1"
input_rhessys$end_date <- "2012 10 1 1"
input_rhessys$output_folder <- "sim/out"
input_rhessys$output_filename <- "sen"
input_rhessys$command_options <- c("-climrepeat -c -p -b -g -tchange 0 0 ")

# HDR (header) file
input_hdr_list <- list()
input_hdr_list$basin_def <- c("sim/defs/basin.def")
input_hdr_list$hillslope_def <- c("sim/defs/hill.def")
input_hdr_list$zone_def <- c("sim/defs/zone.def")
input_hdr_list$soil_def <- c("sim/defs/patch.def")
input_hdr_list$landuse_def <- c("sim/defs/lu.def")
input_hdr_list$stratum_def <- c("sim/defs/7conifer.def", "sim/defs/22grass.def", "sim/defs/8pine.def")
input_hdr_list$base_stations <- c("sim/clim/csranch_base")


# Define path to a pre-selected df containing parameter sets
input_preexisting_table <- NULL
#input_preexisting_table <- file.path(input_rhessys$output_folder, paste(input_rhessys$output_filename, "_parameter_sets.txt", sep=""))


# Def file parameter changes
# List of lists containing def_file, parameter and parameters values

#input_def_list <- NULL
input_def_list <- list(
 list(input_hdr_list$stratum_def[1], "epc.leaf_turnover", c(0.1,0.3,4)),
 list(input_hdr_list$stratum_def[1], "epc.proj_sla", c(8,15,4)),
 list(input_hdr_list$stratum_def[2], "epc.proj_sla", c(40,50,4)),
 list(input_hdr_list$stratum_def[2], "epc.leaf_cn", c(10,100,4)),
 list(input_hdr_list$stratum_def[3], "epc.leaf_turnover", c(0.1,0.3,4)),
 list(input_hdr_list$stratum_def[3], "epc.proj_sla", c(8,15,4))
#  list(input_hdr_list$stratum_def[1], "epc.netpabs_sla_parm", c(0.8 ,1.2,4)),
#  list(input_hdr_list$stratum_def[2], "epc.shade_sla_mult", c(1,1.1,2))
)

# Make climate base station file

#input_clim_base_list <- NULL
input_clim_base_list <- list(
  list(core = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       annual = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       monthly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       daily = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       hourly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE)
  )
)
input_clim_base_list[[1]][[1]][1,] <- data.frame(c1=1, c2="base_station_id",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][2,] <- data.frame(c1=100.0, c2="x_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][3,] <- data.frame(c1=100.0, c2="y_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][4,] <- data.frame(c1=1748, c2="z_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][5,] <- data.frame(c1=3.5, c2="effective_lai",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][6,] <- data.frame(c1=2, c2="screen_height",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[2]][1,] <- data.frame(c1="annual", c2="annual_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[2]][2,] <- data.frame(c1=0, c2="number_non_critical_annual_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[3]][1,] <- data.frame(c1="monthly", c2="monthly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[3]][2,] <- data.frame(c1=0, c2="number_non_critical_monthly_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[4]][1,] <- data.frame(c1="sim/clim/csranch_daily", c2="daily_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[4]][2,] <- data.frame(c1=0, c2="number_non_critical_daily_sequences",stringsAsFactors=FALSE)
#input_clim_base_list[[1]][[4]][3,] <- data.frame(c1="vpd", c2="daily_climate_prefix",stringsAsFactors=FALSE)


input_clim_base_list[[1]][[5]][1,] <- data.frame(c1="hourly", c2="hourly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[5]][2,] <- data.frame(c1=0, c2="number_non_critical_hourly_sequences",stringsAsFactors=FALSE)



# Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
 input_dated_seq_list <- NULL
# input_dated_seq_list <- list()
# input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.1,stringsAsFactors=FALSE)
# input_dated_seq_list[[2]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.2,stringsAsFactors=FALSE)
# input_dated_seq_list[[3]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.3,stringsAsFactors=FALSE)
# input_dated_seq_list[[4]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.4,stringsAsFactors=FALSE)
# input_dated_seq_list[[5]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.5,stringsAsFactors=FALSE)
# input_dated_seq_list[[6]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.6,stringsAsFactors=FALSE)
# input_dated_seq_list[[7]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.7,stringsAsFactors=FALSE)
# input_dated_seq_list[[8]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.8,stringsAsFactors=FALSE)
# input_dated_seq_list[[9]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.9,stringsAsFactors=FALSE)
# input_dated_seq_list[[10]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=1.0,stringsAsFactors=FALSE)

# Standard sub-surface parameters
# input_standard_par_list <- NULL
# input_standard_par_list <- list(
#   m = c(1.792761),
#   k = c(1.566492),
#   m_v = c(1.792761),
#   k_v = c(1.566492, 4),
#   pa = c(7.896941),
#   po = c(1.179359),
#   gw1 = c(0.1668035),
#   gw2 = c(0.178753)
# )

#input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(0.5,5, 4),
  k = c(1, 100, 4),
  m_v = c(0.5,5, 4),
  k_v = c(1, 100, 4),
  pa = c(1.0, 1.2, 4),
  po = c(1.1, 1.3, 4),
  gw1 = c(0.0, 0.0, 4),
  gw2 = c(1.0, 1.0, 4)
)



# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1985, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1985, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)


# Data frame containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
# output_variables <- NULL
output_variables <- data.frame(variable=character(),out_file=character(),stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("lai", "sen_basin.daily",stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("trans", "sen_basin.daily",stringsAsFactors=FALSE)
output_variables[3,] <- data.frame("evap", "sen_basin.daily",stringsAsFactors=FALSE)
output_variables[4,] <- data.frame("streamflow", "sen_basin.daily",stringsAsFactors=FALSE)
#
# Output variables for awk version
# output_variables <- NULL
#output_variables <- data.frame(variable=character(),awk_path=character(),out_file=character(),stringsAsFactors=FALSE)

# ---------------------------------------------------------------------


system.time(
  run_rhessys(parameter_method = parameter_method,
              output_method = output_method,
            input_rhessys = input_rhessys,
            input_hdr_list = input_hdr_list,
            input_preexisting_table = input_preexisting_table,
            input_def_list = input_def_list,
            input_standard_par_list = input_standard_par_list,
            input_clim_base_list = input_clim_base_list,
            input_dated_seq_list = input_dated_seq_list,
            input_tec_data = input_tec_data,
            output_variables = output_variables)
)

#beep(1)

