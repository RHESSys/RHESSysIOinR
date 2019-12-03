# RHESSys run script for multiscale testing

library(RHESSysIOinR)

# ----- Project/Run Name -----
name = "p301_clip_multi"

n_sim = 1

# ----- Climate Data -----
clim_data = "p301_input/clim/Grove_lowprov_clim"
start_end = read_rhessys_met(clim_data, dates = TRUE)

# change length
start_end[2] = "1961 09 30 24"

# ----- Parameter Method -----
parameter_method = "exact_values"

# ----- Input RHESSys -----
input_rhessys <- list()
input_rhessys$rhessys_version <- "bin/Salsa/rhessys7.0"
input_rhessys$tec_file <- "p301_input/tecfiles/p301_clip.tec"
input_rhessys$world_file <- "p301_input/worldfiles/p301_clip.world"
input_rhessys$world_hdr_prefix <- "p301_clip"
input_rhessys$flow_file <- "p301_input/flowtables/p301_clip.flow"
input_rhessys$start_date <- start_end[1]
input_rhessys$end_date <- start_end[2]
input_rhessys$output_folder <- "output/"
input_rhessys$output_filename <- "p301_clip_multi"
input_rhessys$command_options <- c("-b -h -z -g -p -c")

# ----- Input Headers -----
input_hdr_list <- list()
input_hdr_list$basin_def <- c("p301_input/defs/basin_p301.def")
input_hdr_list$hillslope_def <- c("p301_input/defs/hill_p301.def")
input_hdr_list$zone_def <- c("p301_input/defs/zone_p301.def")
input_hdr_list$soil_def <- c("p301_input/defs/soil_sandyloam.def")
input_hdr_list$landuse_def <- c("p301_input/defs/lu_p301.def")
input_hdr_list$stratum_def <- c("p301_input/defs/veg_p301_conifer.def",
                                "p301_input/defs/veg_p301_grass.def",
                                "p301_input/defs/veg_p301_shrub.def",
                                "p301_input/defs/veg_p301_noveg.def")
input_hdr_list$base_stations <- c("p301_input/clim/Grove_lowprov_clim.base")

# ----- Parameters -----
input_preexisting_table <- NULL

# -----  Def File Parameters -----
input_def_list = NULL
# input_def_list <- list(
#   list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(0.1, 1, 4)),
# )

# ----- Standard (soil/subsurface) Parameters-----
# input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(2),
  k = c(2),
  m_v = c(2),
  k_v = c(2),
  pa = c(1),
  po = c(1),
  gw1 = c(0.2),
  gw2 = c(0.2)
)

# input_standard_par_list <- list(
#   m = c(2, 2, n_sim),
#   k = c(2, 2, n_sim),
#   m_v = c(2, 2, n_sim),
#   k_v = c(2, 2, n_sim),
#   pa = c(1, 1, n_sim),
#   po = c(1, 1, n_sim),
#   gw1 = c(0.2, 0.2, n_sim),
#   gw2 = c(0.2, 0.2, n_sim)
# )
# ----- Make Climate Basestation -----
#input_clim_base_list <- NULL

input_clim_base_list = clim_auto(base_station_id = 101,
                                 x_coordinate = 100.0,
                                 y_coordinate = 100.0,
                                 z_coordinate = 1748,
                                 effective_lai = 3.5,
                                 screen_height = 2,
                                 daily_prefix = "p301_input/clim/Grove_lowprov_clim")


# ----- Make Dated Sequence -----
# Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
input_dated_seq_list <- NULL
# input_dated_seq_list <- list()
# input_dated_seq_list[[1]] <- data.frame(name = "lowProv",type = "pspread",year = 1941,month = 10,day = 1,hour = 1,value = 0.1,stringsAsFactors = FALSE)
# input_dated_seq_list[[2]] <- data.frame(name = "lowProv",type = "pspread",year = 1941,month = 10,day = 1,hour = 1,value = 0.2,stringsAsFactors = FALSE)

# ----- Make Tec File -----
#input_tec_data <- NULL
input_tec_data = input_tec(NULL, start_end = start_end)
# input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
# input_tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
# input_tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)
# input_tec_data[3,] <- data.frame(1947, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)

# ----- Output -----
output_method = NULL
# ----- Output - Awks -----
# Data frame containing variable of interest, location/name of awk file (relative to output file location),
# and the location/name of rhessys output file with variable of interest.
output_variables = NULL
# output_variables <- data.frame(variable=character(),awk_path=character(),out_file=character(),stringsAsFactors=FALSE)
# output_variables[1,] <- data.frame("lai", "awks/output_var_bd_lai.awk","patch_sim_basin.daily",stringsAsFactors=FALSE)
# ----- Output - R -----
# Output variables for R version
# output_variables <- data.frame(out_type=character(), variable=character(), stringsAsFactors=FALSE)
# output_variables[1,] <- data.frame("bd", "lai", stringsAsFactors=FALSE)

# ----- Run RHESSys -----

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


