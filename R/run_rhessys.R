# Function for running a single RHESSys run
# Ryan Bart September 2016


# ---------------------------------------------------------------------
# Create Monte Carlo parameter set

parameters_mc = function(m, k, m_v, k_v, pa, po, gw1, gw2, ...){
  parameters = cbind(m, k, m_v, k_v, pa, po, gw1, gw2, ...)
  return(parameters)
}

# ---------------------------------------------------------------------
# Created gridded parameter sets

parameters_grid = function(m, k, m_v, k_v, pa, po, gw1, gw2, ...){
  parameters = expand.grid(m=m, k=k, m_v=m_v, k_v=k_v, pa=pa, po=po, gw1=gw1, gw2=gw2, ...)
  return(parameters)
}

# ---------------------------------------------------------------------
#

run_awk = function(par_value, awk_file, input_file, output_file){

  tmp = sprintf("awk -f %s par=%f < %s > %s", awk_file, par_value, input_file, output_file)
  system(tmp, ignore.stderr = T)
}

# ---------------------------------------------------------------------
# Call single RHESSys run

run_rhessys = function(rhessys_version, tec_file, world_file, world_hdr_file,
                       flow_file, start_date, end_date, output_file,
                       output_filename, comm_line_options,
                       m, k, m_v, k_v, pa, po, gw1, gw2){

  tmp = sprintf("%s -t %s -w %s -whdr %s -r %s -st %s -ed %s -pre %s -s %f %f
                -sv %f %f -svalt %f %f -gw %f %f %s",
                rhessys_version, tec_file, world_file, world_hdr_file, flow_file,
                start_date, end_date, paste(output_file,"/",output_filename,sep=""),
                m, k, m_v, k_v, pa, po, gw1, gw2, comm_line_options)

  system(tmp, ignore.stderr = T)
}

# ---------------------------------------------------------------------
#

run_rhessys_simulation = function(rhessys_version, tec_file, world_file, world_hdr_file,
                                  flow_file, start_date, end_date, output_file,
                                  output_filename, comm_line_options, parameter_type,
                                  m, k, m_v, k_v, pa, po, gw1, gw2, awk_filenames, ...){

  # Processes parameters
  if (parameter_type == "MC"){
    parameters = parameters_mc(m, k, m_v, k_v, pa, po, gw1, gw2, ...)
  } else {
    parameters = parameters_grid(m, k, m_v, k_v, pa, po, gw1, gw2, ...)
  }
  print(parameters)

  # Need 'for' loop instead of apply function since calls to RHESSys cannot be done simultaneously
  for (aa in seq_along(parameters[,1])){

    # Evaluate each awk
    for (bb in seq_along(awk_filenames)){
      print(parameters[aa, 8+bb])
      print(awk_filenames[[bb]][[1]])
      print(awk_filenames[[bb]][[2]])
      print(awk_filenames[[bb]][[3]])
      run_awk(parameters[aa, 8+bb], awk_file = awk_filenames[[bb]][[1]],
              input_file = awk_filenames[[bb]][[2]], output_file = awk_filenames[[bb]][[3]])
    }

    run_rhessys(rhessys_version = rhessys_version, tec_file = tec_file,
                world_file = world_file, world_hdr_file = world_hdr_file,
                flow_file = flow_file, start_date = start_date,
                end_date = end_date, output_file = output_file,
                output_filename = output_filename, comm_line_options = comm_line_options,
                m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, gw1 = gw1, gw2 = gw2)
  }
}



# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------
# Extra code for now


#m = seq(1:3)
#k = seq(1:3)
#m_v = 1.792761
#k_v = 1.566492
#pa = 7.896941
#po = 1.179359
#gw1 = 0.168035
#gw2 = 0.178753
#awk1 = list(c(4,5,6), "test1", "test2", "test3")
#awk2 = list(runif(min=1.566492, max=11.566492, n=12), "test11", "test12", "test13")


#get_awk_parameters = function(m, k, m_v, k_v, pa, po, gw1, gw2, ...){
#  awk_data = list(...)

#  awk_par = list()
#  f = function(x,y){ x[[1]] }
#  awk_par = lapply(awk_data, f)

#  return(awk_par)
#}

#awk_par = get_awk_parameters(m, k, m_v, k_v, pa, po, gw1, gw2, awk1, awk2)
#print(awk_par)

####

#get_awk_filenames = function(...){

#  awk_data = list(...)
#  awk_l = length(awk_data)

#  awk_files = list()
#  f = function(x,y){ x[c(2,3,4)] }
#  awk_files = lapply(awk_data, f)

#  return(awk_files)
#}

#awk_files = get_awk_filenames(awk1, awk2)
#print(awk_files)















