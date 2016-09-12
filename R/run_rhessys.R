# Function for running a single RHESSys run
# Ryan Bart September 2016

# ---------------------------------------------------------------------
# Call single RHESSys run

run_rhessys = function(rhessys_version, tec_file, world_file, world_hdr_file,
                       flow_file, start_date, end_date, output_file,
                       output_filename, m, k, m_v, k_v, pa, po, gw1, gw2,
                       comm_line_options){

  tmp = sprintf("%s -t %s -w %s -whdr %s -r %s -st %s -ed %s -pre %s -s %f %f
                -sv %f %f -svalt %f %f -gw %f %f %s",
                rhessys_version, tec_file, world_file, world_hdr_file, flow_file,
                start_date, end_date, paste(output_file,"/",output_filename,sep=""),
                m, k, m_v, k_v, pa, po, gw1, gw2, comm_line_options)

  system(tmp, ignore.stderr = T)
}
