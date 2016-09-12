# Functions for calibrating RHESSys
# Ryan Bart September 2016

# ---------------------------------------------------------------------
#

calibration_output_processing = function(){

  system("pwd;
         pwd")

  make_calibration_dir = sprintf("mkdir %s", paste(output_file,"/allsim",sep=""))
  system(make_calibration_dir)

  mkdir ../out/cal4/allsim
  echo   > ../out/cal4/allsim/par1
  echo   > ../out/cal4/allsim/lai1
  echo   > ../out/cal4/allsim/lai_c1
  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/change.statevar.cf.awk par=0.500000 < ../worldfiles/world.p301_patch_2canopy > ../worldfiles/world.tmp1
  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/change.statevar.gf.awk par=0.000000 < ../worldfiles/world.tmp1 > ../worldfiles/world.tmp2
  rhessys5.20.fire_off -t ../tecfiles/tec.p301_sim -w ../worldfiles/world.tmp2 -whdr ../worldfiles/world.p301_patch_2canopy.hdr   -r ../flowtables/flow.patch  -st 1941 10 1 1 -ed 2000 10 1 1 -pre ../out/cal4/sims/sen1   -s 1.792761 1.566492 -sv 1.792761 1.566492 -svalt 7.896941 1.179359 -gw 0.168035 0.178753 -b -c -p -g
  cd ../out/cal4/allsim
  \rm tpar1
  echo 1.792761 1.566492 7.896941 1.179359 0.168035 0.178753 0.500000 0.000000 > tpar1
  cat par1 tpar1 > newpar1
  mv newpar1 par1
  \rm tlai1
  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/extlai.awk < ../sims/sen1_basin.daily > tlai1
  paste lai1 tlai1 > newlai1
  mv newlai1 lai1
  \rm tlai_c1
  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/extlai_c.awk < ../sims/sen1_stratum.daily > tlai_c1
  paste lai_c1 tlai_c1 > newlai_c1
  mv newlai_c1 lai_c1
  cd ../../../scripts

}

# ---------------------------------------------------------------------
#

calibrate_rhessys = function(m, k, m_v, k_v, pa, po, gw1, gw2){

  for (aa in seq_along(m)){
    for (bb in seq_along(k)){
      for (cc in seq_along(m_v)){
        for (dd in seq_along(k_v)){
          for (ee in seq_along(pa)){
            for (ff in seq_along(po)){
              for (gg in seq_along(gw1)){
                for (hh in seq_along(gw2)){
                  run_rhessys(rhessys_version = rhessys_version, tec_file = tec_file,
                              world_file = world_file, world_hdr_file = world_hdr_file,
                              flow_file = flow_file, start_date = start_date,
                              end_date = end_date, output_file = output_file,
                              output_filename = output_filename,
                              m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po,
                              gw1 = gw1, gw2 = gw2, comm_line_options = comm_line_options)

                  # Calibration output processing function


                }
              }
            }
          }
        }
      }
    }
  }


}








