# Functions for calibrating RHESSys
# Ryan Bart September 2016

# ---------------------------------------------------------------------
#

#calibration_output_processing = function(){

#  system("pwd;
#         pwd")

#  make_calibration_dir = sprintf("mkdir %s", paste(output_file,"/allsim",sep=""))
#  system(make_calibration_dir)

#  mkdir ../out/cal4/allsim
#  echo   > ../out/cal4/allsim/par1
#  echo   > ../out/cal4/allsim/lai1
#  echo   > ../out/cal4/allsim/lai_c1
#  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/change.statevar.cf.awk par=0.500000 < ../worldfiles/world.p301_patch_2canopy > ../worldfiles/world.tmp1
#  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/change.statevar.gf.awk par=0.000000 < ../worldfiles/world.tmp1 > ../worldfiles/world.tmp2
#  rhessys5.20.fire_off -t ../tecfiles/tec.p301_sim -w ../worldfiles/world.tmp2 -whdr ../worldfiles/world.p301_patch_2canopy.hdr   -r ../flowtables/flow.patch  -st 1941 10 1 1 -ed 2000 10 1 1 -pre ../out/cal4/sims/sen1   -s 1.792761 1.566492 -sv 1.792761 1.566492 -svalt 7.896941 1.179359 -gw 0.168035 0.178753 -b -c -p -g
#  cd ../out/cal4/allsim
#  \rm tpar1
#  echo 1.792761 1.566492 7.896941 1.179359 0.168035 0.178753 0.500000 0.000000 > tpar1
#  cat par1 tpar1 > newpar1
#  mv newpar1 par1
#  \rm tlai1
#  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/extlai.awk < ../sims/sen1_basin.daily > tlai1
#  paste lai1 tlai1 > newlai1
#  mv newlai1 lai1
#  \rm tlai_c1
#  awk -f /users/ryanrbart/work_dir/modeling/rhessys/projects/fire_effects_model/awks/extlai_c.awk < ../sims/sen1_stratum.daily > tlai_c1
#  paste lai_c1 tlai_c1 > newlai_c1
#  mv newlai_c1 lai_c1
#  cd ../../../scripts

#}














