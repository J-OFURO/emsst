#!/bin/csh
#
# 01_make_emsst_VER_YYYY1_YYYY2_MM.csh
#----------------------------------------------------------------------- 
set ver=$argv[1]
set yr1=$argv[2]
set yr2=$argv[3]
set mon=$argv[4]  # end month

if ($ver == "V1.0.0") then
 set code=em_sst_v1.0.0.f
else if ($ver == "V1.1.0") then
 set code=em_sst_v1.1.0.f
endif

if -e out_emsst.out rm out_emsst.out
ifort -o out_emsst.out  $code
./out_emsst.out $yr1 $yr2 $mon


