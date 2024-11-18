#!/usr/bin/env bash
# script to fetch IFS data from the mars archive for a given date and cycle
# Note that the tot prec  fied is multiplied by 1000 to make it in the same 
# units as the dini model. The units are not changed, hence they are not correct anymore

init=00
date=20240530

class="od"
stream="oper"
time=$init
expver="1"
area="65/5/50/17"
type="fc"
levtype="sfc"
param="228"

#for step in $(seq -w 00 3 21);
for step in $(seq -w 01 06);
do
#NOTE: requesting reg lat lon grid with the Grid settings
com="date=$date,expver=$expver,class=$class,stream=$stream,area=$area,type=$type,levtype=$levtype,param=$param,time=$time,step=$step,Grid=0.25/0.25"
dump=IFS_${date}${init}_${step}_TP.grib
mars << eof
retrieve, $com,fieldset=tp
compute, formula="1000*tp",
target="$dump"
eof
done
