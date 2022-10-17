# !/bin/bash

for (( i=1; i<=$3; i++ ))
do

python "3. Python/2_CMM_sampling.py" <<EOF
$1
$2
forecast_smooth_train_$i.csv
y
l
y
Smooth
EOF

python "3. Python/2_CMM_sampling.py" <<EOF
$1
$2
forecast_smooth_test_$i.csv
n
l
y
Smooth
EOF

Rscript "2. R/5_Error_Analysis.R" NHPP_Smooth+CLMM_$i $1 $2

done


Rscript "2. R/6_gather_forecasts.R" NHPP_Smooth+CLMM $1 $2
Rscript "2. R/6_gather_forecasts.R" NHPP_Smooth+CLMM_error $1 $2