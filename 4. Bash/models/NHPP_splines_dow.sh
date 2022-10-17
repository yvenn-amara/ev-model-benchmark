# !/bin/bash

for (( i=1; i<=$3; i++ ))
do

python "3. Python/2_CMM_sampling.py" <<EOF
$1
$2
forecast_nhpp_splines_dow_train_$i.csv
y
l
y
splines_dow
EOF

python "3. Python/2_CMM_sampling.py" <<EOF
$1
$2
forecast_nhpp_splines_dow_test_$i.csv
n
l
y
splines_dow
EOF

Rscript "2. R/5_Error_Analysis.R" NHPP_splines_dow+CLMM_$i $1 $2
# Rscript "2. R/5_Error_Analysis.R" NHPP_splines_dow+CGMM_$i $1 $2

done

Rscript "2. R/6_gather_forecasts.R" NHPP_splines_dow+CLMM $1 $2
Rscript "2. R/6_gather_forecasts.R" NHPP_splines_dow+CLMM_error $1 $2

# Rscript "2. R/6_gather_forecasts.R" NHPP_splines_dow+CGMM $1 $2
# Rscript "2. R/6_gather_forecasts.R" NHPP_splines_dow+CGMM_error $1 $2