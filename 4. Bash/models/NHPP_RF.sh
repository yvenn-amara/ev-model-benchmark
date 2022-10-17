# !/bin/bash

for (( i=1; i<=$3; i++ ))
do


python "3. Python/2_CMM_sampling.py" <<EOF
$1
$2
forecast_rf_train_$i.csv
y
l
y
RF
EOF

python "3. Python/2_CMM_sampling.py" <<EOF
$1
$2
forecast_rf_test_$i.csv
n
l
y
RF
EOF

Rscript "2. R/5_Error_Analysis.R" NHPP_RF+CLMM_$i $1 $2
# Rscript "2. R/5_Error_Analysis.R" NHPP_RF+CGMM_$i $1 $2

done

Rscript "2. R/6_gather_forecasts.R" NHPP_RF+CLMM $1 $2
Rscript "2. R/6_gather_forecasts.R" NHPP_RF+CLMM_error $1 $2

# Rscript "2. R/6_gather_forecasts.R" NHPP_RF+CGMM $1 $2
# Rscript "2. R/6_gather_forecasts.R" NHPP_RF+CGMM_error $1 $2