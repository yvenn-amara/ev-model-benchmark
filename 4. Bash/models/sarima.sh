# !/bin/bash

for (( i=1; i<=$3; i++ ))
do

python "3. Python/2_MM_sampling.py" <<EOF
$1
$2
pred_sessions_train_$i.csv
y
l
y
SARIMA
EOF

python "3. Python/2_MM_sampling.py" <<EOF
$1
$2
pred_sessions_test_$i.csv
n
l
y
SARIMA
EOF

Rscript "2. R/5_Error_Analysis.R" SARIMA+LMM_$i $1 $2
# Rscript "2. R/5_Error_Analysis.R" SARIMA+GMM_$i $1 $2

done

Rscript "2. R/6_gather_forecasts.R" SARIMA+LMM $1 $2
Rscript "2. R/6_gather_forecasts.R" SARIMA+LMM_error $1 $2
# Rscript "2. R/6_gather_forecasts.R" SARIMA+GMM $1 $2
# Rscript "2. R/6_gather_forecasts.R" SARIMA+GMM_error $1 $2