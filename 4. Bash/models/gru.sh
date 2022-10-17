python "3. Python/1_Forecast.py" <<EOF
$1
EOF

for (( i=1; i<=$3; i++ ))
do

python "3. Python/2_MM_sampling.py" <<EOF
$1
$2
gru_sessions_train_$i.csv
y
l
y
GRU
EOF

python "3. Python/2_MM_sampling.py" <<EOF
$1
$2
gru_sessions_test_$i.csv
n
l
y
GRU
EOF

Rscript "2. R/5_Error_Analysis.R" GRU+LMM_$i $1 $2
# Rscript "2. R/5_Error_Analysis.R" GRU+GMM_$i $1 $2

done

Rscript "2. R/6_gather_forecasts.R" GRU+LMM $1 $2
Rscript "2. R/6_gather_forecasts.R" GRU+LMM_error $1 $2

# Rscript "2. R/6_gather_forecasts.R" GRU+GMM $1 $2
# Rscript "2. R/6_gather_forecasts.R" GRU+GMM_error $1 $2