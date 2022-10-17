# !/bin/bash

Rscript "2. R/1_Forecast.R" $1

# python "3. Python/3_Reconstruction.py" <<EOF
# $1
# $2
# EOF

# python "3. Python/1_MM_fit.py" <<EOF
# $1
# $2
# EOF

# bash "4. Bash/models/sarima.sh" $1 $2 $3
# bash "4. Bash/models/gru.sh" $1 $2 $3
# bash "4. Bash/models/NHPP_splines_dow.sh" $1 $2 $3
# bash "4. Bash/models/NHPP_RF.sh" $1 $2 $3
# bash "4. Bash/models/NHPP_smooth.sh" $1 $2 $3

# Rscript "2. R/8_Persistence_Baseline.R" $1 $2
# Rscript "2. R/7_Aggregation_v2.R" $1 $2

# Rscript "2. R/9_Block_bootstrap.R" $1 $2

# python "3. Python/4_Model_Analysis.py" <<EOF
# $1
# $2
# EOF