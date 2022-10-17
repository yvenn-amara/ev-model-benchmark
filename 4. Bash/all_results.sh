
for i in acn boulder palo_alto sap; do for j in Charge.Duration Park.Duration; do
echo "===================================" $i $j "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
Rscript "2. R/9_Block_bootstrap.R" $i $j $1

python "3. Python/4_Model_Analysis.py" <<EOF
$i
$j
EOF
done
done

for i in domestics dundee paris perth
do
echo "===================================" $i "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
Rscript "2. R/9_Block_bootstrap.R" $i Park.Duration $1


python "3. Python/4_Model_Analysis.py" <<EOF
$i
Park.Duration
EOF
done

for j in Charge.Duration Park.Duration; do
# python "3. Python/5_Consolidated_Metrics.py" <<EOF
python "3. Python/5_Consolidated_Metrics_v2.py" <<EOF
$j
EOF
done