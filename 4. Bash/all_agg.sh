for i in acn boulder palo_alto sap; do for j in Charge.Duration Park.Duration; do
echo "===================================" $i $j "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
Rscript "2. R/8_Persistence_Baseline.R" $i $j
Rscript "2. R/7_Aggregation_v2.R" $i $j
done
done

for i in domestics dundee paris perth; do
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "==================================="
echo "===================================" $i "Park.Duration ==================================="
Rscript "2. R/8_Persistence_Baseline.R" $i Park.Duration
Rscript "2. R/7_Aggregation_v2.R" $i Park.Duration
done
