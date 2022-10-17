# for i in acn boulder palo_alto sap domestics dundee paris perth; do
for i in domestics dundee paris perth; do
echo "==================================="
echo "==================================="
echo "==================================="
echo "===============" $i "==============="
echo "==================================="
echo "==================================="
echo "==================================="

# Rscript "2. R/models/nhpp_smoothing_grid_search.R" $i
# Rscript "2. R/models/nhpp_rf_grid_search.R" $i
Rscript "2. R/models/nhpp_gam_grid_search.R" $i
# Rscript "2. R/models/sarima_valid.R" $i
# python "3. Python/gru_grid_search.py" $i

done
