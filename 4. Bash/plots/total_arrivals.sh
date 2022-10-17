# !/bin/bash

for i in acn boulder domestics dundee palo_alto paris perth sap; do
Rscript "2. R/0_select_dates.R" $i
done