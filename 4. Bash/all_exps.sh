# !/bin/bash

for i in acn; do for j in Charge.Duration Park.Duration; do
bash "4. Bash/full_model.sh" $i $j 4
done
done


# for i in acn boulder palo_alto sap; do for j in Charge.Duration Park.Duration; do
# bash "4. Bash/full_model.sh" $i $j 4
# done
# done

# for i in domestics dundee perth; do
# bash "4. Bash/full_model.sh" $i Park.Duration 4
# done

# bash "4. Bash/models/gru.sh" paris Park.Duration 2
# "4. Bash/full_model.sh" paris Park.Duration 2

# for i in Charge.Duration Park.Duration; 
# do
# python "3. Python/5_Consolidated_Metrics_v2.py" <<EOF
# $i
# EOF
# done