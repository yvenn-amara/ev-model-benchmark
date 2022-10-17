import pickle
import pandas as pd
from Utility import *
from os import listdir, path


cwd0 = '2. R/1. Outputs/'
datasets = [f for f in listdir(cwd0)]

dataset=input("Datasets Available: " + ', '.join(datasets) + "\nSelect Dataset: ") +"/"
cwd2 = '2. R/1. Outputs/' + dataset + 'nhp/'
cwd3 = '3. Python/1. Outputs/' + dataset

duration = input("Choose Duration Type (Park.Duration/Charge.Duration):")

subdatasets = [f for f in listdir(cwd2)]
subdata = input("Subdata Available: " + ', '.join(subdatasets) + "\nSelect Subdataset: ")
signal = input("Is it a train signal ? (y/n")
number = subdata[-5]

gmm_type = input("Would you like a Gaussian or Log-normal mixture (g/l):")
# gmm_type = argv[2]
gmm_name = "best_" + gmm_type + "mm_" + duration + ".pkl"

if gmm_type=="g":
    loglink=False
elif gmm_type=='l':
    loglink = True

if path.exists(cwd3+gmm_name):
    print("Recovering best mixture fit")
    with open(cwd3+gmm_name, "rb") as fp:
        best_gmm = pickle.load(fp)
else:
    print("No existing mixture model")
    exit

## 1st Step - Import Simulated Arrivals
print("Simulating triplets based on NHPP simulation and Conditional Mixture fit")
newsim = input("New NHPP+Mixture Simulation (y/n):")

if newsim=='y':
    nhp_simulations = [f for f in listdir(cwd2)]

    NHPsimu = pd.read_csv(cwd2 + subdata)
    NHPsimu['Start'] = pd.to_datetime(NHPsimu['Start'])
    NHPsimu = NHPsimu.loc[NHPsimu.Start.dt.weekday <= 4].reset_index(drop=True) ##### ATTENTION: Only if weekends are not included in the analysis

    ## 2nd Step - Simulate conditional gaussian mixture
    if signal == 'y':
        nhp_cgmm_name = "nhpp+c"+gmm_type+"mm_"+"train_"+str(number) + ".pkl"#input("ARIMA+Mixture Simulation Name:") + "_"+str(number)
    elif signal == 'n':    
        nhp_cgmm_name = "nhpp+c"+gmm_type+"mm_"+"test_"+str(number) + ".pkl"#input("ARIMA+Mixture Simulation Name:") + "_"+str(number)
   
    a = NHP_to_triple(NHPsimu,best_gmm,duration, type='mean', loglink=loglink)
    raw_rows=len(a)
    a = a[(a[duration] > 0) & (a['Energy']>0)]# Remove impossible sessions (negative charge.duration or negative energy)
    print("Impossible sessions discarded: ", (raw_rows-len(a))/raw_rows, "%")

    print("Simulating triplets based on NHP simulation and CGMM fit")
    NHP_list = [interpol(a.iloc[i],duration) for i in range(len(a))]
    with open(cwd3+"simulations/nhp+cgmm/"+nhp_cgmm_name, "wb") as fp:
        pickle.dump(NHP_list, fp)
elif newsim=='n':
    nhp_cgmm_simulations = [f for f in listdir(cwd3+"simulations/nhp+cgmm")]
    print("Existing NHP+CGMM Simulations: " + ', '.join(nhp_cgmm_simulations))
    nhp_cgmm_name = input("NHP+CGMM Simulation Name: ")
    with open(cwd3+"simulations/nhp+cgmm/"+nhp_cgmm_name, "rb") as fp:
        NHP_list = pickle.load(fp)


# Reconstructing load curve

print("Reconstructing and exporting observed and modelled curves")
start_date = min(NHPsimu['Start']).replace(hour=0, minute=0, second=0, microsecond=0)
end_date = max(NHPsimu['Start']).replace(hour=23, minute=59, second=0, microsecond=0)
# print(start_date)
# print(end_date)
basis = input("Basis type: ")
model_name = 'NHPP_'+basis+'+C'+gmm_type.upper()+'MM' + "_" + str(number)

reconstructed = get_curves([NHP_list],[model_name],start_date=start_date,end_date=end_date)
#reconstructed.loc[reconstructed.Start.dt.weekday > 4, "Power"] = 0 ##### ATTENTION: Only if weekends are not included in the analysis

if duration=="Charge.Duration":
    if signal == 'y':
        pd.DataFrame(reconstructed).to_csv(cwd3  + "train_signals/" + model_name + '_load_curve.csv',index=False)
    elif signal == 'n':
        pd.DataFrame(reconstructed).to_csv(cwd3  + "test_signals/" + model_name + '_load_curve.csv',index=False)
elif duration=="Park.Duration":
    if signal == 'y':
        pd.DataFrame(reconstructed).to_csv(cwd3  + "train_signals/" + model_name + '_occupancy.csv',index=False)
    elif signal == 'n':
        pd.DataFrame(reconstructed).to_csv(cwd3  + "test_signals/" + model_name + '_occupancy.csv',index=False)