import pickle
import pandas as pd
from Utility import *
from os import listdir, path

cwd0 = '2. R/1. Outputs/'
datasets = [f for f in listdir(cwd0)]

dataset=input("Datasets Available: " + ', '.join(datasets) + "\nSelect Dataset: ") +"/"
cwd1 = '2. R/1. Outputs/' + dataset + 'sarima/'
cwd3 = '3. Python/1. Outputs/' + dataset

duration = input("Choose Duration Type (Park.Duration/Charge.Duration):")

subdatasets = [f for f in listdir(cwd1)]
subdata = input("Subdata Available: " + ', '.join(subdatasets) + "\nSelect Subdataset: ")
signal = input("Is it a train signal ? (y/n")
number = subdata[-5]
print(number)

sessions = pd.read_csv(cwd1 + subdata)
sessions['Start_date'] = pd.to_datetime(sessions['Start_date'])
# print(sessions)

gmm_type = input("Would you like a Gaussian or Log-normal mixture (g/l):")
# gmm_type = argv[2]
gmm_name = "best_" + gmm_type + "mm_"+duration+".pkl"

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


# Simulation
print("Simulating triplets based on SARIMA expected daily sessions and Mixture fit")

newsim = input("New ARIMA+Mixture Simulation (y/n):")
# newsim = argv[3]

if newsim=='y':
    if signal == 'y':
        arima_gmm_name = "sarima+"+gmm_type+"mm_"+"train_"+str(number) + ".pkl"#input("ARIMA+Mixture Simulation Name:") + "_"+str(number)
    elif signal == 'n':    
        arima_gmm_name = "sarima+"+gmm_type+"mm_"+"test_"+str(number) + ".pkl"#input("ARIMA+Mixture Simulation Name:") + "_"+str(number)
    simu_prep = []
    for i in range(len(sessions)):
        if loglink:
            first = pd.DataFrame(np.exp(best_gmm.sample(sessions.pred.iloc[i])[0]))
        else:
            first = pd.DataFrame(best_gmm.sample(sessions.pred.iloc[i])[0])
        first.columns = ["Arrival",duration,"Energy"]
        first['Start'] = [sessions.iloc[i]['Start_date'] + timedelta(hours=e) for e in first.Arrival]
        first['Start'] = first['Start'].dt.round('1min')
        simu_prep.append(first)

    simsim = pd.concat(simu_prep)
    raw_rows = len(simsim)
    simsim = simsim[(simsim[duration] > 0) & (simsim['Energy']>0)]# Remove impossible sessions (negative charge/park duration or negative energy)
    print("Impossible sessions discarded: ", (raw_rows-len(simsim))/raw_rows, "%")
    simu_list = [interpol(simsim.iloc[i],duration) for i in range(len(simsim))]
    with open(cwd3+"simulations/arima+gmm/"+arima_gmm_name, "wb") as fp:
        pickle.dump(simu_list, fp)
elif newsim=='n':
    arima_gmm_simulations = [f for f in listdir(cwd3+"simulations/arima+gmm")]
    print("Existing ARIMA+GMM Simulations: " + ', '.join(arima_gmm_simulations))
    arima_gmm_name = input("ARIMA+GMM Simulation Name: ")
    with open(cwd3+"simulations/arima+gmm/"+arima_gmm_name, "rb") as fp:
        simu_list = pickle.load(fp)


# Reconstructing load curve

print("Reconstructing and exporting observed and modelled curves")
start_date = min(sessions['Start_date']).replace(hour=0, minute=0, second=0, microsecond=0)
end_date = max(sessions['Start_date']).replace(hour=23, minute=59, second=0, microsecond=0)
# print(start_date)
# print(end_date)
model_name = input() + '+'+gmm_type.upper()+'MM' + "_" + str(number)

reconstructed = get_curves([simu_list],[model_name],start_date=start_date,end_date=end_date)
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