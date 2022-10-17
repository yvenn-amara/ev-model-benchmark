#### Reconstructing load curve from charging sessions
import pandas as pd 
from Utility import *
from os import listdir

cwd0 = '2. R/1. Outputs/'
datasets = [f for f in listdir(cwd0)]
dataset=input("Datasets Available: " + ', '.join(datasets) + "\nSelect Dataset: ") +"/"
duration = input("Choose Duration Type (Park.Duration/Charge.Duration):")

#### ALL
df = pd.read_csv("2. R/1. Outputs/"+dataset+"sarima/cleaned_all.csv")
df['Start'] = pd.to_datetime(df['Start'])
df['Start']=df['Start'].dt.round('1 min')  

# Interpolation
print("Interpolation step")
data_list = [interpol(df.iloc[i],duration) for i in range(len(df))]

# Reconstruction
print("Reconstruction step")
start_date = min(df['Start']).replace(hour=0, minute=0, second=0, microsecond=0)
end_date = max(df['Start']).replace(hour=23, minute=59, second=0, microsecond=0)
rec = reconstruct(data_list,start_date,end_date)

# rec['curve'] = 'NAF-SARIMA' # Specify model name
rec['curve'] = 'observed' # Specify model name

rec['date'] = rec['Start'].dt.date

# rec.to_csv('1. Outputs/palo_alto/3_sarima+naf.csv',index=False) # Specify path and filename
rec.to_csv('3. Python/1. Outputs/'+ dataset +'observed_all_'+duration+'.csv',index=False) # Specify path and filename


#### TEST
df = pd.read_csv("2. R/1. Outputs/"+dataset+"sarima/cleaned_test.csv")
df['Start'] = pd.to_datetime(df['Start'])
df['Start']=df['Start'].dt.round('1 min')  

# Interpolation
print("Interpolation step")
data_list = [interpol(df.iloc[i],duration) for i in range(len(df))]

# Reconstruction
print("Reconstruction step")
start_date = min(df['Start']).replace(hour=0, minute=0, second=0, microsecond=0)
end_date = max(df['Start']).replace(hour=23, minute=59, second=0, microsecond=0)
rec = reconstruct(data_list,start_date,end_date)

# rec['curve'] = 'NAF-SARIMA' # Specify model name
rec['curve'] = 'observed' # Specify model name

rec['date'] = rec['Start'].dt.date

# rec.to_csv('1. Outputs/palo_alto/3_sarima+naf.csv',index=False) # Specify path and filename
rec.to_csv('3. Python/1. Outputs/'+ dataset +'observed_test_'+duration+'.csv',index=False) # Specify path and filename