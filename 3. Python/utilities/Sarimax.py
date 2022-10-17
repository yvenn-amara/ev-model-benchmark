import pandas as pd
import numpy as np
from scipy.stats import norm
import statsmodels.api as sm
import matplotlib.pyplot as plt
from datetime import datetime
import requests
from io import BytesIO
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.stattools import adfuller
from pandas.tseries.offsets import DateOffset
import pickle

# Load Model
from Models.Models import *
model_name = 'Model_name'
def load_model(model_name):
    return

model = load_model(model_name) 

# Split Data into Training and Test sets
train_test = (70,30)

def split_data(train_test,data):
    N = round((0.7)*len(data))
    train_data = data[0:N]
    test_data = data[N+1:]
    return train_data,test_data

#train_data,test_data = split_data(train_test,data)

# Load Data
filename = 'palo_alto.csv'
def load_data(filename):
    location = 'Data/'+filename
    data = pd.read_csv(location)
    return data

data = load_data(filename)

data['just_date'] = pd.to_datetime(data['Start']).dt.date
data =data.groupby(['just_date']).size()
data = {'Date':list(data.index),
        'N':list(data.values)}
 
# Create DataFrame
raw = pd.DataFrame(data)
raw.index = pd.to_datetime(raw['Date'])
raw.drop(columns='Date',inplace=True)

#Restrict to Stationary part of graph in 2017
raw=raw['2017-01-01' :'2017-8-01']

decompose_data = seasonal_decompose(raw, model="additive")

seasonality=decompose_data.seasonal

## Applying Rolling Mean and Differencing
rolling_mean = raw.rolling(window = 35).mean()
raw['rolling_mean_diff'] = rolling_mean - rolling_mean.shift()

dftest = adfuller(raw['rolling_mean_diff'].dropna(), autolag = 'AIC')
print("1. ADF : ",dftest[0])
print("2. P-Value : ", dftest[1])
print("3. Num Of Lags : ", dftest[2])
print("4. Num Of Observations Used For ADF Regression and Critical Values Calculation :", dftest[3])
print("5. Critical Values :")
for key, val in dftest[4].items():
  print("\t",key, ": ", val)

model=sm.tsa.statespace.SARIMAX(raw['N'],order=(1, 1, 1),seasonal_order=(1,1,1,54))
results=model.fit()

#Save Model
with open('Sarmimax_model.pkl', 'wb') as file:
    pickle.dump(models, file)   


pred_date=[raw.index[-1]+ DateOffset(months=x)for x in range(0,24)]
pred_date=pd.DataFrame(index=pred_date[1:],columns=raw.columns)
raw_and_pred=pd.concat([raw,pred_date])

#Forecasting
raw_and_pred['forecast'] = results.predict(start = 200, end = 320, dynamic= True)  
raw_and_pred[['N', 'forecast']].plot(figsize=(12, 8))

#Generating data for predictions beyond the test set for user counts
prediction= results.predict(start=366,end=500,dynamic=True)

print(prediction)