# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
from datetime import datetime
from datetime import timedelta
from gmr import GMM
from sklearn.metrics import mean_absolute_error as mae
from sklearn.metrics import mean_squared_error as mse
import seaborn as sns
import plotly.express as px

def rmse(y_true,y_pred):
    return(np.sqrt(mse(y_true,y_pred)))

def daterange(date1,date2):
    date1 = date1 - timedelta(seconds=date1.second)
    #date2 = date2 + timedelta(seconds=60-date2.second)
    date2 = date2 - timedelta(seconds=date1.second)
    dates = []
    for n in range(int ((date2-date1).total_seconds()/60+1) ):
        dates.append(date1 + timedelta(minutes=n))
    return(pd.DataFrame({'Start':dates}))

def interpol(row_df,duration):
    if duration=="Charge.Duration":
        dd = daterange(row_df.Start,row_df.Start+timedelta(minutes=int(row_df['Charge.Duration'])))
        dd['Power'] = row_df['Energy']/(row_df['Charge.Duration']/60)
        return(dd[['Start','Power']])
    elif duration=="Park.Duration":
        dd = daterange(row_df.Start,row_df.Start+timedelta(minutes=int(row_df['Park.Duration'])))
        dd['Occupancy'] = 1
        return(dd[['Start','Occupancy']])

# def interpol(row_df,duration):
#     if duration=="Charge.Duration":
#         dd = daterange(row_df.Start.iloc[0],row_df.Start.iloc[0]+timedelta(minutes=int(row_df['Charge.Duration'].iloc[0])))
#         dd['Power'] = row_df['Energy'].iloc[0]/(row_df['Charge.Duration'].iloc[0]/60)
#         return(dd[['Start','Power']])
#     elif duration=="Park.Duration":
#         dd = pd.merge(daterange(row_df.Start.iloc[0],row_df.Start.iloc[0]+timedelta(minutes=int(row_df['Park.Duration'].iloc[0]))),row_df, how='left', on='Start')
#         dd['Occupancy'] = 1
#         return(dd[['Start','Occupancy']])
    
def fill_missing_dates(dt,start_date,end_date):
    #dd = pd.merge(left=daterange(datetime.combine(min(dt.Start), datetime.min.time()),datetime.combine(max(dt.Start), datetime.min.time())),right=dt,how='left',on='Start')
    dd = pd.merge(left=daterange(start_date,end_date),right=dt,how='left',on='Start')
    return(dd.fillna(0))
    
def reconstruct(dt,start_date,end_date):
    rec = pd.concat(dt)
    rec2 = rec.groupby('Start').sum().reset_index()
    rec3 = fill_missing_dates(rec2,start_date,end_date)
    return(rec3)

####
    
def NHP_to_triple(dt, gmm_sklearn, duration, type="sample", loglink=False, logeps=0.000000001):
    arrivals = dt['Start'].dt.strftime("%H").astype(int) + dt['Start'].dt.strftime("%M").astype(int)/60
    log_arrivals = np.log(arrivals + logeps) # To avoid dividing by 0
    gmm = GMM(n_components=len(gmm_sklearn.means_), priors=gmm_sklearn.weights_, 
          means=gmm_sklearn.means_,
          covariances = gmm_sklearn.covariances_)
          #covariances=np.array([np.diag(c) for c in best_gmm.covariances_]))
    if type=="sample":
        if loglink:
            triple = pd.DataFrame([np.exp(gmm.condition(np.array([0]), np.array([a])).sample(1)[0]) for a in log_arrivals] )
        else:
            triple = pd.DataFrame([gmm.condition(np.array([0]), np.array([a])).sample(1)[0] for a in arrivals] )
    elif type=="mean":
        triple = []
        if loglink:
            for a in log_arrivals:
                conditional = gmm.condition(np.array([0]), np.array([a]))
                shape = np.array(conditional.means).shape
                #print(shape)
                temp = np.zeros(shape)
                for i in range(shape[0]):
                    for j in range(shape[1]):
                        temp[i,j] = conditional.means[i,j] + conditional.covariances[i,j,j]/2
                triple.append(np.dot(conditional.priors, np.exp(temp)))
        else:
            for a in arrivals:
                conditional = gmm.condition(np.array([0]), np.array([a]))
                triple.append(np.dot(conditional.priors, conditional.means))
        triple = pd.DataFrame(triple)
    triple.columns = [duration,'Energy']
    triple['Arrival'] = arrivals
    triple['Start'] = dt['Start']
    return(triple)
    
####

def get_curves(array_list,array_names,start_date,end_date): 

    rec_list = [reconstruct(dt,start_date,end_date) for dt in array_list]
    for i in range(len(rec_list)):
        rec_list[i]['curve'] = array_names[i]
    final = pd.concat(rec_list)
    final['date'] = final['Start'].dt.date
    return(final)
    
####
    
# def global_performance(reconstructed,interactive=False,all=False):
#     # if all:
#     #     reconstructed = reconstructed[(reconstructed['date'] != min(reconstructed['date'])) & (reconstructed['date'] != max(reconstructed['date']))]
#     #     reconstructed = reconstructed[(reconstructed['date'] != min(reconstructed['date'])) & (reconstructed['date'] != max(reconstructed['date']))]
#     benchmark_mae = round(mae(reconstructed[reconstructed['curve']=='observed'].Power,reconstructed[reconstructed['curve']=='Sim-ARIMA+GMM'].Power),2)
#     challenger_mae = round(mae(reconstructed[reconstructed['curve']=='observed'].Power,reconstructed[reconstructed['curve']=='Sim-NHP+CGMM'].Power),2)
#     benchmark_rmse = round(rmse(reconstructed[reconstructed['curve']=='observed'].Power,reconstructed[reconstructed['curve']=='Sim-ARIMA+GMM'].Power),2)
#     challenger_rmse = round(rmse(reconstructed[reconstructed['curve']=='observed'].Power,reconstructed[reconstructed['curve']=='Sim-NHP+CGMM'].Power),2)
#     print("Sim-ARIMA+GMM:",benchmark_mae,benchmark_rmse)
#     print("Sim-NHP+CGMM:",challenger_mae,challenger_rmse)
#     if interactive==False:
#         return(sns.lineplot(data=reconstructed,x='Start',y='Power',hue='curve').set_title("Sim-ARIMA+MM: " + "MAE " + str(benchmark_mae) + ", RMSE " + str(benchmark_rmse) + ", Sim-NHP+CMM:" + "MAE " + str(challenger_mae) + ", RMSE " + str(challenger_rmse)     ))
#     elif interactive:
#         return(px.line(reconstructed,x='Start',y='Power',color='curve',title="Sim-ARIMA+MM: " + "MAE " + str(benchmark_mae) + ", RMSE " + str(benchmark_rmse) + ", Sim-NHP+CMM:" + "MAE " + str(challenger_mae) + ", RMSE " + str(challenger_rmse)     ))

# ####

# def compare_curves(final,date="2018-10-08"):
#     final = final[final['Start'].dt.strftime("%Y-%m-%d")==date]
#     benchmark = round(mae(final[final['curve']=='observed'].Power,final[final['curve']=='Sim-ARIMA+GMM'].Power),2)
#     challenger = round(mae(final[final['curve']=='observed'].Power,final[final['curve']=='Sim-NHP+CGMM'].Power),2)
#     print("Sim-ARIMA+GMM:",benchmark)
#     print("Sim-NHP+CGMM:",challenger)
#     return(sns.lineplot(data=final,x='Start',y='Power',hue='curve').set_title("[" + date+"] " + "Sim-ARIMA+GMM:" + str(benchmark) + ", Sim-NHP+CGMM:" + str(challenger )     ))

# ####

# def compare_error_profiles(final,interactive=False,metric=mae):
#     errors = []
#     for h in range(24):
#         temp = final[final['Start'].dt.strftime("%H").astype(int)==h].reset_index(drop=True)
#         errors.append([h,
#         round(metric(temp[temp['curve']=='observed'].Power,temp[temp['curve']=='Sim-ARIMA+GMM'].Power),2),
#         round(metric(temp[temp['curve']=='observed'].Power,temp[temp['curve']=='Sim-NHP+CGMM'].Power),2)])
#     result = pd.DataFrame(errors)
#     result.columns = ['tod','ARIMA+MM','NHPP+MM']
#     result = result.set_index('tod')
#     result = result.stack().reset_index()
#     result.columns = ['tod','model','error']
#     if interactive==False:
#         return(sns.lineplot(data=result,x='tod',y='error',hue='model').set_title("Error per tod" ))
#     elif interactive:
#         return(px.line(result,x='tod',y='error',color='model',title=str("Error per tod" )))