import pandas as pd
import numpy as np
from os import listdir
import plotly.express as px
from plotly import subplots
from sklearn.metrics import mean_absolute_error as mae
from sklearn.metrics import mean_squared_error as mse

def rmse(y_true,y_pred):
    return(np.sqrt(mse(y_true,y_pred)))

def imp(y_true,y_pers,y_pred):

    # dd = pd.DataFrame({
    #     "true":y_true,
    #     "pers":y_pers,
    #     "pred":y_pred
    # })
    #
    # # dd = dd[dd["pred"]-dd["true"]!=0]
    # dd = dd[dd["true"] >= 1]
    # return(np.mean(abs(dd['pers']-dd['true'])/abs(dd['pred']-dd['true'])))

    return(mae(y_true,y_pers)/mae(y_true,y_pred))

def imp2(y_true,y_pers,y_pred):
    return(rmse(y_true,y_pers)/rmse(y_true,y_pred))

### 1. Importing Data

cwd = '3. Python/1. Outputs/'
datasets = [f for f in listdir(cwd)]

dataset_name =input("Datasets Available: " + ', '.join(datasets) + "\nSelect Dataset: ")
dataset =  dataset_name+"/"
duration = input("Choose Duration Type (Park.Duration/Charge.Duration):")
if duration == "Charge.Duration":
    name = "load_curve"
    target = "Power"
elif duration == "Park.Duration":
    name = "occupancy"
    target = "Occupancy"

#### Loading Models

path = "3. Python/1. Outputs/" + dataset + "/"
print(target.lower())
files = [f for f in listdir(path) if f.endswith(name+".csv") and ("observed" in f)==False]

data_list = []
for i in range(len(files)):
    temp = pd.read_csv(path+files[i])
    # print(temp)
    temp = temp.drop_duplicates(['Start','curve'])
    temp = temp[temp['curve']!="observed"].reset_index(drop=True)
    data_list.append(temp)
    # temp1 = temp[temp['curve']=="Sim-NHP+CGMM"].reset_index(drop=True)
    # temp2 = temp[temp['curve']=="Sim-ARIMA+GMM"].reset_index(drop=True)
    # temp3 = temp[temp['curve']=="NAF-SARIMA"].reset_index(drop=True)

    # if not temp1.empty:
    #     temp1['curve'] = files[i][6: -18]
    #     data_list.append(temp1)
    # if not temp2.empty:
    #     temp2['curve'] = "arima_" + files[i][6] + "mm"
    #     data_list.append(temp2)
    # if not temp3.empty:
    #     temp3['curve'] = files[i][0] + "_sarima-naf" 
    #     data_list.append(temp3)

df = pd.concat(data_list,axis=0)

# df = df.drop_duplicates(['Start','col_2'])
#### Loading Reconstructed Observed Curve

observed = pd.read_csv(path+"observed_test_"+duration+".csv")
# observed = observed[observed['curve']=="observed"].reset_index(drop=True)

df = pd.concat([df,observed],axis=0)
df['Start'] = pd.to_datetime(df['Start'])
start_dt = min(observed['Start'])
end_dt = max(observed['Start'])

df = df[(df['Start']>=start_dt) & (df['Start']<=end_dt) ] # Ensuring that we are only looking at the observed time period
# df.loc[df.Start.dt.weekday > 4, target] = 0 # WARNING: Only if weekends are not included in the analysis
df = df[df.Start.dt.weekday <= 4] # WARNING: Only if weekends are not included in the analysis
#df = df.fillna(method='ffill')

### 2. Error per time of day and day of week
abs_errors = squared_errors = percentage_improvement = df.pivot(index="Start",columns="curve",values=target).reset_index()
abs_errors = abs_errors.fillna(method='ffill')
global_mae = []
global_rmse = []
global_imp = []
global_imp2 = []
relevant_cols = [col for col in abs_errors.columns if col not in ["observed","Start"]]

for col in relevant_cols:
    global_mae.append(round(mae(abs_errors['observed'],abs_errors[col]),1))
    global_rmse.append(round(rmse(abs_errors['observed'],abs_errors[col]),1))
    global_imp.append(round(imp(abs_errors['observed'],abs_errors['persistence'],abs_errors[col]),2))
    global_imp2.append(round(imp2(abs_errors['observed'], abs_errors['persistence'], abs_errors[col]), 2))

global_metrics = pd.DataFrame({"model":relevant_cols,
                                "mae":global_mae,
                                "rmse":global_rmse,
                                "imp":global_imp,
                               "imp2":global_imp2})

global_metrics = global_metrics.set_index("model")
global_metrics = global_metrics.stack().reset_index()
global_metrics.columns = ['model','metric','value']
global_metrics.to_csv("3. Python/1. Outputs/all/" + dataset_name +"_"+name +".csv",index=False)

# fig = px.bar(global_metrics, x="model", y="value", color="metric", barmode='group', text='value')
# fig.update_traces(textposition='outside')
# fig.update_layout(
#     title="Load Curves Reconstructed",
#      xaxis={'categoryorder':'total ascending'},
#     xaxis_title="Time",
#     yaxis_title=target,
#     legend_title="Model",
#     uniformtext_minsize=8, 
#     uniformtext_mode='hide',
#     font=dict(
#         family="Courier New, monospace",
#         size=18,
#         color="RebeccaPurple"
#     )
# )
# fig.write_html(path + "analysis/metrics_"+name+".html")


# fig = subplots.make_subplots(cols=2)

global_mae = global_metrics[global_metrics['metric']=="mae"]
fig1 = px.bar(global_mae, x="model", y="value", text='value')
fig1.update_traces(textposition='outside',marker_color='blue')
fig1.update_layout(
    title="<b>MAE</b>",
    xaxis={'categoryorder':'total ascending'},
    xaxis_title="Time",
    yaxis_title="MAE",
    legend_title="Model",
    uniformtext_minsize=8, 
    uniformtext_mode='hide',
    font=dict(
        family="Courier New, monospace",
        size=18,
        color="RebeccaPurple"
    )
)

global_rmse = global_metrics[global_metrics['metric']=="rmse"]
fig2 = px.bar(global_rmse, x="model", y="value", text='value')
fig2.update_traces(textposition='outside', marker_color='orange')
fig2.update_layout(
    title="<b>RMSE</b>",
     xaxis={'categoryorder':'total ascending'},
    xaxis_title="Time",
    yaxis_title="RMSE",
    legend_title="Model",
    uniformtext_minsize=8, 
    uniformtext_mode='hide',
    font=dict(
        family="Courier New, monospace",
        size=18,
        color="RebeccaPurple"
    )
)

global_imp = global_metrics[global_metrics['metric']=="imp"]
fig2bis = px.bar(global_imp, x="model", y="value", text='value')
fig2bis.update_traces(textposition='outside', marker_color='green')
fig2bis.update_layout(
    title="<b>IMP</b>",
     xaxis={'categoryorder':'total descending'},
    xaxis_title="Time",
    yaxis_title="IMP",
    legend_title="Model",
    uniformtext_minsize=8,
    uniformtext_mode='hide',
    font=dict(
        family="Courier New, monospace",
        size=18,
        color="RebeccaPurple"
    )
)

global_imp2 = global_metrics[global_metrics['metric']=="imp2"]
fig2ter = px.bar(global_imp2, x="model", y="value", text='value')
fig2ter.update_traces(textposition='outside', marker_color='green')
fig2ter.update_layout(
    title="<b>IMP</b>",
     xaxis={'categoryorder':'total descending'},
    xaxis_title="Time",
    yaxis_title="IMP2",
    legend_title="Model",
    uniformtext_minsize=8,
    uniformtext_mode='hide',
    font=dict(
        family="Courier New, monospace",
        size=18,
        color="RebeccaPurple"
    )
)

# fig1_traces = []
# fig2_traces = []
# for trace in range(len(fig1["data"])):
#    fig1_traces.append(fig1["data"][trace])
# for trace in range(len(fig2["data"])):
#    fig2_traces.append(fig2["data"][trace])

# for traces in fig1_traces:
#    fig.append_trace(traces, row = 1, col = 1)
# for traces in fig2_traces:
#    fig.append_trace(traces, row = 1, col = 2)
# fig.write_html(path + "analysis/metrics_"+name+".html")

# MAES

for col in abs_errors.columns[1:-1]:
    abs_errors[col] = abs(abs_errors['observed']-abs_errors[col])

abs_errors["dow-hour"] = abs_errors['Start'].dt.strftime('%w') + "-" + abs_errors['Start'].dt.strftime('%H')
abs_errors = abs_errors.drop(["Start","observed"],axis=1)
maes = abs_errors.groupby(["dow-hour"]).mean().reset_index()
maes = maes.set_index("dow-hour").stack().reset_index()
maes.columns = ['dow-hour','model','mae']

fig3 = px.line(maes,x='dow-hour',y='mae',color='model',title="MAE per time of day and day of week")
fig3.update_layout(
    title="<b>MAE per dow-hour</b>",
    xaxis_title="Time",
    yaxis_title="MAE",
    legend_title="Model",
    font=dict(
        family="Courier New, monospace",
        size=18,
        color="RebeccaPurple"
    )
)
# fig3.write_html(path + "analysis/mae_"+name+".html")

# RMSES
for col in squared_errors.columns[1:-1]:
    squared_errors[col] = (squared_errors['observed']-squared_errors[col])**2

squared_errors["dow-hour"] = squared_errors['Start'].dt.strftime('%w') + "-" + squared_errors['Start'].dt.strftime('%H')
squared_errors = squared_errors.drop(["Start","observed"],axis=1)
rmses = squared_errors.groupby(["dow-hour"]).mean().reset_index()

for col in rmses.columns[1:]:
    rmses[col] = np.sqrt(rmses[col])

final = rmses.set_index("dow-hour").stack().reset_index()
final.columns = ['dow-hour','model','rmse']

fig4 = px.line(final,x='dow-hour',y='rmse',color='model',title="RMSE per time of day and day of week")
fig4.update_layout(
    title="<b>RMSE per dow-hour</b>",
    xaxis_title="Time",
    yaxis_title="RMSE",
    legend_title="Model",
    font=dict(
        family="Courier New, monospace",
        size=18,
        color="RebeccaPurple"
    )
)

# fig4.write_html(path + "analysis/rmse_"+name+".html")

### 3. Load Curves

print("Plotting Reconstructed")
fig5 = px.line(df,x='Start',y=target,color='curve',title=str("Forecasts and Observed" ))
fig5.update_layout(
    title="<b>Forecasts and Observed</b>",
    xaxis_title="Time",
    yaxis_title=target,
    legend_title="Model",
    font=dict(
        family="Courier New, monospace",
        size=18,
        color="RebeccaPurple"
    )
)

# final = df.pivot(index="Start",columns="curve",values=target).reset_index()
# for col in final.columns[1:-1]:
#     final[col] = final['observed']-final[col]

# print(final.head())

# final.set_index('curve', inplace=True)
# final = final.stack()

# fig5 = px.line(final,x='Start',y=target,color='curve',title=str("Load Curves Reconstructed" ))
# fig5.update_layout(
#     title="<b>Load Curves Reconstructed</b>",
#     xaxis_title="Time",
#     yaxis_title=target,
#     legend_title="Model",
#     font=dict(
#         family="Courier New, monospace",
#         size=18,
#         color="RebeccaPurple"
#     )
# )

# fig.write_html(path + "analysis/"+name+".html")

### END: Creating final HTML file
print("Saving HTML file")
with open(path + "analysis/"+name+'.html', 'w') as f:
    f.write(fig1.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig2.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig2bis.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig2ter.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig3.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig4.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig5.to_html(full_html=False, include_plotlyjs='cdn'))

