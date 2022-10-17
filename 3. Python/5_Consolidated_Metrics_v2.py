import pandas as pd
import seaborn as sns
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from os import listdir
import plotly.express as px
matplotlib.rc('pdf', fonttype=42)
path = '2. R/1. Outputs/all/'

duration = input("Choose Duration Type (Park.Duration/Charge.Duration):")
if duration == "Charge.Duration":
    name = "load_curve"
    target = "load"
elif duration == "Park.Duration":
    name = "occupancy"
    target = "occupancy"

files = [f for f in listdir(path) if f.endswith(name+".csv")]

data_list = []
for i in range(len(files)):
    temp = pd.read_csv(path+files[i])
    data_list.append(temp)

df = pd.concat(data_list,axis=0)
df['imp'] = (df['imp']-1)*100
df['imp2'] = (df['imp2']-1)*100

df_agg = round(df.groupby('model').mean().reset_index(),2)
df_agg = df_agg[['model','imp','imp2','mae','rmse']].sort_values(by=['imp'],ascending=False)


table1 = df_agg.to_html(classes='w-50 table table-dark text-center table-hover', justify='center')

fig1 = px.box(df, x="model", y="imp")
fig1.update_layout(
    title="<b>Boxplot BB IMP</b>",
    xaxis={'categoryorder':'median descending'},
    xaxis_title="Model",
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

fig2 = px.box(df, x="model", y="imp2")
fig2.update_layout(
    title="<b>Boxplot BB IMP2</b>",
    xaxis={'categoryorder':'median descending'},
    xaxis_title="Model",
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

print("Saving HTML file")
with open(path + "benchmark_"+name+".html", 'w') as f:
    f.write("""<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">""")
    f.write(table1)
    f.write(fig1.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig2.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write("""<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js" integrity="sha384-QJHtvGhmr9XOIpI6YVutG+2QOK9T+ZnN4kzFN1RtK3zEFEIsxhlmWl5/YESvpZ13" crossorigin="anonymous"></script>""")


#### Seaborn pdf save ####
### BB boxplots
short_names = {'AGG_EXP':'AGG',
               'GAM':'GAM',
               'GAM_ARIMA':'GAM-err',
               'RF':'RF',
               'RF_ARIMA':'RF-err',
               'GRU+LMM':'GRU-MM',
               'GRU+LMM_error':'GRU-MM-err',
               'NHPP_RF+CLMM_error':'NHPP-RF-MR-err',
               'NHPP_RF+CLMM':'NHPP-RF-MR',
               'NHPP_Smooth+CLMM_error':'NHPP-MA-MR-err',
               'NHPP_Smooth+CLMM':'NHPP-MA-MM',
               'NHPP_splines_dow+CLMM_error':'NHPP-GAM-MR-err',
               'NHPP_splines_dow+CLMM':'NHPP-GAM-MR',
               'persistence':'persistence',
               'SARIMA+LMM_error':'SARIMA-MM-err',
               'SARIMA+LMM':'SARIMA-MM',
               'NAF-SARIMA':'SARIMA-NAF'
}

df.replace({"model": short_names}, inplace = True)
df = df.reset_index(drop=True)

### Filter only for relevant models
relevant_models = ['AGG','GAM','RF','GAM-err','RF-err','GRU-MM','GRU-MM-err','NHPP-RF-MR-err','NHPP-RF-MR','NHPP-GAM-MR-err','NHPP-GAM-MR','persistence','SARIMA-MM-err','SARIMA-MM']
df = df[df['model'].isin(relevant_models)]

#### PIP MAE Boxplots
my_order = df.groupby(by=["model"])["imp"].median( ).iloc[::-1].sort_values(ascending=False).index

sns.set(style = "whitegrid", font_scale=1)
sns.boxplot(y="model", x="imp", data=df, order = my_order, showfliers = False)
# plt.xticks(rotation=45)
plt.ylabel(" ")
plt.xlabel("MAE [%]")
plt.tight_layout()
plt.savefig('2. R/1. Outputs/all/bb_boxplot_'+duration+'.pdf')

#### PIP RMSE Boxplots
my_order = df.groupby(by=["model"])["imp2"].median( ).iloc[::-1].sort_values(ascending=False).index
plt.figure()
sns.set(style = "whitegrid", font_scale=1)
sns.boxplot(y="model", x="imp2", data=df, order = my_order, showfliers = False)
# plt.xticks(rotation=45)
plt.ylabel(" ")
plt.xlabel("RMSE [%]")
plt.tight_layout()
plt.savefig('2. R/1. Outputs/all/bb_boxplot2_'+duration+'.pdf')

### AGG stacked barplot
clean_data =  {'acn':'Caltech',
               'boulder':'Boulder',
               'domestics':'Domestics UK',
               'dundee':'Dundee',
               'palo_alto':'Palo Alto',
               'paris':'Paris',
               'perth':'Perth',
               'sap':'SAP'
}

weight_files = [f for f in listdir(path+"/agg_weights/") if f.endswith(target+".csv")]

weight_list = []
for i in range(len(weight_files)):
    temp = pd.read_csv(path+"/agg_weights/"+weight_files[i])
    weight_list.append(temp)

weights = pd.concat(weight_list,axis=0)
weights = weights[weights['target']==target]
weights.replace({"dataset": clean_data}, inplace = True)
# weights['mean'] = round(weights['mean']/len(weight_list),2)*100


round(weights.groupby(by=["model"])["mean"].mean(),2).sort_values(ascending=False).to_csv('2. R/1. Outputs/all/all_weights/'+duration+'_table.csv')

my_order_1 = weights.groupby(by=["model"])["mean"].sum( ).iloc[::-1].sort_values(ascending=True).index
my_order_2 = weights["dataset"].sort_values(ascending=False).unique()

weights1 = weights.pivot(index='dataset',columns='model',values='mean').reset_index()
weights2 = weights.pivot(index='model',columns='dataset',values='mean').reset_index()
weights1 = weights1[['dataset']+list(my_order_1)]
weights2 = weights2[['model']+list(my_order_2)]
print(weights2)

sns.set(style = "whitegrid", font_scale=1)
plt.figure()

# palette = {'Caltech':"tab:cyan",
#                'Boulder':"tab:orange",
#                'Domestics UK':"tab:purple",
#                'Dundee':'tab:red',
#                'Palo Alto':'tab:blue',
#                'Paris':'tab:green',
#                'Perth':'tab:yellow',
#                'SAP':'tab:grey'
# }

ax1 = weights1.set_index('dataset').T.plot(kind='barh', stacked=True, colormap='Set1')
sns.move_legend(ax1, "lower right")
plt.legend(title="Dataset",bbox_to_anchor=(1.05, 1), loc='upper left', borderaxespad=0)
# sns.barplot(x ="model", y = 'mean', data = weights, hue = "dataset")
plt.ylabel(" ")
plt.xlabel("Average weight [%]")
plt.tight_layout()
plt.savefig('2. R/1. Outputs/all/agg_weights/stacked_bars_mod_'+target+'.pdf')


if target=="occupancy":
    leg = False
else:
    leg=True

plt.figure()
ax2 = weights2.set_index('model').T.plot(kind='barh', stacked=True, legend=leg)
# sns.move_legend(ax2, "lower right")
# plt.legend(title="Model",bbox_to_anchor=(1.05, 1), loc='upper left', borderaxespad=0)
if leg:
    plt.legend(title="Model",bbox_to_anchor=(1, -0.35), borderaxespad=0, ncol=3)
# sns.barplot(x ="model", y = 'mean', data = weights, hue = "dataset")
plt.ylabel(" ")
plt.xlabel("Average weight [%]")
plt.tight_layout()
plt.savefig('2. R/1. Outputs/all/agg_weights/stacked_bars_data_'+target+'.pdf')