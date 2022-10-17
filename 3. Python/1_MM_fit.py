# -*- coding: utf-8 -*-
"""
MM fit
"""
import pandas as pd
from gmm import gmm_search_fit
from datetime import timedelta
import matplotlib.pyplot as plt
from Utility import *
from os import path
import pickle
from os import listdir
from os.path import isfile

cwd0 = '2. R/1. Outputs/'
datasets = [f for f in listdir(cwd0)]
dataset=input("Datasets Available: " + ', '.join(datasets) + "\nSelect Dataset: ") +"/"

cwd1 = '2. R/1. Outputs/' + dataset
cwd3 = '3. Python/1. Outputs/' + dataset

df = pd.read_csv(cwd1 + "sarima/cleaned_train.csv")
df['Start'] = pd.to_datetime(df['Start']).dt.round('1min')

raw_rows = len(df)

duration = input("Choose Duration Type (Park.Duration/Charge.Duration):")
df = df[(df[duration] > 0) & (df['Energy']>0)]# Remove impossible sessions (negative charge/park duration or negative energy)
print("Impossible sessions discarded: ", (raw_rows-len(df))/raw_rows, "%")

triple = df[["Arrival",duration,"Energy"]].values

# GMM fit
if path.exists(cwd3+"best_gmm_"+duration+".pkl"):
    print("Recovering best mixture fit")
    with open(cwd3+"best_gmm_"+duration+".pkl", "rb") as fp:
        best_gmm = pickle.load(fp)
else:
    print("Finding best mixture")
    best_gmm = gmm_search_fit(X=triple,path=cwd3, n_comp=30, verbose = True, desc='gmm-fit', loglink=False)
    with open(cwd3+"best_gmm_"+duration+".pkl", "wb") as fp:
        pickle.dump(best_gmm, fp)

# LMM fit
if path.exists(cwd3+"best_lmm_"+duration+".pkl"):
    print("Recovering best mixture fit")
    with open(cwd3+"best_lmm_"+duration+".pkl", "rb") as fp:
        best_gmm = pickle.load(fp)
else:
    print("Finding best mixture")
    best_gmm = gmm_search_fit(X=triple,path=cwd3, n_comp=30, verbose = True, desc='lmm-fit', loglink=True)
    with open(cwd3+"best_lmm_"+duration+".pkl", "wb") as fp:
        pickle.dump(best_gmm, fp)