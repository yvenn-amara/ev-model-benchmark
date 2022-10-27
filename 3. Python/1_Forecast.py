import os
import pandas as pd

dataset=input("Dataset:")
train_dates = pd.read_csv('2. R/temp/'+dataset+'/train_dates.csv')
test_dates = pd.read_csv('2. R/temp/'+dataset+'/test_dates.csv')

for i in range(len(train_dates)):
    print("Iteration " + str(i+1))
    list_str = ["python 3.\ Python/gru.py",
                dataset,
                train_dates["Start"][i],
                train_dates["End"][i],
                test_dates["Start"][i],
                test_dates["End"][i],
                str(i+1)]
    os.system(" ".join(list_str))