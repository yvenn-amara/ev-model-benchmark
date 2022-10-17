from Utility import *
from datetime import datetime
import pandas as pd
import time

duration = "Charge.Duration"

s = datetime(2022,1,1,10,0,0)
e = datetime(2022,1,1,20,0,0)

dd = pd.read_csv("1. Preprocessed Data/palo_alto.csv").iloc[0:10000,]
dd["Start"] = pd.to_datetime(dd["Start"])


# dd = pd.DataFrame({
#     "Start":[s],
#     "Charge.Duration":[60],
#     "Energy":[50]
# })

# a = daterange(s,e)
# print(a)

start = time.time()
[interpol(dd.iloc[i],"Charge.Duration") for i in range(len(dd))]

# l = []
# for i in range(len(dd)):
#     l.append(interpol(dd.iloc[[i],:],"Charge.Duration"))

# dd.apply(interpol, axis=1, duration=duration)
print(time.time()-start)