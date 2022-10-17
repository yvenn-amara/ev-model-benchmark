library(tidyverse)



args=commandArgs(TRUE)
dataset = args[2]
model = str_replace(args[1],"\\+","\\\\+")
path = paste("3. Python/1. Outputs/",dataset,"/",sep="")

duration = args[3]

# Importing the rolling forecasts
# temp = list.files(path = paste(path,"test_signals",sep=""), pattern = paste0(model,"(_error_)","(\\d+)",".*\\.csv$"))

if (duration == "Charge.Duration"){
  temp = list.files(path = paste(path,"test_signals",sep=""), pattern = paste0(model,"(_)\\d+(_load_curve.csv)"))
} else if (duration == "Park.Duration"){
  temp = list.files(path = paste(path,"test_signals",sep=""), pattern = paste0(model,"(_)\\d+(_occupancy.csv)"))
}
print(temp)
myfiles = lapply(paste0(path,"test_signals/",temp), read.csv)
df = bind_rows(myfiles)

# Renaming curve name
df['curve']=args[1]

# Saving final forecast
if (duration == "Charge.Duration"){
  write.csv(df,paste(path,args[1],"_load_curve.csv",sep=""),row.names = F)
} else if (duration == "Park.Duration"){
  write.csv(df,paste(path,args[1],"_occupancy.csv",sep=""),row.names = F)
}
