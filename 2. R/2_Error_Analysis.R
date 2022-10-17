##### 1. Importing functions libraries #####
library(tidyverse)
library(lubridate)
library(forecast)

args=commandArgs(TRUE)
dataset = args[2]
file = args[1]
print(file)
name = paste0(str_sub(file,end = -2),"error_",str_sub(file,-1,-1))

duration = args[3]

# ##### 3. Error analysis

path = paste("3. Python/1. Outputs/", dataset,"/",sep="")
observed = read.csv(paste(path,"observed_all_",duration,".csv",sep="")) %>%
  mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S"))


if (duration=="Charge.Duration"){
  train_pred = read.csv(paste(path,"train_signals/",file,"_load_curve.csv",sep="")) %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S")) %>%
    select(Start,Power) %>%
    rename("pred_power"="Power")
  
  test_pred = read.csv(paste(path,"test_signals/", file,"_load_curve.csv",sep="")) %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S")) %>%
    select(Start,Power) %>%
    rename("pred_power"="Power")
  
  train = inner_join(observed,train_pred,by=c("Start")) %>%
    mutate(error = Power - pred_power) %>%
    mutate(instant = hour(Start)*60 + minute(Start) )
  
  test = inner_join(observed,test_pred,by=c("Start")) %>%
    mutate(error = Power - pred_power) %>%
    mutate(instant = hour(Start)*60 + minute(Start) )
} else if (duration=="Park.Duration"){
  train_pred = read.csv(paste(path,"train_signals/",file,"_occupancy.csv",sep="")) %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S")) %>%
    select(Start,Occupancy) %>%
    rename("pred_occupancy"="Occupancy")
  
  test_pred = read.csv(paste(path,"test_signals/", file,"_occupancy.csv",sep="")) %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S")) %>%
    select(Start,Occupancy) %>%
    rename("pred_occupancy"="Occupancy")
  
  train = inner_join(observed,train_pred,by=c("Start")) %>%
    mutate(error = Occupancy - pred_occupancy) %>%
    mutate(instant = hour(Start)*60 + minute(Start) )
  
  test = inner_join(observed,test_pred,by=c("Start")) %>%
    mutate(error = Occupancy - pred_occupancy) %>%
    mutate(instant = hour(Start)*60 + minute(Start) )
}

all = bind_rows(train,test)

# Training models
# start_time = Sys.time()
# models = list()
# 
# for (i in 0:1439){
#   if (i %% 100 == 0){
#     print(paste("Iteration",i,"out of 1439"))
#   }
#   models[[i+1]] = auto.arima((train %>% filter(instant==i))$error)
# }
# print(Sys.time() - start_time)
  
auto_train = function(df,tod){
  return(auto.arima((df %>% filter(instant==tod))$error))
}

models = sort(unique(all$instant)) %>%
  map(auto_train,df=train)

# Getting forecast
# refits = list()
# fc = list()
# for (i in 0:1439){
# if (i %% 100 == 0){
#   print(paste("Iteration",i,"out of 1439"))
# }
# 
# all_instant = (all %>% filter(instant==i))$error
# 
# test_instant = (test %>% filter(instant==i))$error
# 
# refits[[i+1]] = Arima(all_instant, model=models[[i+1]])
# 
# fc[[i+1]] = test %>% filter(instant==i) %>% mutate(pred_error=window(fitted(refits[[i+1]]), start=length(all_instant)-length(test_instant)+1 ))
# }

auto_fc = function(models,tod){
  all_instant = (all %>% filter(instant==tod))$error
  test_instant = (test %>% filter(instant==tod))$error
  refit = Arima(all_instant, model=models[[tod+1]])
  return(test %>% filter(instant==tod) %>% mutate(pred_error=window(fitted(refit), start=length(all_instant)-length(test_instant)+1 )))
}

fc = sort(unique(all$instant)) %>%
  map(auto_fc,models=models)

if (duration=="Charge.Duration"){
  final = bind_rows(fc) %>%
    arrange(Start) %>%
    mutate(Power = pred_power + pred_error) %>%
    select(Start,Power,curve,date) %>%
    mutate(curve = name) %>%
    mutate(Power = ifelse(Power < 0,0,Power)) #Check that we only get non-negative values
  
  write.csv(final,paste("3. Python/1. Outputs/",dataset,"/test_signals/",name,"_load_curve.csv",sep=""),row.names = F)
} else if (duration=="Park.Duration"){
  final = bind_rows(fc) %>%
    arrange(Start) %>%
    mutate(Occupancy = pred_occupancy + pred_error) %>%
    select(Start,Occupancy,curve,date) %>%
    mutate(curve = name) %>%
    mutate(Occupancy = ifelse(Occupancy < 0,0,round(Occupancy))) #Check that we only get non-negative rounded values
  write.csv(final,paste("3. Python/1. Outputs/",dataset,"/test_signals/",name,"_occupancy.csv",sep=""),row.names = F)
}


# END