##### 1. Importing functions libraries #####
require(tidyverse)
require(lubridate)
require(NHPoisson)
require(mgcv)
args=commandArgs(TRUE)

dataset = args[1]

if (dataset == "domestics"){
  dayonly = FALSE
} else {
  dayonly = TRUE
}

train_dates = read.csv(paste("2. R/temp/",dataset,"/train_dates.csv",sep=''))
test_dates = read.csv(paste("2. R/temp/",dataset,"/test_dates.csv",sep=''))

start_date = train_dates$Start[[1]]
end_date = test_dates$End[[nrow(test_dates)]]
tz = "UTC"
window = read.csv(paste("2. R/temp/",dataset,"/best_window.csv",sep=""))[['window']]
source("2. R/utilities/utility.R")
source("2. R/utilities/NHPP_wrapper_v2.R")
source("2. R/utilities/SARIMA_wrapper_v2.R")
source("2. R/utilities/RF_wrapper.R")

##### 2. NHPP Routine #####

###
config_gam = readRDS(paste("2. R/temp/",dataset,"/best_config_nhpp_gam.rds",sep=""))
config_gam$fmla = as.character(config_gam$fmla)
write.csv(config_gam,paste("2. R/temp/",dataset,"/best_config_nhpp_gam.csv",sep=""),row.names = FALSE)
config_gam = read.csv(paste("2. R/temp/",dataset,"/best_config_nhpp_gam.csv",sep=""))
forecast_splines_dow = nhpp_splines_dow(df_all,as.formula(config_gam$fmla),train_dates,test_dates)

###
forecast_sarima = sarima(train_dates,test_dates)

###
config_rf = readRDS(paste("2. R/temp/",dataset,"/best_config_nhpp_rf.rds",sep=""))
config_rf$fmla = as.character(config_rf$fmla)
config_rf = config_rf %>% dplyr::select(-model)
write.csv(config_rf,paste("2. R/temp/",dataset,"/best_config_nhpp_rf.csv",sep=""),row.names = FALSE)
config_rf = read.csv(paste("2. R/temp/",dataset,"/best_config_nhpp_rf.csv",sep=""))

forecast_rf = rf_pois(df_all,as.formula(config_rf$fmla),config_rf$num.trees,config_rf$mtry,train_dates,test_dates)

##### 3. Saving Forecasts #####

print("Saving forecast")
write_csv(df %>% 
            filter(Weekend==0) %>% 
            mutate(Start = as.character(Start)),paste("2. R/1. Outputs/",dataset,"/sarima/cleaned_all.csv",sep=""))

write_csv(df %>% 
            filter(as.character(as.Date(Start)) >= test_dates[[1]][[1]]) %>%
            filter(as.character(as.Date(Start)) <= test_dates[[length(test_dates)]][[2]]) %>%
            filter(Weekend==0) %>% 
            mutate(Start = as.character(Start)),paste("2. R/1. Outputs/",dataset,"/sarima/cleaned_test.csv",sep=""))

write_csv(df %>% 
            filter(as.character(as.Date(Start)) >= train_dates[[1]][[1]]) %>%
            filter(as.character(as.Date(Start)) <= train_dates[[1]][[2]]) %>%
            filter(Weekend==0) %>% 
            mutate(Start = as.character(Start)),paste("2. R/1. Outputs/",dataset,"/sarima/cleaned_train.csv",sep=""))

write_csv(df_daily %>% filter(wday(Start_date) %in% c(2,3,4,5,6)), paste("2. R/1. Outputs/",dataset,"/daily_sessions.csv",sep=""))

for (i in 1:length(test_dates)){
  # write.csv(forecast_nhpp[[1]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_wav_train_",i,".csv",sep=""),row.names=FALSE)
  # write.csv(forecast_nhpp[[2]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_wav_test_",i,".csv",sep=""),row.names=FALSE)

  # write.csv(forecast_splines[[1]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_splines_train_",i,".csv",sep=""),row.names=FALSE)
  # write.csv(forecast_splines[[2]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_splines_test_",i,".csv",sep=""),row.names=FALSE)

  write.csv(forecast_splines_dow[[1]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_splines_dow_train_",i,".csv",sep=""),row.names=FALSE)
  write.csv(forecast_splines_dow[[2]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_splines_dow_test_",i,".csv",sep=""),row.names=FALSE)

  # write.csv(forecast_splines_tod[[1]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_splines_tod_train_",i,".csv",sep=""),row.names=FALSE)
  # write.csv(forecast_splines_tod[[2]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_nhpp_splines_tod_test_",i,".csv",sep=""),row.names=FALSE)

  # write.csv(forecast_sarima[[1]][[i]],paste("2. R/1. Outputs/", dataset, "/sarima/pred_sessions_train_",i,".csv",sep=""),row.names=FALSE)
  # write.csv(forecast_sarima[[2]][[i]],paste("2. R/1. Outputs/", dataset, "/sarima/pred_sessions_test_",i,".csv",sep=""),row.names=FALSE)
  
  write.csv(forecast_rf[[1]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_rf_train_",i,".csv",sep=""),row.names=FALSE)
  write.csv(forecast_rf[[2]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_rf_test_",i,".csv",sep=""),row.names=FALSE)
  
  # write.csv(forecast_smooth[[1]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_smooth_train_",i,".csv",sep=""),row.names=FALSE)
  # write.csv(forecast_smooth[[2]][[i]],paste("2. R/1. Outputs/", dataset, "/nhp/forecast_smooth_test_",i,".csv",sep=""),row.names=FALSE)
}

write.csv(bind_rows(forecast_splines_dow[[3]]) %>% filter(wday(Start) %in% c(2,3,4,5,6)),paste("2. R/1. Outputs/", dataset, "/nhp/splines_lambdas.csv",sep=""),row.names=FALSE)
write.csv(bind_rows(forecast_rf[[3]]) %>% filter(wday(Start) %in% c(2,3,4,5,6)),paste("2. R/1. Outputs/", dataset, "/nhp/rf_lambdas.csv",sep=""),row.names=FALSE)


# END