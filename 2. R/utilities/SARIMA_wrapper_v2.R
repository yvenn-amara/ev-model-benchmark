##### 1. Importing functions libraries #####
library(tidyverse)
library(lubridate)
library(forecast)

##### 2. SARIMA estimation #####

sarima = function(train_dates,test_dates){
  
  res_train=list()
  res_test=list()
  #models=list()
  
  for(i in 1:nrow(train_dates)){
  df_daily_train = df_daily %>% 
    filter(wday(as.Date(Start_date)) %in% c(2,3,4,5,6)) %>%
    filter(Start_date >= train_dates$Start[[i]]) %>% 
    filter(Start_date <= train_dates$End[[i]]) 
  df_daily_test = df_daily %>% 
    filter(wday(as.Date(Start_date)) %in% c(2,3,4,5,6)) %>%
    filter(Start_date >= test_dates$Start[[i]]) %>% 
    filter(Start_date <= test_dates$Start[[i]])
  
  # Training model
  mod = auto.arima(ts(df_daily_train$sessions, frequency = 5),
                   stepwise = FALSE,
                   max.p = 5,
                   max.q = 5,
                   max.P = 5,
                   max.Q = 5,
                   max.order = 10,
                   max.d = 5,
                   max.D = 5,
                   seasonal=TRUE,trace=T, ic = "bic")
  
  # Getting Predictions
  refit = Arima(df_daily_test$sessions, model=mod)
  
  res_train[[i]] = df_daily_train %>% mutate(pred = mod$fitted)
  res_test[[i]] = df_daily_test %>% mutate(pred = round(refit$fitted))
  # models[[i]] = mod
  
  }
  return(list(res_train,res_test))
  
}
