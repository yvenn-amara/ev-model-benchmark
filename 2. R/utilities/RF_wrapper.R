##### 1. Importing functions libraries #####
library(tidyverse)
library(ranger)

rf_pois = function(df_all,fmla,num.trees,mtry,train_dates,test_dates,simu_type="thinning",step=30,model_name=NULL){
  res_train = list()
  res_test = list()
  lambdas_test = list()
  for(i in 1:nrow(train_dates)){
    print(paste("Iteration: ",i,"/",nrow(train_dates),sep=''))
    # Splitting train and test data
    df_all_train = df_all %>% filter(as.Date(as.character(Start)) >= train_dates$Start[[i]]) %>% filter(as.Date(as.character(Start)) <= train_dates$End[[i]])
    df_all_test = df_all %>% filter(as.Date(as.character(Start)) >= test_dates$Start[[i]]) %>% filter(as.Date(as.character(Start)) <= test_dates$End[[i]])
    
    # Training model
    rf = ranger(fmla, data=df_all, num.trees=num.trees, mtry=mtry)
    
    # Simulation
    print("Simulation")
    lambdafit_train = predict(rf, data=df_all_train)$prediction
    lambdafit_test = predict(rf, data=df_all_test)$prediction

    if (simu_type=="thinning"){
      simu_train = thin_sim_afterfit(lambdafit_train, 1,nrow(df_all_train))
      df_all_train = df_all_train %>% mutate(time = seq(1,nrow(df_all_train)))
      simu_arrival_train = left_join(tibble(time=simu_train),df_all_train,by="time") %>% dplyr::select(Start)
      
      # simu_arrival_train = tibble(Start=min(df_all_train$Start)+simu_train*60)
      
      simu_test = thin_sim_afterfit(lambdafit_test, 1,nrow(df_all_test))
      df_all_test = df_all_test %>% mutate(time = seq(1,nrow(df_all_test)))
      simu_arrival_test = left_join(tibble(time=simu_test),df_all_test,by="time") %>% dplyr::select(Start)
      
      
      # simu_arrival_test = tibble(Start=min(df_all_test$Start)+simu_test*60)
    }
    
    else if (simu_type=="cinlar"){
      simu_train = simNHP.fun(lambdafit_train)
      simu_arrival_train = tibble(Start=min(df_all_train$Start)+simu_train*60)
      
      simu_test = simNHP.fun(lambdafit_test)
      simu_arrival_test = tibble(Start=min(df_all_test$Start)+simu_test*60)
    }
    
    res_train[[i]] = simu_arrival_train
    res_test[[i]] = simu_arrival_test
    lambdas_test[[i]]=tibble(Start = df_all_test$Start,lambda = lambdafit_test)
  }
  return(list(res_train,res_test,lambdas_test))
  
}

direct_sim = function(df_all,train_dates,test_dates){
  res_train = list()
  res_test = list()
  for(i in 1:nrow(train_dates)){
    print(paste("Iteration: ",i,"/",nrow(train_dates),sep=''))
    df_all_train = df_all %>% filter(as.Date(as.character(Start)) >= train_dates$Start[[i]]) %>% filter(as.Date(as.character(Start)) <= train_dates$End[[i]])
    df_all_test = df_all %>% filter(as.Date(as.character(Start)) >= test_dates$Start[[i]]) %>% filter(as.Date(as.character(Start)) <= test_dates$End[[i]])
    
    print("Simulation")
    lambdafit_train = df_all_train$smooth_event
    lambdafit_test = df_all_test$smooth_event
    
    simu_train = thin_sim_afterfit(lambdafit_train, 1,nrow(df_all_train))
    df_all_train = df_all_train %>% mutate(time = seq(1,nrow(df_all_train)))
    simu_arrival_train = left_join(tibble(time=simu_train),df_all_train,by="time") %>% dplyr::select(Start)
    
    simu_test = thin_sim_afterfit(lambdafit_test, 1,nrow(df_all_test))
    df_all_test = df_all_test %>% mutate(time = seq(1,nrow(df_all_test)))
    simu_arrival_test = left_join(tibble(time=simu_test),df_all_test,by="time") %>% dplyr::select(Start)
    
    res_train[[i]] = simu_arrival_train
    res_test[[i]] = simu_arrival_test
  }
  return(list(res_train,res_test))
}
