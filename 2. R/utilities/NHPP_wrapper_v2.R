##### 1. Importing functions libraries #####
library(tidyverse)
library(lubridate)
library(NHPoisson)
library(mgcv)
library(gamwave)

#### Wavelets

# Estimation function
estim_nhpp = function(covariates,posE){
  
  minuslogl = function(beta)
  {
    mllikpois = -sum(as.matrix(covariates[posE,])%*%beta) + sum(exp(as.matrix(covariates)%*%beta ))
    return(as.double(mllikpois))
  }
  
  out = nlminb(start=rep(0,ncol(covariates)), objective=minuslogl)
  #return(out)
  return(list("fullcoef"=out$par,"lambdafit"=as.vector(exp(covariates %*% out$par))))
  
}

# Wrapper
nhpp_wav = function(train_dates,test_dates,simu_type="thinning"){
  #time.span = nrow(df_all) #All time
  #all.minutes = seq(1,time.span,length.out=time.span)
  res_train=list()
  res_test=list()
  #models=list()
  for(i in 1:length(train_dates)){
  print(paste("Iteration: ",i,"/",length(train_dates),sep=''))
  # Splitting train and test data
  df_all_train = df_all %>% filter(as.Date(as.character(Start)) >= train_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= train_dates[[i]][[2]])
  df_all_test = df_all %>% filter(as.Date(as.character(Start)) >= test_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= test_dates[[i]][[2]])
    
  df_poisson_train = df_poisson %>% filter(as.Date(as.character(Start)) >= train_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= train_dates[[i]][[2]])
  #df_poisson_test = df_poisson %>% filter(as.Date(as.character(Start)) >= test_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= test_dates[[i]][[2]])
    
  #df_daily_train = df_daily %>% filter(as.Date(as.character(Start)) >= train_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= train_dates[[i]][[2]])
  df_daily_test = df_daily %>% filter(Start_date >= test_dates[[i]][[1]]) %>% filter(Start_date <= test_dates[[i]][[2]])
  
  # Creating basis
  basis_tod32 = WavD(seq(0,31,1),numLevels=5, family= "DaubExPhase", filterNumber = 1)
  colnames(basis_tod32) = paste("wav_tod",seq(1,ncol(basis_tod32),1),sep="")
  wavcov_tod32 = as_tibble(basis_tod32) %>%
    mutate(tod32=seq(0,31,1))
  
  max_agglagD1 = max(df_all_train$agglag_D1)
  basis_agglagD1 = WavD(seq(0,max_agglagD1,1),numLevels=round(log2(max_agglagD1)), family= "DaubExPhase", filterNumber = 1)
  colnames(basis_agglagD1) = paste("wav_agglag_D1",seq(1,ncol(basis_agglagD1),1),sep="")
  wavcov_agglagD1 = as_tibble(basis_agglagD1) %>%
    mutate(agglag_D1=seq(0,max_agglagD1,1))
  
  max_agglagD7 = max(df_all_train$agglag_D7)
  basis_agglagD7 = WavD(seq(0,max_agglagD7,1),numLevels=round(log2(max_agglagD7)), family= "DaubExPhase", filterNumber = 1)
  colnames(basis_agglagD7) = paste("wav_agglag_D7",seq(1,ncol(basis_agglagD7),1),sep="")
  wavcov_agglagD7 = as_tibble(basis_agglagD7) %>%
    mutate(agglag_D7=seq(0,max_agglagD7,1))
  
  # Creating covariates
  covar_train = df_all_train %>% 
    left_join(.,wavcov_tod32,by='tod32') %>%
    left_join(.,wavcov_agglagD1,by='agglag_D1') %>%
    left_join(.,wavcov_agglagD7,by='agglag_D7') %>%
    mutate(Day_week = ifelse(wday(df_all_train$Start) %in% c(2,3,4,5,6),1,0),
           intercept = 1) %>%
    dplyr::select(intercept,
                  starts_with("wav_tod"),
                  starts_with("Day_")#,
                  # starts_with("wav_agglag")
                  ) %>%
    as.matrix()
  
  covar_test = df_all_test %>% 
    left_join(.,wavcov_tod32,by='tod32') %>%
    left_join(.,wavcov_agglagD1,by='agglag_D1') %>%
    left_join(.,wavcov_agglagD7,by='agglag_D7') %>%
    mutate(Day_week = ifelse(wday(df_all_test$Start) %in% c(2,3,4,5,6),1,0),
           intercept = 1) %>%
    dplyr::select(intercept,
                  starts_with("wav_tod"),
                  starts_with("Day_")#,
                  # starts_with("wav_agglag")
                  ) %>%
    as.matrix()
  
  colnames(covar_train) = NULL
  colnames(covar_test) = NULL
  
  # Check if covar is ill-conditioned
  test = Matrix::rankMatrix(covar_train)[[1]] != ncol(covar_train)
  print(test)
  # if (test){
  #   covar_train = WeightIt::make_full_rank(covar_train,with.intercept = T)
  # }
  # covar_corr = round(cor(covar),2)
  # Matrix::rankMatrix(covar_train)
  
  # 
  # start = as.list(rep(0,ncol(covar_train) + 1))
  # names(start) = paste("b",seq(0,ncol(covar_train)),sep="")
  
  print("Training model")
  
  # out = fitPP.fun(covariates = covar_train, posE = df_poisson_train$events,#[df_poisson$events < time.span],
  #                 start=start, modSim=T, tind=T, minfun="nlminb"#fixed=list("b32"=0.2)#,method='L-BFGS'
  # )
  out = estim_nhpp(covariates=covar_train,posE = df_poisson_train$events)
  
  #### Simulation
  
  # lambdafit = as.vector(exp(covar_test %*% out@fullcoef[2:length(out@fullcoef)] + out@fullcoef[1]))#[1:nrow(data)]
  lambdafit_train = as.vector(exp(covar_train %*% out$fullcoef))
  lambdafit_test = as.vector(exp(covar_test %*% out$fullcoef))
  
  # Simulation algorithm
  print("Simulation")
  if (simu_type=="thinning"){
    simu_train = thin_sim_afterfit(lambdafit_train, 1,nrow(df_all_train))
    simu_arrival_train = tibble(Start=min(df_all_train$Start)+simu_train*60)
    
    simu_test = thin_sim_afterfit(lambdafit_test, 1,nrow(df_all_test))
    simu_arrival_test = tibble(Start=min(df_all_test$Start)+simu_test*60)
  }

  else if (simu_type=="cinlar"){
    simu_train = simNHP.fun(lambdafit_train)
    simu_arrival_train = tibble(Start=min(df_all_train$Start)+simu_train$posNH*60)
    
    simu_test = simNHP.fun(lambdafit_test)
    simu_arrival_test = tibble(Start=min(df_all_test$Start)+simu_test$posNH*60)
  }

  # Getting daily sessions
  simu_arrival_daily = simu_arrival_test %>%  
    mutate(Start_date = as.Date(Start)) %>%
    group_by(Start_date) %>%
    summarise(sessions = n())
  
  plot(simu_arrival_daily,col='green',ylim=range(simu_arrival_daily$sessions,df_daily_test$sessions),type='b')
  points(df_daily_test$Start_date,df_daily_test$sessions,col='blue',type='b')
  
  # print(mae(df_daily_test$sessions,simu_arrival_daily$sessions))
  
  res_train[[i]] = simu_arrival_train
  res_test[[i]] = simu_arrival_test
  #models[[i]] = out
  }
  return(list(res_train,res_test))
}

#### Splines

# Estimation function
nhpp_splines = function(df_all,fmla,train_dates,test_dates,simu_type="thinning",step=30,model_name=NULL){
  ##### Step in minutes
  require(ppgam)
  
  res_train=list()
  res_test=list()
  lambdas_test=list()
  
  # agg_step = list()
  #models=list()
  
  for(i in 1:length(train_dates)){
    print(paste("Iteration: ",i,"/",nrow(train_dates),sep=''))
    # Splitting train and test data
    df_all_train = df_all %>% filter(as.Date(as.character(Start)) >= train_dates$Start[[i]]) %>% filter(as.Date(as.character(Start)) <= train_dates$End[[i]])
    df_all_test = df_all %>% filter(as.Date(as.character(Start)) >= test_dates$Start[[i]]) %>% filter(as.Date(as.character(Start)) <= test_dates$End[[i]])
    # print(df_all_test)
    
    
    # df_poisson_train = df_poisson %>% filter(as.Date(as.character(Start)) >= train_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= train_dates[[i]][[2]])
    #df_poisson_test = df_poisson %>% filter(as.Date(as.character(Start)) >= test_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= test_dates[[i]][[2]])
    
    #df_daily_train = df_daily %>% filter(as.Date(as.character(Start)) >= train_dates[[i]][[1]]) %>% filter(as.Date(as.character(Start)) <= train_dates[[i]][[2]])
    df_daily_test = df_daily %>% filter(Start_date >= test_dates$Start[[i]]) %>% filter(Start_date <= test_dates$End[[i]])
    
    ##### FORMULA
    out = ppgam(fmla, as.data.frame(df_all_train))
    # out = ppgam(fmla, knots=list(tod32=seq(7,29,2)), as.data.frame(df_all_train))
    # out = ppgam(fmla, knots=list(tod48=seq(10,40,2)), as.data.frame(df_all_train))
    # out = ppgam(fmla, knots=list(tod96=seq(24,72,2)), as.data.frame(df_all_train))
    
    
    print(out)
    
    #### Simulation
    
    # lambdafit = as.vector(exp(covar_test %*% out@fullcoef[2:length(out@fullcoef)] + out@fullcoef[1]))#[1:nrow(data)]
    lambdafit_train = as.vector(exp(predict(out,as.data.frame(df_all_train))))
    lambdafit_test = as.vector(exp(predict(out,as.data.frame(df_all_test))))
    
    # Simulation algorithm
    print("Simulation")
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
    
    # # Getting number of sessions per timestep
    # 
    # df_step_test = df_all_test %>%
    #   mutate(Start = floor_date(Start,paste(as.character(step),"mins")),
    #          step = (hour(Start)*60+minute(Start)) %/% step) %>%
    #   group_by(Start) %>%
    #   summarise(sessions = sum(event),
    #             step=mean(step))
    # 
    # # print(head(df_step_test))
    # 
    # simu_arrival_step = simu_arrival_test %>%  
    #   mutate(Start = floor_date(Start,paste(as.character(step),"mins"))) %>%
    #   group_by(Start) %>%
    #   summarise(pred = n())
    # 
    # # print(head(simu_arrival_step))
    # 
    # final_step = left_join(df_step_test,simu_arrival_step,by="Start") %>%
    #   replace_na(list(pred=0))
    # # print(head(final_step))
    # 
    # plot(final_step$Start,final_step$sessions,col='green',ylim=range(final_step$sessions,final_step$pred),type='b')
    # points(final_step$Start,final_step$pred,col='blue',type='b')
    
    # metrics_all = bind_rows(metrics,tibble(mae=mae(df_step_test$sessions,simu_arrival_step$sessions),
    #                                        rmse=rmse(df_step_test$sessions,simu_arrival_step$sessions)))
    
    
    res_train[[i]] = simu_arrival_train
    res_test[[i]] = simu_arrival_test
    lambdas_test[[i]] = tibble(Start = df_all_test$Start,lambda = lambdafit_test)
    # agg_step[[i]] = final_step %>% mutate(name=model_name)
    
    
  }
  # return(list(res_train,res_test,agg_step))
  return(list(res_train,res_test,lambdas_test))
}

nhpp_splines_dow = function(df_all,fmla,train_dates,test_dates,simu_type="thinning",step=30){
  days = list("Mon","Tue","Wed","Thu","Fri")
  res = list()
  for (i in 2:6){
    print(paste0("Training model: ",i))
    df_all_day = df_all %>% filter(Day==i)
    temp = nhpp_splines(df_all_day,fmla,train_dates,test_dates,simu_type="thinning",step=step,model_name=days[[i-1]])
    res[[i-1]] = temp
  }
  # print(res)
  
  final = list(list(),list(),list())
  for (j in 1:length(test_dates)){
    temp_1 = NULL
    temp_2 = NULL
    temp_3 = NULL
    for (n in 1:5){
      # print(res[[n]][[1]][[j]])
      # print(res[[n]][[2]][[j]])
      temp_1 = bind_rows(temp_1,res[[n]][[1]][[j]])
      temp_2 = bind_rows(temp_2,res[[n]][[2]][[j]])
      temp_3 = bind_rows(temp_3,res[[n]][[3]][[j]])
    }
    final[[1]][[j]] = temp_1 %>% arrange(Start)
    final[[2]][[j]] = temp_2 %>% arrange(Start)
    final[[3]][[j]] = temp_3 %>% arrange(Start)
  }
  
  return(final)
  
}

nhpp_splines_tod = function(df_all,fmla,train_dates,test_dates,simu_type="thinning"){
  
  df_all = df_all %>% filter(Day %in% c(2,3,4,5,6))
  res = list()
  ind = 0
  for (i in 6:31){
    ind = ind + 1
    print(paste0("Training model: ",i))
    df_all_tod = df_all %>% filter(tod32==i)
    temp = nhpp_splines(df_all_tod,fmla,train_dates,test_dates,simu_type="thinning")
    res[[ind]] = temp
  }
  # print(res)
  
  final = list(list(),list())
  for (j in 1:length(test_dates)){
    temp_1 = NULL
    temp_2 = NULL
    for (n in 1:length(res)){
      # print(res[[n]][[1]][[j]])
      # print(res[[n]][[2]][[j]])
      temp_1 = bind_rows(temp_1,res[[n]][[1]][[j]])
      temp_2 = bind_rows(temp_2,res[[n]][[2]][[j]])
    }
    final[[1]][[j]] = temp_1 %>% arrange(Start)
    final[[2]][[j]] = temp_2 %>% arrange(Start)
  }
  
  return(final)
  
}

