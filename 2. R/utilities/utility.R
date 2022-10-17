library(zoo)

#### Data Prep

generate_toy = function(start,end,step){
  year_start = floor_date(start, "year")
  year_end = ceiling_date(end, "year")
  dates = tibble(dates=seq(year_start,year_end,step))
  toy = NULL
  for (i in unique(year(dates$dates))){
    temp = dates %>%
      filter(year(dates) == i) %>%
      nrow()
    toy = c(toy,seq(0,1,length=temp))
  }
  return(dates %>% mutate(toy=toy))
}

df = read.csv(paste("1. Preprocessed Data/",dataset,".csv",sep=''))

### CHECK TIMEZONES BEFORE RUNNING
df = df %>%
  filter(Weekend == 0) %>%
  #filter(isHoliday(Start, businessOnly = FALSE) == 0) %>%
  mutate(Start = floor_date(as.POSIXct(Start,"%Y-%m-%d %H:%M:%S",tz=tz),"1 min"), 
         #End = floor_date(as.POSIXct(End,"%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles"),"1 min"),
         Start_Date = as.Date(Start)#,
         #End_Date = as.Date(End)
         ) %>%
  filter(as.Date(Start) >= start_date) %>%
  filter(as.Date(Start) <= end_date) %>%
  arrange(Start)

# window = 60 #60  # size of sliding window in minutes
filter = array(1/window, dim=window)
boundaries = read.csv(paste("2. R/temp/",dataset,"/boundaries.csv",sep=""))

df_all = df %>%
  left_join(tibble(Start=seq(floor_date(min(.$Start),unit='day'),ceiling_date(max(.$Start),unit='day')-1*60,"1 min")),.) %>%
  filter(wday(Start) %in% c(2,3,4,5,6)) %>%
  mutate(Start_Date = date(Start),
         Day = factor(wday(Start)),
         tod = hour(Start),
         winter = ifelse(month(Start)%in% c(10,11,12,1,2,3),1,0),
         tod32 = ((hour(Start) + minute(Start)/60)*100) %/% 75,
         tod48 = hour(Start) + (minute(Start) %/% 30),
         tod96 = hour(Start) + (minute(Start) %/% 15),
         mod = hour(Start)*60 + minute(Start),
         night = ifelse(hour(Start) %in% c(23,0,1,2,3,4,5),1,0),
         event=ifelse(is.na(Arrival)==F,1,0),
         smooth_event=stats::filter(event,  filter=filter, method="convolution",  circular=T),
         const = 1,
         lag_D1 = dplyr::lag(event, n=60*24),
         lag_D7 = dplyr::lag(event, n=60*24*5),
         lag_D3 = dplyr::lag(event, n=60*24*3),)

if (boundaries$bound_inf < boundaries$bound_sup){
  df_all = df_all %>% filter(mod > boundaries$bound_inf, mod < boundaries$bound_sup)
  print(sort(unique(df_all$mod)))
  df_all = df_all %>%  
    dplyr::select(Start,Day,mod,tod,winter,tod32,tod48,tod96,night,const,event,smooth_event,Start_Date,lag_D1,lag_D3,lag_D7) %>%
    left_join(.,generate_toy(min(.$Start),max(.$Start), step='1 mins'), by=c("Start"="dates")) %>% 
    mutate(trend = seq(1,nrow(.),1),
           s_mod = (mod-min(.$mod))/(max(.$mod)-min(.$mod)),
           s_tod32 = (tod32-min(.$tod32))/(max(.$tod32)-min(.$tod32)),
           roll_lag_D1 = rollsum(lag_D1, 90, align= "center", fill = NA),
           roll_lag_D3 = rollsum(lag_D3, 90, align= "center", fill = NA),
           roll_lag_D7 = rollsum(lag_D7, 90, align= "center", fill = NA)) %>% # Creating lag of events
    mutate(s_trend = (trend-min(.$trend))/(max(.$trend)-min(.$trend))) %>%
    na.omit(.)
} else if (boundaries$bound_inf > boundaries$bound_sup){
  df_all = df_all %>% filter(mod > boundaries$bound_inf | mod < boundaries$bound_sup)
  print(sort(unique(df_all$mod)))
  
  times_list_inf = unique((df_all %>% filter(mod > boundaries$bound_inf))$mod) %>% tibble(mod=.,t_mod=seq(1,length(.),1))
  times_list_sup = unique((df_all %>% filter(mod < boundaries$bound_sup))$mod) %>% tibble(mod=.,t_mod=seq(length(times_list_inf),length(times_list_inf)+length(.)-1,1))
  times = bind_rows(times_list_inf,times_list_sup) %>% mutate(s_mod=(t_mod-min(.$t_mod))/(max(.$t_mod)-min(.$t_mod))) %>% dplyr::select(mod,s_mod)
  
  times_list_inf_32 = unique((df_all %>% filter(mod > boundaries$bound_inf))$tod32) %>% tibble(tod32=.,t_tod32=seq(1,length(.),1))
  times_list_sup_32 = unique((df_all %>% filter(mod < boundaries$bound_sup))$tod32) %>% tibble(tod32=.,t_tod32=seq(length(times_list_inf_32),length(times_list_inf_32)+length(.)-1,1))
  times_32 = bind_rows(times_list_inf_32,times_list_sup_32) %>% mutate(s_tod32=(t_tod32-min(.$t_tod32))/(max(.$t_tod32)-min(.$t_tod32))) %>% dplyr::select(tod32,s_tod32)
  
  
  df_all = df_all %>%  
    dplyr::select(Start,Day,mod,tod,winter,tod32,tod48,tod96,night,const,event,smooth_event,Start_Date,lag_D1,lag_D3,lag_D7) %>%
    left_join(.,generate_toy(min(.$Start),max(.$Start), step='1 mins'), by=c("Start"="dates")) %>% 
    left_join(.,times, by=c("mod")) %>%
    left_join(.,times_32, by=c("tod32")) %>%
    mutate(trend = seq(1,nrow(.),1),
           roll_lag_D1 = rollsum(lag_D1, 90, align= "center", fill = NA),
           roll_lag_D3 = rollsum(lag_D3, 90, align= "center", fill = NA),
           roll_lag_D7 = rollsum(lag_D7, 90, align= "center", fill = NA)) %>% # Creating lag of events
    mutate(s_trend = (trend-min(.$trend))/(max(.$trend)-min(.$trend))) %>%
    na.omit(.)
}

# Creating aggregated lags
agglags = df_all %>%
  # group_by(Start_Date,tod32) %>%
  group_by(Start_Date,tod) %>%
  summarise(agglag_D1 = sum(lag_D1),
            agglag_D7 = sum(lag_D7),
            agglag_D3 = sum(lag_D3))

df_all = df_all %>%
  left_join(.,agglags,by=c("Start_Date","tod")) %>%
  mutate(lag_D1_adj = ifelse(Day==2,lag_D3,lag_D1),
         agglag_D1_adj = ifelse(Day==2,agglag_D3,agglag_D1),
         roll_lag_D1_adj = ifelse(Day==2,roll_lag_D3,roll_lag_D1))

df_poisson = df %>%
  filter(Start >= min(df_all$Start)) %>%
  mutate(events = as.numeric(difftime(Start,min(df_all$Start),units="mins")) ) %>%
  dplyr::select(Start,events) %>%
  left_join(.,df_all,by="Start")

df_daily = df %>%
  mutate(Start_date = as.Date(as.character(Start))) %>%
  group_by(Start_date) %>%
  summarise(sessions = n()) %>%
  ungroup(.)

mae = function(y_true,y_pred){
  return(mean(abs(y_true-y_pred)))
}

mape = function(y_true,y_pred){
  return( 100*mean(abs((y_true - y_pred)/y_true)) )
}

rmse = function(y_true,y_pred){
  return(sqrt(mean((y_true-y_pred)**2)))
}

imp = function(y_true,y_pres,y_pred){
  return(mae(y_true,y_pres)/mae(y_true,y_pred))
}

##### Simulating non homoegnous poisson process

f = function(t,a,b){
  #return(a*dnorm(t,9,1) + (1-a-b)*dnorm(t,12.5,1.5) + b*dnorm(t,19,2) ) 
  return(a*dnorm(t,12,1) + (1-a-b)*dnorm(t,16,1.5) + b*dnorm(t,25,2) ) 
}

g = function(data,week,weekend){
  temp = tibble(dow = data) %>%
    mutate(res = ifelse(dow %in% c(2,3,4,5,6),week,weekend))
  return(temp$res)
}

# g = function(t,a,b){
#   return(a*dnorm(t,2,1) + (1-a-b)*dnorm(t,4,1) + b*dnorm(t,6,1)) 
# }
# g = function(t_seq,mon,tue,wed,thu,fri,sat,sun){
#   res = c()
#   for (i in 1:length(t_seq)){
#     if (t_seq[i]==2){res = c(res,mon)}
#     else if (t_seq[i]==3){res = c(res,tue)}
#     else if (t_seq[i]==4){res = c(res,wed)}
#     else if (t_seq[i]==5){res = c(res,thu)}
#     else if (t_seq[i]==6){res = c(res,fri)}
#     else if (t_seq[i]==7){res = c(res,sat)}
#     else if (t_seq[i]==1){res = c(res,sun)}
#   }
# return(res)
# }

lambda_tod32 = function(tod32,dow){
  return(exp(f(tod32,0.5,0.5) + g(dow,0.2,0) -2))
}

# lambda = function(t){
#   return(exp(f((t %/% 45),0.5,0.5) -2))
# }

# Underlying intensity function
# lambda = exp(f(data$tod,0.4,0.4) + g(data$dow,0.4,0.4)-2)
# lambda = exp(f(data$tod32,0.5,0.5) + g(data$dow,0.2,0.15,0.1,0.15,0.2,0.05,0.05)-2)
# lambda = exp(f(data$tod32,0.5,0.5) #+ g(data,0.2,0)
#              -2
# )

# Fixed window mode
thin_sim = function(lambda_tod32, lambda_plus,Start, T_end){
  # Simulate the homogenous process
  m = round(3*T_end*lambda_plus) # Taking a number of events much larger than the expected value in order to ensure we overshoot the boundary
  u = runif(m,0,1)
  t = -1/lambda_plus*log(u) # inter arrival times
  s = cumsum(t) # arrival times
  s = s[s<=T_end] # trimming events that happen after then end of our window
  nstar = length(s)
  
  # Selecting which event we are keeping with the appropriate probability
  w = runif(nstar,0,1)
  events = Start + s*60
  s_tod32 = (((hour(events) + minute(events)/60)*100) %/% 75)
  s_dow = wday(events)
  Ind = (w <= lambda_tod32(s_tod32,s_dow)/lambda_plus)
  
  # Returning the arrival times of the non homogenosu poisson process
  return(round(s[Ind]))
  
}

skimming = function(coef,num_skim){
  
  if (num_skim>0){
    for (i in 1:num_skim){
      temp = abs(as.vector(coef))
      ind = which(temp == min(temp[temp > 0]))
      coef[ind] = 0
    }
  }
  return(coef)
}

optimal_coef = function(data,mod){
  
  mape = mape(data$lambda,mod@lambdafit[1:nrow(data)])
  opt = 0
  for (i in 1:length(mod@fullcoef)){
    coef = skimming(mod@fullcoef,i)
    newlambda = as.vector(exp(mod@covariates %*% coef[2:length(mod@fullcoef)] + coef[1]))[1:nrow(data)]
    if (mape(data$lambda,newlambda)<mape){
      opt = i
      mape = mape(data$lambda,newlambda)
    }
  }
  
  return(skimming(mod@fullcoef,opt))
}


thin_sim_afterfit = function(lambda, lambda_plus, T_end){
  # Simulate the homogenous process
  m = round(3*T_end*lambda_plus) # Taking a number of events much larger than the expected value in order to ensure we overshoot the boundary
  u = runif(m,0,1)
  t = -1/lambda_plus*log(u) # inter arrival times
  s = cumsum(t) # arrival times
  s = s[s<=T_end] # trimming events that happen after then end of our window
  nstar = length(s)
  
  # Selecting which event we are keeping with the appropriate probability
  w = runif(nstar,0,1)
  Ind = (w <= lambda[ceiling(s)]/lambda_plus)
  
  # Returning the arrival times of the non homogenous poisson process
  return(round(s[Ind]))
  
}

save_dates = function(ll){
  aa =do.call(rbind.data.frame, ll)
  colnames(aa) = c("Start","End")
  return(aa)
}

theme_Publication = function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            #panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            #legend.key = element_rect(colour = NA),
            # legend.position = c(.95, .95),
            # legend.justification = c("right", "top"),
            # legend.box.just = "right",
            legend.position = "right",
            legend.direction = "vertical",
            #legend.key.size= unit(0.2, "cm"),
            #legend.key.size= unit(0.75, "cm"),
            legend.key.size=unit(3,"line"),
            # legend.margin = margin(6, 6, 6, 6),
            legend.title = element_text(face="italic"),
            panel.grid.major = element_line(colour = "black", size = 0.2,linetype="dashed"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

valid_nhpp = function(df_all,lambda,step="15 mins"){
  simu = thin_sim_afterfit(lambda, 1,nrow(df_all))
  df_all = df_all %>% mutate(time = seq(1,nrow(df_all)))
  final = right_join(tibble(time=simu,pred_event=1),df_all,by="time") %>%
    mutate(pred_event = ifelse(is.na(pred_event),0,pred_event),
           bucket = ceiling_date(Start,step)) %>%
    group_by(bucket) %>%
    summarise(event = sum(event),pred_event = sum(pred_event))
  return(tibble(rmse=rmse(final$event,final$pred_event),
                mae=mae(final$event,final$pred_event),
                res_mean = mean(final$pred_event - final$event),
                res_var = var(final$pred_event - final$event),
  ))
}
