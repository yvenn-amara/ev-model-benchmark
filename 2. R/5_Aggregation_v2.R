require(opera)
require(tidyverse)
require(lubridate)

args=commandArgs(TRUE)
dataset=args[1]
duration = args[2]

if (duration=="Charge.Duration"){
  type="load"
} else if (duration=="Park.Duration"){
  type="occupancy"
}

path = paste("3. Python/1. Outputs/",dataset,"/",sep="")
if (type=="load"){
  # temp = list.files(path = path, pattern = "[^(AGG_EXP)]*(load_curve.csv)")
  temp = list.files(path = path, pattern = "[^(AGG_EXP)](*)(_load_curve.csv)")
  print(temp)
} else if (type=="occupancy"){
  temp = list.files(path = path, pattern = "[^(AGG_EXP)](*)(_occupancy.csv)")
  print(temp)
}

myfiles = lapply(paste0(path,temp), read.csv)

###### Remove duplicates as a temporary fix
for (j in 1:length(myfiles)){
  myfiles[[j]] =myfiles[[j]] %>% distinct(Start, .keep_all = TRUE)
}


observed = read.csv(paste("3. Python/1. Outputs/",dataset,"/","observed_test_",duration,".csv",sep="")) %>%
  mutate(Start = as.POSIXct(Start)) %>%
  filter(wday(Start) %in% c(2,3,4,5,6))

if (type=="load"){
  df = bind_rows(myfiles) %>%
    pivot_wider(names_from = curve,
                values_from = Power)  %>%
    mutate(Start = as.POSIXct(Start)) %>%
    inner_join(.,observed %>% dplyr::select(Start,Power) %>% rename("observed"="Power")) %>%
    mutate(instant = hour(Start)*60+minute(Start))
} else if (type=="occupancy"){
  df = bind_rows(myfiles) %>%
    pivot_wider(names_from = curve,
                values_from = Occupancy)  %>%
    mutate(Start = as.POSIXct(Start)) %>%
    inner_join(.,observed %>% dplyr::select(Start,Occupancy) %>% rename("observed"="Occupancy")) %>%
    mutate(instant = hour(Start)*60+minute(Start))
}

df = df %>% rename(
              "GAM"="GAM",
              "GAM_err"="GAM_ARIMA",
              "RF"="RF",
              "RF_err"="RF_ARIMA",
              "persistence"="persistence",
              "GRU_MM"="GRU+LMM",
              "GRU_MM_err"="GRU+LMM_error",
              "NHPP_RF_MR"="NHPP_RF+CLMM",
              "NHPP_RF_MR_err"="NHPP_RF+CLMM_error",
              "NHPP_GAM_MR"="NHPP_splines_dow+CLMM",
              "NHPP_GAM_MR_err"="NHPP_splines_dow+CLMM_error",
              "SARIMA_MM"="SARIMA+LMM",
              "SARIMA_MM_err"="SARIMA+LMM_error",
              "SARIMA_NAF"="NAF-SARIMA")

# relevant_models = c("GAM_err","RF_err",
#                     "GRU_MM_err","SARIMA_MM_err",
#                     "NHPP_GAM_MR_err","NHPP_RF_MR_err")

relevant_models = c("GAM_err","RF_err","GAM","RF",
                    "GRU_MM","GRU_MM_err","SARIMA_MM","SARIMA_MM_err",
                    "NHPP_GAM_MR","NHPP_RF_MR","NHPP_GAM_MR_err","NHPP_RF_MR_err",
                    "persistence")

# resid = bind_cols(df['Start'],df[['observed']] - df[relevant_models]) %>%
#   mutate(mod=hour(Start)*60+minute(Start)) %>%
#   dplyr::select(-Start) %>%
#   group_by(mod) %>%
#   summarise_all(mean) %>%
#   pivot_longer(relevant_models)
# 
# ggplot(data = resid, aes(x=mod)) +
#   geom_line(aes(y=value,col=name))

df = df %>%
  dplyr::select(Start,date,instant,observed,relevant_models)

ML_pol_instant = function(df,mod,type){
  df_instant = df %>%
    filter(instant == mod)
  
  X = df_instant %>%
    dplyr::select(-Start, -date, -instant) %>% dplyr::select(-observed) %>% as.matrix(.)
  Y = df_instant %>%
    dplyr::select(-Start, -date, -instant) %>% dplyr::select(observed) %>% as.matrix(.)
           
  MLpol = mixture(Y = Y, experts = X, model = "MLpol", loss.type = "square")
  
  if (type == "load"){
    return(df_instant %>% dplyr::select(Start,date) %>%
             mutate(Power=as.vector(MLpol$prediction),
                    curve="AGG_EXP") %>%
             dplyr::select(Start,Power,curve,date) %>% 
             bind_cols(.,tibble(as.data.frame(MLpol$weights))))
  } else if (type == "occupancy"){
    return(df_instant %>% dplyr::select(Start,date) %>%
             mutate(Occupancy=as.vector(MLpol$prediction),
                    curve="AGG_EXP") %>%
             dplyr::select(Start,Occupancy,curve,date) %>% 
             bind_cols(.,tibble(as.data.frame(MLpol$weights))))
             
  }
  
}

final = map_dfr(unique(df$instant),ML_pol_instant,df=df,type=type) %>% arrange(Start)
weights = as.data.frame(apply(final[relevant_models],MARGIN=2,mean)) %>%
  mutate(model = rownames(.),
         dataset= dataset,
         target= type) %>%
  tibble(.)
colnames(weights) = c("mean","model","dataset","target")
write_csv(weights,paste("2. R/1. Outputs/all/agg_weights/",dataset,"_",type,".csv",sep=""))

# apply(final[relevant_models],MARGIN=2,sd)

critical_instant = which.max((df %>% group_by(instant) %>% summarise(mean = mean(observed)))$mean)-1

plot_MLpol = function(df,critical_instant,type,type_plot="all",dynamic=TRUE){
  
  # print(critical_instant)
  
  df_instant = df %>%
    filter(instant == critical_instant)
  
  X = df_instant %>%
    dplyr::select(-Start, -date, -instant) %>% dplyr::select(-observed) %>% as.matrix(.)
  Y = df_instant %>%
    dplyr::select(-Start, -date, -instant) %>% dplyr::select(observed) %>% as.matrix(.)
  AGG = mixture(Y = Y, experts = X, model = "MLpol", loss.type = "square")
  return(plot(AGG,type=type_plot,dynamic=dynamic))
  # return(AGG)
  
}

# pdf(file = paste("2. R/1. Outputs/",dataset,"/agg_plot_weights_",dataset,".pdf",sep=""),
#     width = 5, 
#     height = 4) 
# agg = plot_MLpol(df,critical_instant,type,type_plot="plot_weight",dynamic=F)
# dev.off()

library(htmltools)
save_html(plot_MLpol(df,critical_instant,type), paste0(path,"AGG_plot_",type,format(df[df$instant==critical_instant,][1,]$Start,"_%H_%M"),".html"))


if (type == "load"){
  write.csv(final %>% dplyr::select(Start,Power,curve,date),paste(path,"AGG_EXP_load_curve.csv",sep=""),row.names = F)
} else if (type == "occupancy"){
  write.csv(final %>% dplyr::select(Start,Occupancy,curve,date),paste(path,"AGG_EXP_occupancy.csv",sep=""),row.names = F)
}
