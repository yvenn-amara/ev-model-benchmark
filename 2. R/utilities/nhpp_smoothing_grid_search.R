##### 1. Importing functions libraries #####
require(tidyverse)
require(lubridate)
require(ranger)
args=commandArgs(TRUE)

dataset = args[1]

train_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/train_dates",sep=''))

start_date = train_dates[[1]][[1]]
end_date = train_dates[[1]][[2]]
tz = "UTC"
window=read.csv(paste("2. R/1. Outputs/",dataset,"/best_window.csv",sep=""))[['window']]
# window=60
source("2. R/0_Utility.R")

# Creating the grid
gs = list(window = seq(10,120,10),
          sides = c(1,2)) %>% 
  cross_df() %>%
  mutate(loss = as.numeric(NA),
         res_mean = as.numeric(NA),
         res_var = as.numeric(NA),
         bound_inf = as.numeric(NA),
         bound_sup = as.numeric(NA),
         test = NA)   

for (i in 1:nrow(gs)){
  window=gs[['window']][i]
  sides=gs[['sides']][i]
  print(paste0("window: ",window,"; sides: ",sides))
  filter = array(1/window, dim=window)
  df_all %>% mutate(smooth_event = stats::filter(event,  filter=filter, method="convolution", sides=sides, circular=T))
  
  final = NULL
  Nsimu = 100
  for (j in 1:Nsimu){
    if (j %% 25 == 0){print(paste0("Iteration: ",j))}
    final = bind_rows(final,valid_nhpp(df_all,df_all$smooth_event))
  }
  
  # Normality tests
  s_1 = shapiro.test(final$res_mean)$p.value > 0.05
  s_2 = shapiro.test(final$res_var)$p.value > 0.05
  
  # t_stat = sqrt(Nsimu)*mean(final$res_mean)/mean(final$res_var)
  bound_inf = mean(final$res_var)*qt(0.025,Nsimu-1)/sqrt(Nsimu)
  bound_sup = mean(final$res_var)*qt(0.975,Nsimu-1)/sqrt(Nsimu)
  
  # T-test
  t = (bound_inf <= mean(final$res_mean)) & (bound_sup >= mean(final$res_mean))
  
  # P-value
  # 2*(1 - pt(abs(t_stat),Nsimu-1))
  
  gs[['loss']][i] = mean(final$rmse)
  gs[['res_mean']][i] = mean(final$res_mean)
  gs[['res_var']][i] = mean(final$res_var)
  gs[['bound_inf']][i] = bound_inf
  gs[['bound_sup']][i] = bound_sup
  gs[['test']][i] = s_1 & s_2 & t
  
}
print(gs[which.min(gs[['loss']]),])
# Saving best configuration
write_csv(gs[which.min(gs[['loss']]),],paste("2. R/1. Outputs/",dataset,"/best_window.csv",sep=""))

