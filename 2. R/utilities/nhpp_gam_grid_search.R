##### 1. Importing functions libraries #####
require(tidyverse)
require(lubridate)
require(NHPoisson)
require(mgcv)
require(ppgam)
args=commandArgs(TRUE)

dataset = args[1]

train_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/train_dates",sep=''))

start_date = train_dates[[1]][[1]]
end_date = train_dates[[1]][[2]]
tz = "UTC"
window=read.csv(paste("2. R/1. Outputs/",dataset,"/best_window.csv",sep=""))[['window']]
source("2. R/0_Utility.R")

# df_all = df_all %>% filter(hour(Start) %in% seq(6,22,1))
train_ind = df_all %>% slice_head(prop = 0.8) %>% rownames(.) %>% as.numeric(.)

train = df_all[train_ind,]
test = df_all[-train_ind,]

# Creating the grid
if (dataset == "paris"){
  # gs = tibble(fmla = c(as.formula("event ~ s(s_mod, bs='cc')"),
  #                      as.formula("event ~ s(s_mod, bs='cc') + s(s_trend)"),
  #                      as.formula("event ~ s(s_mod, bs='cc') + s(s_trend) + lag_D1_adj"),
  #                      as.formula("event ~ s(s_mod, bs='cc') + s(s_trend) + lag_D7"),
  #                      as.formula("event ~ s(s_mod, bs='cc') + s(s_trend) + lag_D1_adj + lag_D7"),
  #                      as.formula("event ~ s(s_mod, bs='cc') + s(s_trend) + roll_lag_D1_adj + roll_lag_D7"),
  #                      as.formula("event ~ s(s_mod, bs='cc') + s(s_trend) + agglag_D1_adj + agglag_D7"),
  #                      as.formula("event ~ s(s_mod, bs='cc')"),
  #                      as.formula("event ~ s(s_mod,k=30, bs='cc')"),
  #                      as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_trend,k=20)"),
  #                      as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_trend,k=20) + lag_D1_adj + lag_D7"),
  #                      as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_trend,k=20) + roll_lag_D1_adj + roll_lag_D7"),
  #                      as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_trend,k=20) + agglag_D1_adj + agglag_D7")
  # )) %>%
  #   mutate(loss = as.numeric(NA))
  gs = tibble(fmla = c(as.formula("event ~ s(s_mod, bs='cc')"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + s(s_trend)"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + s(s_trend) + lag_D1_adj"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + s(s_trend) + lag_D7"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + s(s_trend) + lag_D1_adj + lag_D7"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + s(s_trend) + roll_lag_D1_adj + roll_lag_D7"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + s(s_trend) + agglag_D1_adj + agglag_D7"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc')"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc')"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + s(s_trend,k=20)"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + s(s_trend,k=20) + lag_D1_adj + lag_D7"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + s(s_trend,k=20) + roll_lag_D1_adj + roll_lag_D7"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + s(s_trend,k=20) + agglag_D1_adj + agglag_D7")
  )) %>%
    mutate(loss = as.numeric(NA))
} else {
  gs = tibble(fmla = c(as.formula("event ~ s(s_mod, bs='cc')"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc')"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + winter"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend)"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend) + lag_D1_adj"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend) + lag_D7"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend) + lag_D1_adj + lag_D7"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend) + roll_lag_D1_adj + roll_lag_D7"),
                       as.formula("event ~ s(s_mod, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend) + agglag_D1_adj + agglag_D7"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc')"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc')"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + winter"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend,k=20)"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend,k=20) + lag_D1_adj + lag_D7"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend,k=20) + roll_lag_D1_adj + roll_lag_D7"),
                       as.formula("event ~ s(s_mod,k=30, bs='cc') + s(s_tod32, bs='cc') + winter + s(s_trend,k=20) + agglag_D1_adj + agglag_D7")
  )) %>%
    mutate(loss = as.numeric(NA))
}

for (i in 1:nrow(gs)){
  print(paste0("fmla: ",format(gs[['fmla']][[i]])))
  
  lambdas = NULL
  
  for (j in 2:6){
    print(paste0("Training model: ",j))
    train_day = train %>% filter(Day==j)
    test_day = test %>% filter(Day==j)
    mod = ppgam(formula = gs[['fmla']][[i]], data = as.data.frame(train_day))
    lambdas = bind_rows(lambdas,tibble(Start=test_day$Start,
                                       pred=as.vector(exp(predict(mod,as.data.frame(test_day))))))
  }
  
  lambdas = lambdas %>% arrange(Start)
  
  final = NULL
  Nsimu = 100
  for (j in 1:Nsimu){
    if (j %% 25 == 0){print(paste0("Iteration: ",j))}
    final = bind_rows(final,valid_nhpp(df_all=test,lambda=lambdas$pred))
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

gs_final = gs[which.min(gs[['loss']]),]
print(gs_final)
print(gs_final[['fmla']])

# Saving best configuration
write_csv(gs_final,paste("2. R/1. Outputs/",dataset,"/best_config_nhpp_gam.csv",sep=""))
saveRDS(gs_final, paste("2. R/1. Outputs/",dataset,"/best_config_nhpp_gam.rds",sep=""))
