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
source("2. R/0_Utility.R")

# df_all = df_all %>% filter(hour(Start) %in% seq(6,22,1))
train_ind = df_all %>% slice_head(prop = 0.8) %>% rownames(.) %>% as.numeric(.)

train = df_all[train_ind,]
test = df_all[-train_ind,]

# Creating the grid
if (dataset == "paris"){
  gs = list(fmla = c(as.formula("smooth_event ~ s_mod"),
                     as.formula("smooth_event ~ s_mod + s_tod32"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + s_trend"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + s_trend + lag_D1_adj + lag_D7"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + s_trend + roll_lag_D1_adj + roll_lag_D7"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + s_trend + agglag_D1_adj + agglag_D7")),
            num.trees = c(100, 200, 300, 500),
            mtry = c(1,2,3,4)) %>% 
    cross_df() %>%
    mutate(terms = unlist(map(fmla,function(x){length(attr(terms(x), "term.labels"))}))) %>%
    filter(mtry<=terms) %>%
    mutate(loss = as.numeric(NA),
           model = list(NA))
} else {
  gs = list(fmla = c(as.formula("smooth_event ~ s_mod"),
                     as.formula("smooth_event ~ s_mod + s_tod32"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + winter"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + winter + s_trend"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + winter + s_trend + lag_D1_adj + lag_D7"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + winter + s_trend + roll_lag_D1_adj + roll_lag_D7"),
                     as.formula("smooth_event ~ s_mod + s_tod32 + Day + winter + s_trend + agglag_D1_adj + agglag_D7")
                     ),
            num.trees = c(100, 200, 300, 500),
            mtry = c(1,2,3,4)) %>% 
    cross_df() %>%
    mutate(terms = unlist(map(fmla,function(x){length(attr(terms(x), "term.labels"))}))) %>%
    filter(mtry<=terms) %>%
    mutate(loss = as.numeric(NA),
           model = list(NA))
}

for (i in 1:nrow(gs)){
  print(paste0("fmla: ",format(gs[['fmla']][[i]]),"; num.trees: ",gs[['num.trees']][i],"; mtry: ",gs[['mtry']][i]))
  rf = ranger(gs[['fmla']][[i]], data=train, num.trees=gs[['num.trees']][i], mtry=gs[['mtry']][i])
  lambda_pred = predict(rf, data=test)$prediction
  gs[['loss']][[i]] = rmse(test$smooth_event,lambda_pred)
}

gs_final = gs[which.min(gs[['loss']]),]
gs_final[['model']][[1]] = ranger(gs_final[['fmla']][[1]], data=train, num.trees=gs_final[['num.trees']], mtry=gs_final[['mtry']])
print(gs_final)
print(gs_final[['fmla']])

final = NULL
Nsimu = 100
for (j in 1:Nsimu){
  if (j %% 25 == 0){print(paste0("Iteration: ",j))}
  final = bind_rows(final,valid_nhpp(df_all=test,lambda=predict(gs_final[['model']][[1]], data=test)$prediction))
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

# gs_final[['loss']] = mean(final$rmse)
gs_final[['res_mean']] = mean(final$res_mean)
gs_final[['res_var']] = mean(final$res_var)
gs_final[['bound_inf']] = bound_inf
gs_final[['bound_sup']] = bound_sup
gs_final[['test']] = s_1 & s_2 & t


# Saving best configuration
write_csv(gs_final,paste("2. R/1. Outputs/",dataset,"/best_config_nhpp_rf.csv",sep=""))
saveRDS(gs_final, paste("2. R/1. Outputs/",dataset,"/best_config_nhpp_rf.rds",sep=""))


