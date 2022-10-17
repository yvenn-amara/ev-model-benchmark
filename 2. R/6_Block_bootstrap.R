require(tidyverse)
require(lubridate)
`%ni%` <- Negate(`%in%`)

args=commandArgs(TRUE)
dataset=args[1]
duration = args[2]
selection = args[3]

# train_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/train_dates",sep=''))
test_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/test_dates",sep=''))
#### Metrics Functions

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

imp2 = function(y_true,y_pres,y_pred){
  return(rmse(y_true,y_pres)/rmse(y_true,y_pred))
}

metrics_boot = function(df, model, duration,
                           block_length,
                           n_samples = 250){
  
  # evaluation set size
  n = nrow(df)
    
    n_block = n %/% block_length
    
    # sample observation index
    
    boot_samples_idx =
      purrr::map(1:n_samples, function(i) {
        this_idx = sample.int(n_block, replace = T)
        purrr::map(this_idx, function(idx) {
          ((idx - 1) * block_length + 1):min(n, idx * block_length)
        }) %>% unlist() %>% head(n = n) # keep size = n
      })
    
    # get obs
    boot_samples = purrr::map(boot_samples_idx, function(this_idx) {
      df %>% dplyr::slice(this_idx)
    })
    
    # compute metrics
    metrics = map_dfr(boot_samples, function(this_sample) {
      tibble(mae = mae(this_sample$observed, this_sample[[model]]),
             rmse = rmse(this_sample$observed, this_sample[[model]]),
             imp  = imp(this_sample$observed, this_sample$persistence, this_sample[[model]]),
             imp2  = imp2(this_sample$observed, this_sample$persistence, this_sample[[model]]))
    })
  
  metrics$model = model
  # metrics$dataset = dataset
  
  return(metrics)
}

#### Treatment


if (duration=="Charge.Duration"){
  type="load_curve"
} else if (duration=="Park.Duration"){
  type="occupancy"
}

path = paste("3. Python/1. Outputs/",dataset,"/",sep="")
if (type=="load_curve"){
  temp = list.files(path = path, pattern = "(*)(_load_curve.csv)")
  print(temp)
} else if (type=="occupancy"){
  temp = list.files(path = path, pattern = "(*)(_occupancy.csv)")
  print(temp)
}

myfiles = lapply(paste0(path,temp), read.csv)
observed = read.csv(paste("3. Python/1. Outputs/",dataset,"/","observed_test_",duration,".csv",sep="")) %>%
  mutate(Start = as.POSIXct(Start)) %>%
  filter(wday(Start) %in% c(2,3,4,5,6))

for (j in 1:length(myfiles)){
  myfiles[[j]] =myfiles[[j]] %>% distinct(Start, .keep_all = TRUE)
}

if (type=="load_curve"){
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

# models = colnames(df)
# models = models[models %ni% c("Start","date","observed","instant") ]

models = c('AGG_EXP','GAM_ARIMA','GAM','RF_ARIMA','RF',
           'GRU+LMM','GRU+LMM_error','SARIMA+LMM','SARIMA+LMM_error',
           'NHPP_splines_dow+CLMM_error','NHPP_splines_dow+CLMM',
           'NHPP_RF+CLMM','NHPP_RF+CLMM_error','persistence')

df = df %>% filter(wday(Start) %in% c(2,3,4,5,6))

if (selection=="last"){
  df = df %>% filter(date(Start) >= test_dates[[length(test_dates)]][[1]]) %>%
    filter(date(Start) <= test_dates[[length(test_dates)]][[2]])
}

final = map_dfr(models, metrics_boot, df=df,duration=duration,block_length=1440)
final %>% group_by(model) %>% summarise_all(mean) %>% arrange(-imp)
write.csv(final,paste("2. R/1. Outputs/all/", dataset,"_bb_metrics_",type,".csv",sep=""),row.names=FALSE)

