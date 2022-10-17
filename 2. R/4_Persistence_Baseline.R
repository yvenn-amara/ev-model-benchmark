library(tidyverse)
library(lubridate)

args=commandArgs(TRUE)
dataset = args[1]
duration = args[2]

test_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/test_dates",sep=''))

all = read.csv(paste("3. Python/1. Outputs/",dataset,"/observed_all_",duration,".csv",sep=""))
all$Start = as.POSIXct(all$Start, format='%Y-%m-%d %H:%M:%S', tz='UTC')

if (duration == "Charge.Duration"){
  # if (dataset == "domestics"){
  #   all = all %>%
  #     mutate(pred = dplyr::lag(Power,60*24*7))
  # } else {
  #   all = all %>%
  #     mutate(pred = ifelse(wday(Start)==2,dplyr::lag(Power,60*24*3),dplyr::lag(Power,60*24)))
  # }
  all = all %>%
    mutate(pred = ifelse(wday(Start)==2,dplyr::lag(Power,60*24*3),dplyr::lag(Power,60*24)))
  
  name="load_curve"
  final = all %>% 
    filter(as.Date(Start) >= as.Date(test_dates[[1]][[1]])) %>% 
    filter(as.Date(Start) <= as.Date(test_dates[[length(test_dates)]][[2]])) %>%
    select(Start,pred,curve,date) %>%
    rename("Power"="pred") %>%
    mutate(curve="persistence")
} else if (duration == "Park.Duration"){
  
  # if (dataset == "domestics"){
  #   all = all %>%
  #     mutate(pred = dplyr::lag(Occupancy,60*24*7))
  # } else {
  #   all = all %>%
  #     mutate(pred = ifelse(wday(Start)==2,dplyr::lag(Occupancy,60*24*3),dplyr::lag(Occupancy,60*24)))
  # }

  all = all %>%
    mutate(pred = ifelse(wday(Start)==2,dplyr::lag(Occupancy,60*24*3),dplyr::lag(Occupancy,60*24)))

  name="occupancy"
  final = all %>% 
    filter(as.Date(Start) >= test_dates[[1]][[1]]) %>% 
    filter(as.Date(Start) <= test_dates[[length(test_dates)]][[2]]) %>%
    select(Start,pred,curve,date) %>%
    rename("Occupancy"="pred") %>%
    mutate(curve="persistence")
}
                                                                                  
write.csv(final,paste("3. Python/1. Outputs/",dataset,"/persistence_",name,".csv",sep=""),row.names = FALSE)
