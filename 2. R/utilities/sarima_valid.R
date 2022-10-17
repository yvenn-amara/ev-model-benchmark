library(tidyverse)
library(lubridate)
library(forecast)
args=commandArgs(TRUE)

dataset = args[1]

train_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/train_dates",sep=''))

start_date = train_dates[[1]][[1]]
end_date = train_dates[[1]][[2]]
tz = "UTC"
window=read.csv(paste("2. R/1. Outputs/",dataset,"/best_window.csv",sep=""))[['window']]
source("2. R/0_Utility.R")

train = df_daily %>% filter(wday(as.Date(Start_date)) %in% c(2,3,4,5,6))
mod = auto.arima(ts(train$sessions, frequency = 5),
                 stepwise = FALSE,
                 max.p = 5,
                 max.q = 5,
                 max.P = 5,
                 max.Q = 5,
                 max.order = 10,
                 max.d = 5,
                 max.D = 5,
                 seasonal=TRUE,trace=T, ic = "bic")

acf(mod$residuals)

lag.max = 10
final = tibble(p.value = Box.test(mod$residuals, lag = lag.max, type = "Ljung-Box", fitdf=length(mod$coef))$p.value,
       test = p.value > 0.05)

# Saving test result
write_csv(final,paste("2. R/1. Outputs/",dataset,"/sarima_valid.csv",sep=""))

