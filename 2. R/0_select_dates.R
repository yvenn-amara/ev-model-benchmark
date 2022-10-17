library(tidyverse)
library(lubridate)
library(mgcv)
library(ggplot2)
args=commandArgs(TRUE)

dataset = args[1]
# start_date = "2019-01-02"
# end_date = "2020-01-01"
train_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/train_dates",sep=""))
test_dates = readRDS(paste("2. R/1. Outputs/",dataset,"/test_dates",sep=""))
start_date = train_dates[[1]][[1]]
end_date = train_dates[[1]][[2]]

tz='UTC'
window=60
source("2. R/0_Utility.R")

# plot(df_daily$Start_date,df_daily$sessions,type='b')

# train_dates = list(list("2017-04-03","2017-04-30"),
#                    list("2017-04-03","2017-05-14")
# )
# 
# test_dates = list(list("2017-05-01","2017-05-14"),
#                   list("2017-05-15","2017-05-28")
# )

# saveRDS(train_dates, paste("2. R/1. Outputs/",dataset,"/train_dates",sep=""))
# saveRDS(test_dates, paste("2. R/1. Outputs/",dataset,"/test_dates",sep=""))

### Select relevant times

aa = df_all %>% group_by(mod) %>% summarise(event = sum(event)) %>%
  mutate(smooth = gam(.$event~s(.$mod,bs='cc'))$fitted.values) %>%
  mutate(smooth = ifelse(smooth<0,0,smooth))
# plot(aa)
# hist(aa$event)

# thresholds = aa[aa$smooth < var(aa$smooth),]
thresholds = aa[aa$smooth < max(aa$smooth)*0.075,]

boundaries = read_csv(paste("2. R/1. Outputs/",dataset,"/boundaries.csv",sep="")) %>%
  mutate(color="grey")

bound_a = boundaries$bound_inf
bound_b = boundaries$bound_sup

# boundaries = bind_rows(boundaries,tibble(bound_inf = c(0,bound_a),
#                                          bound_sup = c(bound_b,1440),
#                                          color = "white"))

pdf(file = paste("2. R/1. Outputs/",dataset,"/total_arrivals_",dataset,".pdf",sep=""),   
    width = 5, 
    height = 4) 

ggplot(data = aa, aes(x=mod)) +
  geom_rect(aes(xmin=bound_a, xmax=bound_b, ymin=-Inf, ymax=Inf), alpha=0.05, fill='grey') +
  # geom_rect(aes(xmin=bound_a, xmax=1440, ymin=-Inf, ymax=Inf), alpha=0.05, fill='grey') +
  # geom_rect(aes(xmin=0, xmax=bound_b, ymin=-Inf, ymax=Inf), alpha=0.05, fill='grey') +
  geom_line(aes(y=smooth), size=1, col="steelblue") +
  geom_line(aes(y=max(smooth)*0.075),linetype="dotdash", color="red", size=1) +
  ylab("Total Arrivals") +
  # xlab("Time [minute]") +
  scale_x_continuous(name="Time [minute]", breaks=seq(0, 1440,250)) +
  theme_Publication(base_size=20)

dev.off()


# bound_a = max(thresholds[thresholds$mod<500,]$mod)
# bound_b = min(thresholds[thresholds$mod>500,]$mod)
# 
# write_csv(tibble(bound_inf = bound_a,
#                  bound_sup = bound_b),
#           paste("2. R/1. Outputs/",dataset,"/boundaries.csv",sep=""))

#### we select times only BEWTWEEN bound_inf and bound_sup

#### Check formulas
# readRDS(paste0("2. R/1. Outputs/",dataset,"/best_config_nhpp_gam.rds"))[['fmla']][[1]]


