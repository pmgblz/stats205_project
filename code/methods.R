
# Packages 
library(dplyr)       # Data manipulation (0.8.0.1)
library(fBasics)     # Summary statistics (3042.89)
library(corrplot)    # Correlations (0.84)
library(psych)       # Correlation p-values (1.8.12)
library(grf)         # Generalized random forests (0.10.2)
library(rpart)       # Classification and regression trees, or CART (4.1-13)
library(rpart.plot)  # Plotting trees (3.0.6)
library(treeClust)   # Predicting leaf position for causal trees (1.1-7)
library(car)         # linear hypothesis testing for causal tree (3.0-2)
library(devtools)    # Install packages from github (2.0.1)
library(readr)       # Reading csv files (1.3.1)
library(tidyr)       # Database operations (0.8.3)
library(tibble)      # Modern alternative to data frames (2.1.1)
library(knitr)       # RMarkdown (1.21)
library(kableExtra)  # Prettier RMarkdown (1.0.1)
library(ggplot2)     # general plotting tool (3.1.0)
library(haven)       # read stata files (2.0.0)
library(aod)         # hypothesis testing (1.3.1)
library(evtree)      # evolutionary learning of globally optimal trees (1.0-7)
library(haven)
library(data.table)
library(caret)
library(magick)
library(xtable)
library(stargazer)
#library(HoRM)
#library(KernSmooth)
library(forecast)
#library(Kendall)
#library(trend)
#library(aTSA)
library(lmtest)
library(nnfor)
library(tsutils)

library(smooth)
library(Mcomp)


# Run path
if (dir.exists("/Users/paula/stats205_project/")) {
  setwd("/Users/paula/stats205_project/")
} else {
  setwd("ENTER YOUR PATH HERE")
}


######## LOAD & CLEAN DATA ###########

# Load data 
day <- read_csv("data/day.csv")
hour <- read_csv("data/hour.csv")

# make sure it is ordered correctly 
hour <- hour[order(hour$dteday, hour$hr),]
day <- day[order(day$dteday),]


# Load functions 
source("code/kernel_functions.R")

# Clean data 
setDT(hour)
hour[, season := as.factor(ifelse(season == 1, "Spring", 
                                  ifelse(season == 2, "Summer", 
                                         ifelse(season == 3, "Fall", 
                                                ifelse(season == 4, "Winter", NA)))))]

hour[, weathersit := as.factor(ifelse(weathersit == 1, "Clear", 
                                ifelse(weathersit == 2, "Misty", 
                                       ifelse(weathersit == 3, "Rain", 
                                              ifelse(weathersit == 4, "Thunderstorm", NA)))))]

hour <- hour[, -c("instant")]


# dummify the data
dmy <- dummyVars(" ~ .", data = hour)
hour <- data.frame(predict(dmy, newdata = hour))


# Drop rows containing missing values (doesn't actually do anything)
hour <- na.omit(hour)

# get total counts
setDT(hour)
setDT(day)

# further cleaning 
setDT(hour)
hour[, yr := ifelse(hour$yr == 0, 2011, 2012)]

hour_temp <- hour[, .(mean_count = mean(cnt)), by = c("temp")]
hour_temp <- hour[, lapply(.SD, mean), by=temp]

day[, month := mnth + yr*12]

######## TEST & TRAIN DATA ###########
set.seed(1)

train_fraction <- 0.8

# Day; test = last month (make sure it is ordered)
day_train_forecast <- day[dteday <= "2012-11-30", ]
day_test_forecast <- day[dteday > "2012-11-30", ]

# day_train_forecast <- day[1:round(nrow(day)*train_fraction), ]
# day_test_forecast <- day[(round(nrow(day)*train_fraction) + 1):nrow(day), ]

######## DESCRIPTIVES ###########

# Make a data.frame containing summary statistics of interest
summ_stats <- fBasics::basicStats(hour[, -c("dteday")])
summ_stats <- as.data.frame(t(summ_stats))

# Rename some of the columns for convenience
summ_stats <- summ_stats[c("Mean", "Stdev", "Minimum", "1. Quartile", "Median",  "3. Quartile", "Maximum")]
colnames(summ_stats)[colnames(summ_stats) %in% c('1. Quartile', '3. Quartile')] <- c('Lower quartile', 'Upper quartile')

row.names(summ_stats) <- c("Fall", "Spring", "Summer", "Winter", 
                           "Year", "Month", "Hour",
                           "Holiday", "Day of Week", "Workday", "Clear Weather", "Misty Weather", 
                           "Rainy Weather", "Thunderstorm", "Temperature",
                           "Felt Temperature", "Humidity", "Windspeed", "Casual Users", 
                           "Registered Users", "All Users")



# save
stargazer(summ_stats,
          type = "latex", 
          summary=FALSE, rownames=TRUE,
          digits = 2) -> sumstats 

tabular_positions <- grep("tabular", sumstats)
sumstats <- sumstats[tabular_positions[1]:tabular_positions[2]]
write(sumstats,  file="output/tables/summary_stats.tex")

hour_plot <- hour
colnames(hour_plot) <- c("Date", "Fall", "Spring", "Summer", "Winter", 
                  "Year", "Month", "Hour",
                  "Holiday", "Day of Week", "Workday", "Clear Weather", "Misty Weather", 
                  "Rainy Weather", "Thunderstorm", "Temperature",
                  "Felt Temperature", "Humidity", "Windspeed", "Casual Users", 
                  "Registered Users", "All Users")

# correlation plot 
pairwise_pvalues <- psych::corr.test(hour_plot, hour_plot)$p
png(filename="output/plots/corrplot.png")
pairwise_pvalues <- psych::corr.test(hour_plot, hour_plot)$p
corrplot(cor(hour_plot),
         type="upper",
         tl.col="black",
         order="hclust",
         tl.cex=0.6,
         addgrid.col = "black",
         p.mat=pairwise_pvalues,
         sig.level=0.05,
         number.font=10,
         insig="blank") 
dev.off()

# plot daily trends
day %>% 
  ggplot(aes(dteday, cnt)) + geom_point(size = 0.5) + 
  xlab("Day") + ylab("Total Number of Users")

ggsave("output/plots/users_by_day.png")

# average number of users by hour 
users_by_hour <- hour[, .(avg_users_by_hours = mean(cnt)), by = c("hr")]

users_by_hour %>% 
  ggplot(aes(hr, avg_users_by_hours)) + geom_line(size = 0.5) + 
  xlab("Hour") + ylab("Average Number of Users")
ggsave("output/plots/average_number_users_by_hour.png")



######### SIMPLE MOVING AVERAGE ########### 

numbins <- 5
day_train_forecast$bins <- as.numeric(cut(day_train_forecast$dteday, numbins + 1))
order_max <- 20
mse_order <- matrix(nrow = order_max, ncol = 2)

for(o in 1:order_max) {
  
  mse_cv <- c()
  
  for(i in 1:numbins) {
    
    train <- day_train_forecast[bins <= i, ]
    test <- day_train_forecast[bins == i + 1, ]
    
    # run simple moving average on test set 
    # hold out set is length of training set
    mod <- sma(train$cnt, order = o, h = nrow(test))
    
    # calcualte mse 
    mse_cv[i] <- mean((test$cnt -  forecast(mod, h = nrow(test))$mean[1:nrow(test)])^2)
    
    
  }
  
  mse_order[o, 1] <- o
  mse_order[o, 2] <- mean(mse_cv)
}

# which is the best simple moving average? 
best_cv_order <- which.min(mse_order[, 2])

# refit with best cv order (and predict on test set)
best_sma <- sma(day_train_forecast$cnt, o = best_cv_order,
                h = nrow(day_test_forecast), interval = "np")
plot(forecast(best_sma, h = nrow(day_test_forecast) ))

# plot results 
results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[, fitted := ifelse(test == 0, mod$fitted.values, NA)]
results[, forecast := ifelse(test == 1, 
                             forecast(mod, h = nrow(day_test_forecast))$mean[1:nrow(day_test_forecast)],
                             NA)]

mse_tbats_test <- mean((day_test_forecast$cnt - forecast(mod, h = nrow(day_test_forecast))$mean[1:nrow(day_test_forecast)])^2)
mse_tbats_train <- mean((day_train_forecast$cnt - mod$fitted)^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point() +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast"))
ggsave()


########## CUBIC SPLINES ################# 

# this does cross validation: https://www.rdocumentation.org/packages/forecast/versions/8.9/topics/splinef
# same as arima (0, 2, 2)
# shown how cubic smoothing splines can be used to obtain local linear forecasts for a univariate time series
ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5, 365.25))
cubic_smoothing_spline <- splinef(ts_cnt,  h = nrow(day_test_forecast))
fcast_cubic_spline <- forecast(cubic_smoothing_spline,  h = nrow(day_test_forecast))
plot(fcast_cubic_spline)

results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[, fitted := ifelse(test == 0, cubic_smoothing_spline$fitted, NA)]
results[, forecast := ifelse(test == 1, 
                             forecast(cubic_smoothing_spline, h = nrow(day_test_forecast))$mean[1:nrow(day_test_forecast)],
                             NA)]

mse_tbats_test <- mean((day_test_forecast$cnt - forecast(cubic_smoothing_spline, h = nrow(day_test_forecast))$mean[1:nrow(day_test_forecast)])^2)
mse_tbats_train <- mean((day_train_forecast$cnt - cubic_smoothing_spline$fitted)^2)

results %>%
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast"))

######## EXPONENTIAL SMOOTHING #############

# 1. this is not working without arma errors!!! chosen by AIC
numbins <- 5
day_train_forecast$bins_ets <- as.numeric(cut(day_train_forecast$dteday, numbins + 1))

# some combinations will be invalid: skip
cross_validate_tbats <- function(b, tr, dtr, ar, numbins = 5) {

  mse_cv <- c()
  
  for(i in 1:numbins) {
    
    
    train <- day_train_forecast[bins <= i, ]
    test <- day_train_forecast[bins == i + 1, ]
    
    # run simple moving average on test set 
    # hold out set is length of training set
    
    #mod <- ets(ts_cnt, model = paste0(er, tr, sea))
    ts_cnt <- msts(train$cnt, seasonal.periods=c(7, 30.5, 365.25))
    mod <- tbats(ts_cnt, use.box.cox = b, use.trend = tr, 
                 use.damped.trend = dtr, use.arma.errors = ar)
    
    mse_cv[i] <- mean((test$cnt -  forecast(mod, h = nrow(test))$mean[1:nrow(test)])^2)
    
    
  }
  print(mean(mse_cv))
  return(mean(mse_cv))

  
}

eval_true_matrix_input <- function(input) {
  if(input == TRUE) {
    return(1)
  } else {
    return(0)
  }
}

  
use_boxcox <- c(TRUE, FALSE)
use_trend <- c(TRUE, FALSE)
use_damped_trend <- c(TRUE, FALSE)
use_arma_errors <- c(TRUE, FALSE) # onlye working with TRUE!
  
mse_tbats <- matrix(nrow = 16, ncol = 5)
c <- 0
for(b in use_boxcox) {
  for(tr in use_trend) {
    for(dtr in use_damped_trend) {
      for(ar in use_arma_errors) {
        
          c <- c + 1
        
          mse_tbats[c, 1] <- eval_true_matrix_input(b)
          mse_tbats[c, 2] <- eval_true_matrix_input(tr)
          mse_tbats[c, 3] <- eval_true_matrix_input(dtr)
          mse_tbats[c, 4] <- eval_true_matrix_input(ar)

        
          mse_tbats[c, 5] <- cross_validate_tbats(b = b, tr = tr, dtr = dtr, ar = ar)
        
      }
    }
    
  }
  
}

# what's the best model? 
mse_tbats[which.min(mse_tbats[, 5]), ]


# far2 <- function(x, h){forecast(tbats(x), h=h)}
# e <- tsCV(ts_cnt, far2, h=nrow(day_test_forecast), window = 30)

# refit best model on entire training set and forecast on test set
ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5, 365.25))
mod <- tbats(ts_cnt, use.trend = FALSE, use.damped.trend = TRUE,
             use.arma.errors = FALSE)
forecast_mod_tbats <- forecast(mod, h = nrow(day_test_forecast))

results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[, fitted := ifelse(test == 0, mod$fitted.values, NA)]
results[, forecast := ifelse(test == 1, 
                             forecast(mod, h = nrow(day_test_forecast))$mean[1:nrow(day_test_forecast)],
                             NA)]

mse_tbats_test <- mean((day_test_forecast$cnt - forecast(mod, h = nrow(day_test_forecast))$mean[1:nrow(day_test_forecast)])^2)
mse_tbats_train <- mean((day_train_forecast$cnt - mod$fitted)^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point() +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast"))
ggsave()


plot(forecast_mod_tbats, main = "Best TBATS CV",sub = paste0("Training MSE: ", mse_tbats_train, 
                                      "; Test MSE: ", mse_tbats_test))
lines(mod$fitted.values, col = 4)

# Fit MLP
mlp.fit <- mlp(y)
frc <- forecast(mlp.fit,h=nrow(day_test_forecast))
plot(frc)

fit6 <- elm(y)
frc <- forecast(fit6,h=nrow(day_test_forecast))
plot(frc)



