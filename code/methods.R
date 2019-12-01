
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
  outputpath <- "/Users/paula/stats205_project/output/plots"
} else {
  setwd("ENTER YOUR PATH HERE")
}

# load, clean, run descripties 
source("code/load_clean.R")
source("code/descriptives.R")

# fix colors 
cols <- c("steelblue4", "goldenrod2", "grey4")

######## TEST & TRAIN DATA ###########
set.seed(1)

train_fraction <- 0.8

# Day; test = last month (make sure it is ordered)
day_train_forecast <- day[dteday <= "2012-11-30", ]
day_test_forecast <- day[dteday > "2012-11-30", ]

# day_train_forecast <- day[1:round(nrow(day)*train_fraction), ]
# day_test_forecast <- day[(round(nrow(day)*train_fraction) + 1):nrow(day), ]

######### SIMPLE MOVING AVERAGE ########### 

# 15 fold CV is aproximately equal to a month in each bin

numbins <- 15
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
    mod <- sma(train$cnt, order = o, h = nrow(test), interval = "nonparametric")
    
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
                h = nrow(day_test_forecast), interval = "nonparametric")
fcast_sma <- forecast(best_sma, h = nrow(day_test_forecast), interval = "nonparametric")

# plot results 
results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 0, fitted := best_sma$fitted]
results[test == 1, forecast := fcast_sma$mean[1:nrow(day_test_forecast)]]
results[test == 1, lower_ci := fcast_sma$lower[1:nrow(day_test_forecast)]]
results[test == 1, upper_ci := fcast_sma$upper[1:nrow(day_test_forecast)]]

mse_sma_test <- mean((day_test_forecast$cnt - fcast_sma$mean[1:nrow(day_test_forecast)])^2)
mse_sma_train <- mean((day_train_forecast$cnt - best_sma$fitted)^2)


results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("Moving Average of Order ", best_cv_order),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_sma_test)), "; Training RMSE: ", round(sqrt(mse_sma_train))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[1]],cols[[2]]),
                    labels=c("Training","Forecast")) + 
  scale_fill_manual("Non-parametric 95% PI", values = "grey12")
ggsave(paste0(outputpath, "/sma_forecast.png"), width = 8, height = 4)

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
results[test == 0, fitted := cubic_smoothing_spline$fitted]
results[test == 1, forecast := fcast_cubic_spline$mean[1:nrow(day_test_forecast)]]
results[test == 1, upper_ci := fcast_cubic_spline$upper[1:nrow(day_test_forecast), 2]]
results[test == 1, lower_ci := fcast_cubic_spline$lower[1:nrow(day_test_forecast), 2]]

mse_cspline_test <- mean((day_test_forecast$cnt - fcast_cubic_spline$mean[1:nrow(day_test_forecast)])^2)
mse_cspline_train <- mean((day_train_forecast$cnt - cubic_smoothing_spline$fitted)^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("Cubic Smoothing Spline "),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_cspline_test)), "; Training RMSE: ", round(sqrt(mse_cspline_train))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[1]],cols[[2]]),
                     labels=c("Training","Forecast")) + 
  scale_fill_manual("95% PI", values = "grey12")
ggsave(paste0(outputpath, "/cspline_forecast.png"), width = 8, height = 4)

######## EXPONENTIAL SMOOTHING #############

# 1. this is not working without arma errors!!! chosen by AIC
day_train_forecast$bins_ets <- as.numeric(cut(day_train_forecast$dteday, numbins + 1))

# some combinations will be invalid: skip
cross_validate_tbats <- function(b, tr, dtr, ar, nbin = numbins) {

  mse_cv <- c()
  
  for(i in 1:nbin) {
    
    
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

# refit best model on entire training set and forecast on test set
ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5, 365.25))
best_tbats <- tbats(ts_cnt, use.trend = FALSE, use.damped.trend = TRUE,
             use.arma.errors = FALSE)
forecast_mod_tbats <- forecast(best_tbats, h = nrow(day_test_forecast))

results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 0, fitted := best_tbats$fitted.values]
results[test == 1, forecast := forecast_mod_tbats$mean[1:nrow(day_test_forecast)]]
results[test == 1, upper_ci := forecast_mod_tbats$upper[1:nrow(day_test_forecast)]]
results[test == 1, lower_ci := forecast_mod_tbats$lower[1:nrow(day_test_forecast)]]

mse_tbats_test <- mean((day_test_forecast$cnt - forecast_mod_tbats$mean[1:nrow(day_test_forecast)])^2)
mse_tbats_train <- mean((day_train_forecast$cnt - best_tbats$fitted)^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("TBATS with Box-Cox, Damped Trend and Weekly, Monthly and Yearly Seasonality"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_tbats_test)), "; Training RMSE: ", round(sqrt(mse_tbats_train))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[1]],cols[[2]]),
                     labels=c("Training","Forecast")) + 
  scale_fill_manual("95% PI", values = "grey12")
ggsave(paste0(outputpath, "/tbats_forecast.png"), width = 8, height = 4)


################## MLP ########################

# Fit MLP
ts_cnt <- ts(day_train_forecast$cnt)
mlp_fit <- mlp(ts_cnt)
mlp_forecast <- forecast(mlp_fit,h=nrow(day_test_forecast))
plot(mlp_forecast)

# currenlty only plotting predictions!!!

results <- data.table(x = day_test_forecast$dteday)
results[, y := day_test_forecast$cnt]
results[, forecast :=  mlp_forecast$mean[1:nrow(day_test_forecast)]]
#results[, test := ifelse(x <= "2012-11-30", 0, 1)]
#results[test == 0, fitted := c(rep(NA, 4), mlp_fit$fitted)]
#results[test == 1, forecast := mlp_forecast$mean[1:nrow(day_test_forecast)]]
#results[test == 1, upper_ci := forecast_mod_tbats$upper[1:nrow(day_test_forecast)]]
#results[test == 1, lower_ci := forecast_mod_tbats$lower[1:nrow(day_test_forecast)]]
#mse_mlp_train <- mean((day_train_forecast[5:nrow(day_train_forecast)]$cnt - mlp_fit$fitted)^2)

mse_mlp_test <- mean((day_test_forecast$cnt - mlp_forecast$mean[1:nrow(day_test_forecast)])^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_line(size = 0.2) +
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  labs(title = paste0("MLP with 5 hidden nodes and 20 repetitions"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_mlp_test))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[2]]),
                     labels=c("Forecast"))  
ggsave(paste0(outputpath, "/mlp_forecast.png"), width = 8, height = 4)


# Fit ELM
ts_cnt <- ts(day_train_forecast$cnt)
elm_fit <- elm(ts_cnt)
elm_forecast <- forecast(elm_fit,h=nrow(day_test_forecast))
plot(elm_forecast)


results <- data.table(x = day_test_forecast$dteday)
results[, y := day_test_forecast$cnt]
results[, forecast :=  elm_forecast$mean[1:nrow(day_test_forecast)]]
#results[, test := ifelse(x <= "2012-11-30", 0, 1)]
#results[test == 0, fitted := c(rep(NA, 4), mlp_fit$fitted)]
#results[test == 1, forecast := mlp_forecast$mean[1:nrow(day_test_forecast)]]
#results[test == 1, upper_ci := forecast_mod_tbats$upper[1:nrow(day_test_forecast)]]
#results[test == 1, lower_ci := forecast_mod_tbats$lower[1:nrow(day_test_forecast)]]
#mse_mlp_train <- mean((day_train_forecast[5:nrow(day_train_forecast)]$cnt - mlp_fit$fitted)^2)

mse_elm_test <- mean((day_test_forecast$cnt - elm_forecast$mean[1:nrow(day_test_forecast)])^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_line(size = 0.2) +
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  labs(title = paste0("ELM with 100 hidden nodes and 20 repetitions"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_elm_test))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[2]]),
                     labels=c("Forecast"))  
ggsave(paste0(outputpath, "/elm_forecast.png"), width = 8, height = 4)

############### NNETAR ###############

ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5, 365.25))
nn <- nnetar(ts_cnt)
nn_forecast <- forecast(nn,h=nrow(day_test_forecast))
plot(nn_forecast)

results <- data.table(x = day_test_forecast$dteday)
results[, y := day_test_forecast$cnt]
results[, forecast :=  nn_forecast$mean[1:nrow(day_test_forecast)]]

mse_elm_test <- mean((day_test_forecast$cnt - nn_forecast$mean[1:nrow(day_test_forecast)])^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_line(size = 0.2) +
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  labs(title = paste0("Feed-Forward NN; Single Hidden Layer"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_elm_test))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[2]]),
                     labels=c("Forecast"))  
ggsave(paste0(outputpath, "/nn_forecast.png"), width = 8, height = 4)
