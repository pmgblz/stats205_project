
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
library(fANCOVA)
library(kedd)
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

covs <- c("season", "yr", "mnth", "holiday", "weekday",
          "workingday", "weathersit", "temp", "atemp", "hum", "windspeed")

############# AUTOCORRELATION OF RESIDUALS ########



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


################ LOESS ##################


################ ... stl forecast #############
ts_cnt <- msts(day$cnt, seasonal.periods=c(7, 30.5, 365.25))
loes_decomp <- mstl(ts_cnt)
png(filename="output/plots/mstl_whole_data.png")
plot(loes_decomp, main = "Seasonal (Weekly & Monthly) Decomposition using LOESS on Entire Dataset")
dev.off()

# seasonal decomposition using loess
# takes seasonality into account
# no yearly seasonality since less than 2 years of data
ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5))
loes_decomp <- mstl(ts_cnt)

png(filename="output/plots/mstl_training_data.png")
plot(loess_decomp, main = "Seasonal (Weekly & Monthly) Decomposition using LOESS on Training Data")
dev.off()

# forecast using random walk assumption
loess_forecast <- forecast(loess_decomp, h = nrow(day_test_forecast), method = "naive")

results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 1, forecast := loess_forecast$mean[1:nrow(day_test_forecast)]]
results[test == 1, upper_ci := loess_forecast$upper[1:nrow(day_test_forecast)]]
results[test == 1, lower_ci := loess_forecast$lower[1:nrow(day_test_forecast)]]

mse_stl_test <- mean((day_test_forecast$cnt - loess_forecast$mean[1:nrow(day_test_forecast)])^2, na.rm = TRUE)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("Seasonal (Weekly & Monthly) Decomposition using LOESS: Forecast using Naive Random Walk"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_stl_test))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[2]]),
                     labels=c("Forecast")) + 
  scale_fill_manual("95% PI", values = "grey12")
ggsave(paste0(outputpath, "/stl_loess_forecast_test.png"), width = 8, height = 4)

################ ... loess smoothing (no forecast) #############

#degree of local polynomials: 0, 1, 2: cross validate
# in training data: cross validate span
mse_order <- matrix(nrow = 3, ncol = 3)

for(d in 0:2) {
  
  
    loess_cv <- loess.as(day_train_forecast$index,
                         day_train_forecast$cnt,
                         criterion = "gcv", 
                         degree = d)
    loess_cv_span <- loess_cv$pars$span
    
    # apply to test data 
    loess_test <- loess.as(day_test_forecast$index, 
                           day_test_forecast$cnt, 
                           user.span = loess_cv_span, 
                           degree = d)

    # calcualte mse 
    mse_cv <- mean((day_test_forecast$cnt -  loess_test$fitted)^2)
    
  
    mse_order[d + 1, 1] <- d
    mse_order[d + 1, 2] <- mse_cv
    mse_order[d + 1, 3] <- loess_cv_span
}

# which is the degree
best_degree <- which.min(mse_order[, 2]) - 1
best_span <- mse_order[best_degree + 1, 3]

# on training data 
day[, index := .I]
m <- loess.as(day$index, day$cnt)

loess_train <- loess.as(day_train_forecast$index, 
                        day_train_forecast$cnt, 
                       user.span = best_span, 
                       degree = best_degree)

# apply to test data 
loess_entire <- loess.as(day$index, 
                         day$cnt, 
                       user.span = best_span, 
                       degree = best_degree)

results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[, fitted := loess_entire$fitted]
results[test == 1, fitted_test := fitted]
results[test == 0, fitted_train := fitted]

mse_loess_test <- mean((results[test == 1, ]$y - results[test == 1, ]$fitted)^2)
mse_loess_train <- mean((results[test == 0, ]$y - results[test == 0, ]$fitted)^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted_test, col = "Fitted Test")) + 
  geom_line(aes(x = x, y = fitted_train, col = "Fitted Train")) + 
  labs(title = paste0("Loess Smoothing (Cross-validated on Training Set)"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_loess_test)),
                         "; Training RMSE: ", round(sqrt(mse_loess_train)), 
                         " (Smoothing Span: ", round(best_span, 2), "; Order: ", best_cv_order, ")"), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[2]], cols[[1]]),
                     labels=c("Test", "Training"))
ggsave(paste0(outputpath, "/loess_smoothing.png"), width = 8, height = 4)


########## PCA ################# 

pca_covariates <- prcomp(day_train_forecast[, ..covs], scale = TRUE, center = TRUE)
summary(pca_covariates)

# get principal component score vectors 
pc_score_vectors <- as.data.table(pca_covariates$x)
pca1_covs <- pc_score_vectors$PC1

########## GAUSSIAN KERNEL ################# 


########## ... based on index ############

day_train_forecast[, index := .I]

numbins <- 15
day_train_forecast$bins <- as.numeric(cut(day_train_forecast$dteday, numbins + 1))

# I ran it until 200, best was 30
# only 50 for speed now
order_max <- 50

mse_order <- matrix(nrow = order_max, ncol = 2)

for(o in 1:order_max) {
  
  mse_cv <- c()
  
  for(i in 1:numbins) {
    
    train <- day_train_forecast[bins <= i, ]
    test <- day_train_forecast[bins == i + 1, ]
    
    # run simple moving average on test set 
    # hold out set is length of training set
    mod <- ksmooth(train$index, train$cnt, "normal", bandwidth = o)
    pred_test <- predict(mod, h = nrow(test))

    # calcualte mse 
    mse_cv[i] <- mean((test$cnt -  pred_test$y$mean[1:nrow(test)])^2)
    
    
  }
  
  mse_order[o, 1] <- o
  mse_order[o, 2] <- mean(mse_cv)
}

# which is the best simple moving average? 
best_cv_order <- which.min(mse_order[, 2])

# refit with best cv order (and predict on test set)
best_gaussian <- ksmooth(day_train_forecast$index,
                    day_train_forecast$cnt,
                    "normal", 
                    bandwidth = best_cv_order)

fcast_gaussian <- predict(best_gaussian, h = nrow(day_test_forecast))

# plot results 
results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 0, fitted := best_gaussian$y]
results[test == 1, forecast := fcast_gaussian$y$mean[1:nrow(day_test_forecast)]]
results[test == 1, lower_ci := fcast_gaussian$y$lower[1:nrow(day_test_forecast), 2]]
results[test == 1, upper_ci := fcast_gaussian$y$upper[1:nrow(day_test_forecast), 2]]

mse_sma_test <- mean((day_test_forecast$cnt - fcast_gaussian$y$mean[1:nrow(day_test_forecast)])^2)
mse_sma_train <- mean((day_train_forecast$cnt - best_gaussian$y)^2)


results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("Gausssian Kernel (CV) with Bandwidth ", best_cv_order),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_sma_test)), "; Training RMSE: ", round(sqrt(mse_sma_train))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[1]],cols[[2]]),
                     labels=c("Training","Forecast")) + 
  scale_fill_manual("95% PI", values = "grey12")
ggsave(paste0(outputpath, "/gaussian_kernel_predict.png"), width = 8, height = 4)


########## BOXCAR KERNEL ################# 

########## ... based on index ############

numbins <- 15
day_train_forecast$bins <- as.numeric(cut(day_train_forecast$dteday, numbins + 1))

# I ran it until 200, best was 23
# only 30 for speed now
order_max <- 30

mse_order <- matrix(nrow = order_max, ncol = 2)

for(o in 1:order_max) {
  
  mse_cv <- c()
  
  for(i in 1:numbins) {
    
    train <- day_train_forecast[bins <= i, ]
    test <- day_train_forecast[bins == i + 1, ]
    
    # run simple moving average on test set 
    # hold out set is length of training set
    mod <- ksmooth(train$index, train$cnt, "box", bandwidth = o)
    pred_test <- predict(mod, h = nrow(test))
    
    # calcualte mse 
    mse_cv[i] <- mean((test$cnt -  pred_test$y$mean[1:nrow(test)])^2)
    
    
  }
  
  mse_order[o, 1] <- o
  mse_order[o, 2] <- mean(mse_cv)
}

# which is the best simple moving average? 
best_cv_order <- which.min(mse_order[, 2])

# refit with best cv order (and predict on test set)
best_boxcar <- ksmooth(day_train_forecast$index,
                         day_train_forecast$cnt,
                         "box", 
                         bandwidth = best_cv_order)

fcast_boxcar <- predict(best_boxcar, h = nrow(day_test_forecast))

# plot results 
results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 0, fitted := best_boxcar$y]
results[test == 1, forecast := fcast_boxcar$y$mean[1:nrow(day_test_forecast)]]
results[test == 1, lower_ci := fcast_boxcar$y$lower[1:nrow(day_test_forecast), 2]]
results[test == 1, upper_ci := fcast_boxcar$y$upper[1:nrow(day_test_forecast), 2]]

mse_sma_test <- mean((day_test_forecast$cnt - fcast_boxcar$y$mean[1:nrow(day_test_forecast)])^2)
mse_sma_train <- mean((day_train_forecast$cnt - best_boxcar$y)^2)


results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("Boxcar Kernel (CV) with Bandwidth ", best_cv_order),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_sma_test)), "; Training RMSE: ", round(sqrt(mse_sma_train))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[1]],cols[[2]]),
                     labels=c("Training","Forecast")) + 
  scale_fill_manual("95% PI", values = "grey12")
ggsave(paste0(outputpath, "/boxcar_kernel_predict.png"), width = 8, height = 4)


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


####### ... univariate ##############

# 1. this is not working without arma errors!!! chosen by AIC
#day_train_forecast$bins_ets <- as.numeric(cut(day_train_forecast$dteday, numbins + 1))

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

############### ARIMA ###############


######### ... univariate ############

ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5))

p_order <- seq(0, 1)
d_oder <- seq(0, 1)
q_order <- seq(0, 1)

P_season <- seq(0, 1)
D_season <- seq(0, 1)
Q_season <- seq(0, 1)

mse_cv <- matrix(NA, nrow = length(p_order)^6, ncol = 7)

count <- 0
for(po in p_order) {
  for(do in d_oder) {
    for(qo in q_order) {
      for(PS in P_season) {
        for(DS in D_season) {
          for(QS in Q_season) {
              count <- count + 1
              far2 <- function(x, h){forecast(Arima(x,
                                                    order=c(po,do,qo),
                                                    seasonal = c(PS, DS, QS)), h=h)}
              mse_cv[count, 1] <- mean(tsCV(ts_cnt, far2, h=1)^2, na.rm = TRUE)
              mse_cv[count, 2] <- po 
              mse_cv[count, 3] <- do 
              mse_cv[count, 4] <- qo 
              mse_cv[count, 5] <- PS 
              mse_cv[count, 6] <- DS 
              mse_cv[count, 7] <- QS 
              
          }
        }
      }
    }
  }
}

# which is the best simple moving average? 
best_cv_order <- which.min(mse_cv[, 1])

best_p <- mse_cv[best_cv_order, 2]
best_d <- mse_cv[best_cv_order, 3]
best_q <- mse_cv[best_cv_order, 4]

best_PS <- mse_cv[best_cv_order, 5]
best_DS <- mse_cv[best_cv_order, 6]
best_QS <- mse_cv[best_cv_order, 7]


# refit with best cv order (and predict on test set)
best_sarima <- Arima(ts_cnt,
                     order = c(best_p, best_d, best_q), 
                     seasonal =   c(best_PS, best_DS, best_QS))


fcast_sarima <- forecast(best_sarima, h = nrow(day_test_forecast))

results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 0, fitted := best_sarima$fitted]
results[test == 1, forecast := fcast_sarima$mean[1:nrow(day_test_forecast)]]
results[test == 1, upper_ci := fcast_sarima$upper[1:nrow(day_test_forecast), 2]]
results[test == 1, lower_ci := fcast_sarima$lower[1:nrow(day_test_forecast), 2]]

mse_sarima_test <- mean((day_test_forecast$cnt - fcast_sarima$mean[1:nrow(day_test_forecast)])^2)
mse_sarima_train <- mean((day_train_forecast$cnt - best_sarima$fitted)^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("Best (Seasonal) ARIMA: (", best_p, ",", best_d, "," , best_q, ") ", 
                      "(", best_PS, ", ", best_DS, ", ", best_QS, ")"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_tbats_test)), "; Training RMSE: ", round(sqrt(mse_tbats_train))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[1]],cols[[2]]),
                     labels=c("Training","Forecast")) + 
  scale_fill_manual("95% PI", values = "grey12")
ggsave(paste0(outputpath, "/sarima_forecast.png"), width = 8, height = 4)


######### ... multivariate ############

covariates <- as.matrix(day_train_forecast[, ..covs])

# less than a year of data
ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5))

p_order <- seq(0, 1)
d_oder <- seq(0, 1)
q_order <- seq(0, 1)

P_season <- seq(0, 1)
D_season <- seq(0, 1)
Q_season <- seq(0, 1)

mse_cv <- matrix(NA, nrow = length(p_order)^6, ncol = 7)

count <- 0
for(po in p_order) {
  for(do in d_oder) {
    for(qo in q_order) {
      for(PS in P_season) {
        for(DS in D_season) {
          for(QS in Q_season) {
            count <- count + 1
            far2 <- function(x, h){forecast(Arima(x, order=c(po,do,qo), 
                                            seasonal = c(PS, DS, QS), 
                                            xreg = covariates),
                                            h=h)}
            mse_cv[count, 1] <- mean(tsCV(ts_cnt, far2, h=1)^2, na.rm = TRUE)
            mse_cv[count, 2] <- po 
            mse_cv[count, 3] <- do 
            mse_cv[count, 4] <- qo 
            mse_cv[count, 5] <- PS 
            mse_cv[count, 6] <- DS 
            mse_cv[count, 7] <- QS 
            
          }
        }
      }
    }
  }
}

# which is the best simple moving average? 
best_cv_order <- which.min(mse_cv[, 1])

best_p <- mse_cv[best_cv_order, 2]
best_d <- mse_cv[best_cv_order, 3]
best_q <- mse_cv[best_cv_order, 4]

best_PS <- mse_cv[best_cv_order, 5]
best_DS <- mse_cv[best_cv_order, 6]
best_QS <- mse_cv[best_cv_order, 7]


# refit with best cv order (and predict on test set)
best_sarima_covs <- Arima(ts_cnt,
                     order = c(best_p, best_d, best_q), 
                     seasonal =   c(best_PS, best_DS, best_QS))


fcast_sarima_covs <- forecast(best_sarima, h = nrow(day_test_forecast))

results <- data.table(x = day$dteday)
results[, y := day$cnt]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 0, fitted := best_sarima_covs$fitted]
results[test == 1, forecast := fcast_sarima_covs$mean[1:nrow(day_test_forecast)]]
results[test == 1, upper_ci := fcast_sarima_covs$upper[1:nrow(day_test_forecast), 2]]
results[test == 1, lower_ci := fcast_sarima_covs$lower[1:nrow(day_test_forecast), 2]]

mse_sarima_test <- mean((day_test_forecast$cnt - fcast_sarima_covs$mean[1:nrow(day_test_forecast)])^2)
mse_sarima_train <- mean((day_train_forecast$cnt - best_sarima_covs$fitted)^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_point(size = 0.2) +
  geom_line(aes(x = x, y = fitted, col = "Fitted")) + 
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  geom_ribbon(aes(x = x, ymax = upper_ci, ymin = lower_ci, fill = ""), alpha = 0.3) + 
  labs(title = paste0("Best (Seasonal) ARIMA with Covariates: (", best_p, ",", best_d, "," , best_q, ") ", 
                      "(", best_PS, ", ", best_DS, ", ", best_QS, ")"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_tbats_test)), "; Training RMSE: ", round(sqrt(mse_tbats_train))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[1]],cols[[2]]),
                     labels=c("Training","Forecast")) + 
  scale_fill_manual("95% PI", values = "grey12")
ggsave(paste0(outputpath, "/sarima_forecast.png"), width = 8, height = 4)


covs_pred <- as.matrix(day_test_forecast[, c("temp", "hum", "windspeed", "holiday", 
                                             "workingday", "weathersit")])
fit <- auto.arima(ts_cnt, xreg = covs)
autoplot(fit)
fcast <- forecast(fit, xreg = covs_pred, h = nrow(day_test_forecast))
plot(fcast)
# calcualte mse
rmse <- sqrt(mean((day_test_forecast$cnt - fcast$mean)^2))
rmse
############### Neural Network Autoregression ###############

############## ... with covariates ##################

ts_cnt <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 30.5, 365.25))
nn <- nnetar(ts_cnt, xreg = as.matrix(day_train_forecast[, ..covs]))
nn_forecast <- forecast(nn,h=nrow(day_test_forecast), xreg = as.matrix(day_test_forecast[, ..covs]))
plot(nn_forecast)

results <- data.table(x = day_test_forecast$dteday)
results[, y := day_test_forecast$cnt]
results[, forecast :=  nn_forecast$mean[1:nrow(day_test_forecast)]]

mse_elm_test <- mean((day_test_forecast$cnt - nn_forecast$mean[1:nrow(day_test_forecast)])^2)

results %>% 
  ggplot(aes(x = x, y = y)) + geom_line(size = 0.2) +
  geom_line(aes(x = x, y = forecast, col = "Forecast")) + 
  labs(title = paste0("Neural Network Autoregression: Feed-Forward NN; Single Hidden Layer; With Covariates"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_elm_test))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[2]]),
                     labels=c("Forecast"))  
ggsave(paste0(outputpath, "/nn_forecast_covariates.png"), width = 8, height = 4)


############# ... without covariates #############


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
  labs(title = paste0("Neural Network Autoregression: Feed-Forward NN; Single Hidden Layer; Without Covariates"),
       subtitle = paste0("Test RMSE: ", round(sqrt(mse_elm_test))), 
       x = "Date", y = "Bike Count") +
  theme_linedraw() +
  theme(axis.ticks = element_blank()) +
  scale_color_manual(name="", values=c(cols[[2]]),
                     labels=c("Forecast"))  
ggsave(paste0(outputpath, "/nn_forecast_no_covariates.png"), width = 8, height = 4)
