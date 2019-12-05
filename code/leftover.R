for(input in inputs) {
      tryCatch(print(paste("log of", input, "=", log(input))),
               warning = function(w) {print(paste("negative argument", input)); 
                 log(-input)},
               error = function(e) {print(paste("non-numeric argument", input));
                 NaN})
}



fit <- ets(day_train_forecast$cnt)
plot(forecast(fit))

# 
# day_ts <- ts(day$cnt, start = c(2011, 1, 1), end = c(2012, 12, 31), frequency = 7)
# 
# # weekly, monthly and yearly seasonality
# y <- msts(day_train_forecast$cnt, seasonal.periods=c(1, 7,30.5, 365.25), start = c(2011,01) )



# Multi-step, re-estimation
#x <- msts(day$cnt, seasonal.periods=c(7, 365.25))
# SHIFT FOREWARD BY 7 OBSERVATIONS (ONE WEEK)
x <- ts(day$cnt, start = c(2011, 1), deltat = 1/(365.5))
k <- 7 # minimum data length for fitting a model (ONE WEEK)
n <- length(day$cnt)
f <- 365.5
mae1 <- mae2 <- mae3 <- matrix(NA,f,f)
st <- tsp(x)[1]+(k-1)/f
for(i in 1:shift)
{
  xshort <- window(x, end=st + (i-1))
  xnext <- window(a10, start=st + (i-1) + 1/12, end=st + i)
  fit1 <- tslm(xshort ~ trend + season, lambda=0)
  fcast1 <- forecast(fit1, h=12)
  fit2 <- Arima(xshort, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12),
                include.drift=TRUE, lambda=0, method="ML")
  fcast2 <- forecast(fit2, h=12)
  fit3 <- ets(xshort,model="MMM",damped=TRUE)
  fcast3 <- forecast(fit3, h=12)
  mae1[i,] <- abs(fcast1[['mean']]-xnext)
  mae2[i,] <- abs(fcast2[['mean']]-xnext)
  mae3[i,] <- abs(fcast3[['mean']]-xnext)
}
plot(1:12, colMeans(mae1), type="l", col=2, xlab="horizon", ylab="MAE",
     ylim=c(0.35,1.5))
lines(1:12, colMeans(mae2), type="l",col=3)
lines(1:12, colMeans(mae3), type="l",col=4)
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)


#### Starting with 1 month of training data
#### We think it has weekly frequency, forecast for a week
dt <- day
i <- 30  
forecast_length <- 48
pred_ets <- c()
#pred_arima <- c()
while(i <= nrow(dt)){
  
  ts <- ts(dt[1:i, "cnt"], frequency=7)
  
  #pred_ets <- rbind(pred_ets, data.frame(forecast(ets(ts), 7)$mean[1:3]))
  pred_arima <- rbind(pred_arima, data.frame(forecast(auto.arima(ts), forecast_length)$mean[1:forecast_length]))
  
  i = i + forecast_length
  
}
names(pred_arima) <- "arima"
names(pred_ets) <- "ets"

pred_ets <- ts(pred_ets$ets, start=c(2005, 01), frequency = 12)
pred_arima <- ts(pred_arima$arima, start=c(2005, 01), frequency =12)

accuracy(pred_ets, ts_dt)
accuracy(pred_arima, ts_dt)

######## UNIVARIATE CASE ###########




# simple moving average 
sma(day_train_forecast$cnt) -> test

#Fit an AR(2) model to each rolling origin subset
far2 <- function(x, h){sma(day_train_forecast$cnt, order = h)}
e <- tsCV(day_train_forecast$cnt, far2, h=13)

tsCV(day_train_forecast$cnt, forecastfunction = sma)

# set as time series object with 
# weekly, monthly and yearly seasonality
y <- msts(day_train_forecast$cnt, seasonal.periods=c(7,30.5, 365.25))


# CUBIC SMOOTHING SPLINES: https://www.rdocumentation.org/packages/forecast/versions/8.9/topics/splinef
# same as arima (0, 2, 2)
# shown how cubic smoothing splines can be used to obtain local linear forecasts for a univariate time series
cubic_smoothing_spline <- splinef(y)
fcast_cubic_spline <- forecast(cubic_smoothing_spline,  h = nrow(day_test_forecast))
plot(fcast_cubic_spline)

far2 <- function(x, h){forecast(splinef(y,  h = nrow(day_test_forecast)), h=h)}
e <- tsCV(y, far2, h=1)

# NEURAL NETWORK - DO IN PYTHON!!!
# fit <- nnetar(y)
# plot(forecast(fit,h=nrow(day_test_forecast)))
# lines(y)


# Fit MLP
mlp.fit <- mlp(y)
frc <- forecast(mlp.fit,h=nrow(day_test_forecast))
plot(frc)

fit6 <- elm(y)
frc <- forecast(fit6,h=nrow(day_test_forecast))
plot(frc)

# ARIMA MODELLING (WITHOUT COVARIATES)
#y <- msts(day_train_forecast$cnt, seasonal.periods=c(7, 365.25))
y <- ts(day_train_forecast$cnt, frequency= 7)

fit <- auto.arima(day_train_forecast$cnt)
autoplot(fit)

day_train_forecast$prewhite <- fit$residuals

fcast <- forecast(fit, h = nrow(day_test_forecast))
plot(fcast)

# LINEAR MODEL WITH TREND & SEASONAL DUMMIES


# EXPONENTIAL SMOOTHING


# KERNEL RIDGE REGRESSION


# set as time series  https://robjhyndman.com/hyndsight/dailydata/
y <- ts(day_train_forecast$cnt, frequency= 7)


day[, c("temp", "hum",
        "windspeed", "holiday", "workingday", "weathersit", "temp")] -> m
# set as time-series: 
#modelcv <- CVar(y, k=5, lambda=0.15)
x <- day_train_forecast$cnt
#y <- ts(day_train_forecast$cnt, frequency=c(7,365.25))
y <- msts(day_train_forecast$cnt, seasonal.periods=c(7,31, 365.25))
#y <- msts(x, seasonal.periods=c(7,365.25))
#y <- ts(x, frequency=7)
#fit <- tbats(y)
#fc <- forecast(fit)
#plot(fc)

# fit <- tbats(y)
# fc <- forecast(fit)
# plot(fc)

covs <- as.matrix(day_train_forecast[, c("temp", "hum", "windspeed", "holiday", 
                                         "workingday", "weathersit")])
covs_pred <- as.matrix(day_test_forecast[, c("temp", "hum", "windspeed", "holiday", 
                                             "workingday", "weathersit")])
fit <- auto.arima(y, xreg = covs)
autoplot(fit)
fcast <- forecast(fit, xreg = covs_pred, h = nrow(day_test_forecast))
plot(fcast)
# calcualte mse
mse <- mean((day_test_forecast$cnt - fcast$mean)^2)
mse
day_test_forecast$predicted <- fcast$mean

# neural network autoregression 
modelcv <- CVar(lynx, k=5, lambda=0.15)
print(modelcv)


TSA::prewhiten(x, y, x.model = fit)

day$combined <- NA 
day[1:length(day_train_forecast), ]$combined <- fit$fitted
rbind(fit$fitted,day_test_forecast$predicted )

day_test_forecast$predicted <- fcast$mean
plot(day_train_forecast$dteday, day_train_forecast$cnt, col = 3, type = "l")
lines(day_train_forecast$dteday, fit$fitted, col = 4)

plot(day_test_forecast$dteday, day_test_forecast$cnt, col = 3, type = "l")
lines(day_test_forecast$dteday, day_test_forecast$predicted, col = 4)

far2 <- function(x, h){forecast(Arima(y, order=c(1,1,1)), h=h)}
e <- tsCV(y, far2, h=1)
mean(e^2, na.rm = TRUE)

aic_vals_temp <- NULL
aic_vas <- NULL 

for(i in 1:5) {
  for(j in 1:5) {
    xreg1 <- fourier()
  }
}


y <- ts(day$cnt, frequency=7)
z <- fourier(ts(day$cnt, frequency=365.25), K=5)
zf <- fourier(ts(day$cnt, frequency=365.25), K=5, h=100)
fit <- auto.arima(y, xreg=cbind(day$workingday,day$yr), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(day$workingday,day$yr), h=100)
plot(fc)

fit <- auto.arima(y)
autoplot(fit)
fcast <- forecast(fit)
plot(fcast)

ts <- ts(day$cnt, start=c(2011,1,1),frequency=365.25)
plot.ts(ts)
plot(decompose(ts))
plot(stl(ts,
         s.window="periodic"))
MK = MannKendall(ts)
SMK = SeasonalMannKendall(ts)
sea.sens.slope(ts)

x <- day$cnt
ets(x)
fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal

# create numeric day/hour counter (starting counting with 1 at the beginning of the period)
hour[ , day_counter := .GRP, by = c("dteday")]
hour[ , time_counter := day_counter + hr - 1 + (day_counter - 1)*23]
day[, time_counter := .GRP, by = c("dteday")]

# obtain results for prediciton on equally spacex N = 200 Xs
results_test <- kernel_predictions(df = hour_train_rand, 
                                   test_df = hour_test_rand, 
                                   myx = "hr", 
                                   myy = "cnt",
                                   a = min(hour$hr),
                                   b = max(hour$hr),
                                   m = 10) 

# overall (day-hour)
regressogram(hour$time_counter, hour$cnt)

# ARIMA(1,0,2)(0,1,0)[365] with drift 
fit <- auto.arima(y)
autoplot(fit)
fcast <- forecast(fit)
plot(fcast)
plot(forecast(fit))
# day only 
regressogram(day$time_counter, day$cnt)

if (class(fit2) != "Arima" && class(fit2) != "estimate") {
  print("STOP")
}

fit2 <- fit
class(fit2) <- c("Arima", "forecast_ARIMA", "ARIMA")




################## Multilayer Perceptrons (MLP): MULTIVARIATE ########################

# Fit MLP (trianing data does not have full 2 years, so only monthyly and weekly seasonality)
ts_cnt <- ts(day_train_forecast$cnt)
mlp_fit <- mlp(ts_cnt, xreg = day_train_forecast[, ..covs], 
               allow.det.season = TRUE)
mlp_forecast <- forecast(mlp_fit, h = nrow(day_test_forecast), xreg = day[, ..covs])

plot(mlp_fit)

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

##################  Extreme Learning Machine (ELM) ########################

ts_cnt <- ts(day_train_forecast$cnt)
elm_fit <- elm(ts_cnt)
elm_forecast <- forecast(elm_fit,h=nrow(day_test_forecast))
plot(elm_forecast)


results <- data.table(x = day_test_forecast$dteday)
results[, y := day_test_forecast$cnt]
results[, forecast :=  elm_forecast$mean[1:nrow(day_test_forecast)]]
results[, test := ifelse(x <= "2012-11-30", 0, 1)]
results[test == 0, fitted := c(rep(NA, 4), mlp_fit$fitted)]
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


################# SELECTING LAGS USING DIMENSION REDUCTION #################### 

# Step 1: Create ALL POSSIBLE Lags
day_train_forecast_lags <- day_train_forecast
max_lag <- nrow(day_train_forecast_lags) - 1
day_train_forecast_lags[, paste0("cnt_lag", seq(1:max_lag)) := shift(cnt, 1:max_lag) ]

day_train_forecast_lags <- day_train_forecast_lags[, -c("casual", "registered", "index", "dteday", "cnt", "instant", "bins")]
x <- model.matrix(cnt~., day_train_forecast_lags)[,-1]


x <- as.matrix(day_train_forecast_lags)
y <- as.vector(day_train_forecast$cnt)


myvars <- colnames(day_train_forecast_lags)[!colnames(day_train_forecast_lags) %in% "cnt"]
myformula <- as.formula(paste("cnt", paste(myvars, collapse=" + "), sep=" ~ "))

# 
attach(day_train_forecast_lags)
dwtest(myformula)
detach(day_train_forecast_lags)
############ ... LASSO #####################
lasso <- glmnet(x, y, alpha = 1)

lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)


x <- as.matrix(day_train_forecast[, ..covs])
y <- as.vector(day_train_forecast$cnt)


krr(x, y)



x <- as.matrix(day_train_forecast[, ..covs])
y <- as.vector(day_train_forecast$cnt)

x <- as.matrix(test[, ..covs])
y <- as.vector(day_train_forecast$cnt)

kkrr_test <- krr(x, y)

# Step 1: Create ALL POSSIBLE Lags
day_train_forecast_lags <- day_train_forecast
max_lag <- nrow(day_train_forecast_lags) - 1
day_train_forecast_lags[, paste0("cnt_lag", seq(1:max_lag)) := shift(cnt, 1:max_lag) ]

day_train_forecast_lags <- day_train_forecast_lags[, -c("casual", "registered", "index", "dteday", "cnt", "instant", "bins")]
x <- model.matrix(cnt~., day_train_forecast_lags)[,-1]


x <- as.matrix(day_train_forecast_lags)
y <- as.vector(day_train_forecast$cnt)

# create matrix with lags of test data
xnew <- day_test_forecast[, -c("casual", "registered", "index", "dteday", "cnt", "instant")]

ynew <- predict(kkrr_test, xnew)


########## PCA ################# 

pca_covariates <- prcomp(day_train_forecast[, ..covs], scale = TRUE, center = TRUE)
summary(pca_covariates)

# get principal component score vectors 
pc_score_vectors <- as.data.table(pca_covariates$x)
pca1_covs <- pc_score_vectors$PC1

############ ... RIDGE #####################

############ ... ELASTIC NET #####################

############ ... Kernel Ridge Regression #####################


############ ... Kernel Ridge Regression #####################



loess_data <- day_train_forecast[, c("index", "cnt")]
loess_data_test <- day_test_forecast[, c("index", "cnt")]

loess_cv <- loess.as(loess_data$index,
                     loess_data$cnt,
                     criterion = "gcv", 
                     degree = d)
loess_cv_span <- loess_cv$pars$span

# refit using best cv span
loess_cv <- loess(cnt ~ index, 
                  data = day_train_forecast, 
                  span = loess_cv_span, degree = d)

loess_fitted <- predict(loess_cv, day_test_forecast)

# calcualte mse 
mse_cv <- mean((day_test_forecast$cnt - loess_fitted)^2)


mse_order[d + 1, 1] <- d
mse_order[d + 1, 2] <- mse_cv
mse_order[d + 1, 3] <- loess_cv_span


# #degree of local polynomials: 0, 1, 2: cross validate
# # in training data: cross validate span
# mse_order <- matrix(nrow = 3, ncol = 3)
# 
# for(d in 0:2) {
#   
#   
#   loess_cv <- loess.as(day_train_forecast$index,
#                        day_train_forecast$cnt,
#                        criterion = "gcv", 
#                        degree = d)
#   loess_cv_span <- loess_cv$pars$span
#   
#   # apply to test data 
#   loess_test <- loess.as(day_test_forecast$index, 
#                          day_test_forecast$cnt, 
#                          user.span = loess_cv_span, 
#                          degree = d)
#   
#   # calcualte mse 
#   mse_cv <- mean((day_test_forecast$cnt -  loess_test$fitted)^2)
#   
#   
#   mse_order[d + 1, 1] <- d
#   mse_order[d + 1, 2] <- mse_cv
#   mse_order[d + 1, 3] <- loess_cv_span
# }

# # plot results
# plot(results$x,results$y,col="black", 
#      main = "Nonparametric Regression Methods - Dataset A (Glass Fragments)", 
#      xlab = "Aluminium Content", ylab = "Refractive Index", pch = 1, 
#      cex.main = 1, cex = 0.5)
# lines(results$x,results$fitted_train,col="green")  
# lines(results$x,results$fitted_test,col="blue")   
# legend(max(results$x - 1), max(results$y - 1),
#        legend=c("Datapoints", "Regressogram", "Boxcar"),
#        pch = c(19, NA, NA), lty = c(NA, 1, 2),
#        col=c("black","green", "blue"),  cex=0.8) 

# labs(title = paste0("Loess"),
#      subtitle = paste0("Test RMSE: ", round(sqrt(mse_loess_test)),
#                        "; Training RMSE: ", round(sqrt(mse_loess_train)), 
#                        " (Cross-Validated Smoothing Span: ", round(best_span, 3),
#                        "; Cross-Validated Degree: ", best_degree, ")"), 
#      x = "Date", y = "Bike Count") +
