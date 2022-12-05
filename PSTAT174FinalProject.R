setwd("/Volumes/MICROSD")

# load useful libraries for time series analysis, plotting, and forecast.
library(jsonlite)
library(MASS)
library(forecast)
library(astsa)
library(greybox)
library(lubridate)

# --------------------------------------------------------------------------------------------------------- #

# 1) Loading data into R.
# getting spy data.
ticker = "spy"
timeframe = "daily"
path = paste("data/", timeframe, "/", ticker, ".json", sep="")
price_data_spy <- subset(fromJSON(path, flatten=TRUE), select = -c(high, low, close, volume))

# getting google data.
ticker = "goog"
timeframe = "daily"
path = paste("data/", timeframe, "/", ticker, ".json", sep="")
price_data_goog <- subset(fromJSON(path, flatten=TRUE), select = -c(high, low, close, volume))
# we have to adjust price for google due to a 1:21 split that happened recently.
price_data_goog$open[4486:length(price_data_goog$open)] = 21* price_data_goog$open[4486:length(price_data_goog$open)]

# getting aaple data.
ticker = "aapl"
timeframe = "daily"
path = paste("data/", timeframe, "/", ticker, ".json", sep="")
price_data_aapl <- subset(fromJSON(path, flatten=TRUE), select = -c(high, low, close, volume))

# getting tesla data.
ticker = "tsla"
timeframe = "daily"
path = paste("data/", timeframe, "/", ticker, ".json", sep="")
price_data_tsla <- subset(fromJSON(path, flatten=TRUE), select = -c(high, low, close, volume))
# we have to adjust price for google due to a 1:21 split that happened recently.
price_data_tsla$open[3058:length(price_data_tsla$open)] = 3 * price_data_tsla$open[3058:length(price_data_tsla$open)]

# --------------------------------------------------------------------------------------------------------- #

# 2) Change the datasets to monthly prices and fill in any gaps.
#    this is done to avoid dealing with weekday/weekend/holiday price gaps.
# SPY
partial_spy <- data.frame(
  date = as.Date(price_data_spy$datetime),
  price = price_data_spy$open
)
start <- as.Date(price_data_spy$datetime[1])
end <- as.Date("2022/11/30")
full <- seq(start, by='1 week', length=(end-start)/7)

cleaned_data <- with(partial_spy, price[match(full, date)])
# fill in NA values manually.
for (i in 1:length(cleaned_data)) {
  if (is.na(cleaned_data[i])) {
    cleaned_data[i] = cleaned_data[i-1]
  }
}
spy <- cleaned_data

# GOOGLE
partial_goog <- data.frame(
  date = as.Date(price_data_goog$datetime),
  price = price_data_goog$open
)
start <- as.Date(price_data_goog$datetime[1])
end <- as.Date("2022/11/30")
full <- seq(start, by='1 month', length=(end-start)/31)

cleaned_data <- with(partial_goog, price[match(full, date)])
# fill in NA values manually.
for (i in 1:length(cleaned_data)) {
  if (is.na(cleaned_data[i])) {
    cleaned_data[i] = cleaned_data[i-1]
  }
}
goog <- cleaned_data

# AAPLE
partial_aapl <- data.frame(
  date = as.Date(price_data_aapl$datetime),
  price = price_data_aapl$open
)
start = as.Date(price_data_aapl$datetime[1])
end = as.Date("2022/11/30")
full <- seq(start, by='1 month', length=(end-start)/31)

cleaned_data <- with(partial_aapl, price[match(full, date)])
# fill in NA values manually.
for (i in 1:length(cleaned_data)) {
  if (is.na(cleaned_data[i])) {
    cleaned_data[i] = cleaned_data[i-1]
  }
}
aapl <- cleaned_data

# TESLA
partial_tsla <- data.frame(
  date = as.Date(price_data_tsla$datetime),
  price = price_data_tsla$open
)
start = as.Date(price_data_tsla$datetime[1])
end = as.Date("2022/11/30")
full <- seq(start, by='1 month', length=(end-start)/31)

cleaned_data <- with(partial_tsla, price[match(full, date)])
# fill in NA values manually.
for (i in 1:length(cleaned_data)) {
  if (is.na(cleaned_data[i])) {
    cleaned_data[i] = cleaned_data[i-1]
  }
}
tsla <- cleaned_data

par(mfrow=c(2,2))
orig_spy <- ts(spy, start = c(substr(partial_spy$date[1], 1, 4),
                              substr(partial_spy$date[1], 6, 7), 
                              substr(partial_spy$date[1], 9, 10)), 
               frequency = 52)
orig_goog <- ts(goog, start = c(substr(partial_goog$date[1], 1, 4),
                              substr(partial_goog$date[1], 6, 7), 
                              substr(partial_goog$date[1], 9, 10)), 
               frequency = 12)
orig_aapl <- ts(aapl, start = c(substr(partial_aapl$date[1], 1, 4),
                              substr(partial_aapl$date[1], 6, 7), 
                              substr(partial_aapl$date[1], 9, 10)), 
               frequency = 12)
orig_tsla <- ts(tsla, start = c(substr(partial_tsla$date[1], 1, 4),
                              substr(partial_tsla$date[1], 6, 7), 
                              substr(partial_tsla$date[1], 9, 10)), 
               frequency = 12)
ts.plot(orig_spy, ylab = "Price", main="SPY Daily Open Prices 2002-2022")
ts.plot(orig_goog, ylab = "Price", main="GOOG Daily Open Prices 2004-2022")
ts.plot(orig_aapl, ylab = "Price", main="AAPL Daily Open Prices 2002-2022")
ts.plot(orig_tsla, ylab = "Price", main="TSLA Daily Open Prices 2010-2022")

# --------------------------------------------------------------------------------------------------------- #

# 3) Split data into train and test sets.
percent_split = 0.8
# SPY
train_spy <- ts(orig_spy[1:(length(orig_spy)*percent_split)], 
                 start = c(substr(partial_spy$date[1], 1, 4), 
                           substr(partial_spy$date[1], 6, 7), 
                           substr(partial_spy$date[1], 9, 10)),
                 frequency = 52)
test_spy <- ts(orig_spy[(length(orig_spy)*percent_split + 1):length(orig_spy)],
                start = c(end(train_spy)[1],
                          end(train_spy)[2] + 1),
                frequency = 52)

par(mfrow=c(1,2))
ts.plot(orig_spy,
        ylab = "Price",
        main="SPY Daily Open Prices 2002-2022")
ts.plot(train_spy,
        test_spy,
        col = c("black", "red"),
        main="SPY Train and Test Split")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# GOOGLE
current_t <- time(orig_goog)
train_goog <- ts(orig_goog[1:(length(orig_goog)*percent_split)], 
                start = c(substr(current_t[1], 1, 4), 
                          substr(current_t[1], 6, 7), 
                          substr(current_t[1], 9, 10)),
                frequency = 12)
test_goog <- ts(orig_goog[(length(orig_goog)*percent_split + 1):length(orig_goog)],
                start = c(end(train_goog)[1],
                          end(train_goog)[2] + 1),
               frequency = 12)

par(mfrow=c(1,2))
ts.plot(orig_goog,
        ylab = "Price",
        main="GOOG Daily Open Prices 2004-2022")
ts.plot(train_goog,
        test_goog,
        col = c("black", "red"),
        main="GOOG Train and Test Split")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# AAPLE
train_aapl <- ts(orig_aapl[1:(length(orig_aapl)*percent_split)], 
                start = c(substr(partial_aapl$date[1], 1, 4), 
                          substr(partial_aapl$date[1], 6, 7), 
                          substr(partial_aapl$date[1], 9, 10)),
                frequency = 12)
test_aapl <- ts(orig_aapl[(length(orig_aapl)*percent_split + 1):length(orig_aapl)],
               start = c(end(train_aapl)[1],
                         end(train_aapl)[2] + 1),
               frequency = 12)

par(mfrow=c(1,2))
ts.plot(orig_aapl,
        ylab = "Price",
        main="AAPL Daily Open Prices 2002-2022")
ts.plot(train_aapl,
        test_aapl,
        col = c("black", "red"),
        main="AAPL Train and Test Split")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# TESLA
train_tsla <- ts(orig_tsla[1:(length(orig_tsla)*percent_split)], 
                start = c(substr(partial_tsla$date[1], 1, 4), 
                          substr(partial_tsla$date[1], 6, 7), 
                          substr(partial_tsla$date[1], 9, 10)),
                frequency = 12)
test_tsla <- ts(orig_tsla[(length(orig_tsla)*percent_split + 1):length(orig_tsla)],
                start = c(end(train_tsla)[1],
                          end(train_tsla)[2] + 1),
                frequency = 12)

par(mfrow=c(1,2))
ts.plot(orig_tsla,
        ylab = "Price",
        main="TSLA Daily Open Prices 2010-2022")
ts.plot(train_tsla,
        test_tsla,
        col = c("black", "red"),
        main="TSLA Train and Test Split")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# --------------------------------------------------------------------------------------------------------- #
# We will first work with SPY until a forecast can be made, then move on to other tickers.
# --------------------------------SPY------------------------------ #
# 4) Transforming data.
# log transform.
train_spy_log <- log(train_spy)
test_spy_log <- log(test_spy)
# boxcox transform.
t = 1:length(train_spy)
boxcox_transform <- boxcox(train_spy ~ t, plotit = FALSE)
lambda <- boxcox_transform$x[which(boxcox_transform$y == max(boxcox_transform$y))]
train_spy_boxcox <- (1/lambda)*(train_spy^lambda-1)
test_spy_boxcox <- (1/lambda)*(test_spy^lambda-1)

par(mfrow=c(1,1))
ts.plot(train_spy_log, 
        test_spy_log,
        ylab = "Log(Price)",
        col = c("black", "red"), 
        main="Log Transform of SPY Daily Open Prices")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# check acf and pacf.
par(mfrow=c(1,2))
acf(train_spy_log,
    lag.max = 60)
title(main = "SPY Autocorrelations")
pacf(train_spy_log,
     lag.max = 60)
title(main = "SPY Partial Autocorrelations")

# difference to remove seasonality.
seaonal_term <- 10
y10 <- diff(train_spy_log, seaonal_term)
y10.2 <- diff(y10, seaonal_term)
var(y10); var(y10.2)

# difference to remove trend.
par(mfrow=c(1,1))
y1 <- diff(y10, 1)
var(y10); var(y1)
ts.plot(y1, main="De-trended and De-seaonalized SPY Open Prices")

t = 1:length(y1)
linear_fit <- lm(y1 ~ t)
abline(linear_fit, t, col = c("red"))
abline(h = mean(y1), col = c("blue"))
legend("topleft",
       legend = c("SPY Data",
                  "Linear Fit of Residuals",
                  "Mean of Residuals"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 1))

# --------------------------------------------------------------------------------------------------------- #

# 5) Estimate parameters.
# check acf and pacf of de-trended de-seasonalized series.
par(mfrow=c(1,2))
acf(y1, lag.max = 52, na.action = na.pass)
title(main = "De-trended SPY Autocorrelations")
pacf(y1, lag.max = 52, na.action = na.pass)
title(main = "De-trended SPY Partial Autocorrelations")

# Identifying possible models.
df <- expand.grid(p=c(0,3), q=c(0,3), P=c(0,4), Q=c(1))
df <- cbind(df, AICc=NA)
df

# Compute AICc.
for (i in 1:nrow(df)) {
  sarima.obj <- NULL
  try(arima.obj <- arima(train_spy_log, order=c(df$p[i], 1, df$q[i]),
                         seasonal=list(order=c(df$P[i], 1, df$Q[i]), period=seaonal_term),
                         method="ML"))
  if (!is.null(arima.obj)) { df$AICc[i] <- AICc(arima.obj) }
  print(df[i, ])
}
# sort the dataframe by AICc in the increasing order.
df <- df[order(df$AICc),]
# we will work on the 3 models with the lowest AICc.
result <- df[1:3,]
result

# Top 3 candidate models.
fit1 <- arima(train_spy_log, order=c(result$p[1], 1, result$q[1]),
             seasonal=list(order=c(result$P[1], 1, result$Q[1]), period=seaonal_term),
             method="ML")
fit2 <- arima(train_spy_log, order=c(result$p[2], 1, result$q[2]),
              seasonal=list(order=c(result$P[2], 1, result$Q[2]), period=seaonal_term),
              method="ML")
fit3 <- arima(train_spy_log, order=c(result$p[3], 1, result$q[3]),
              seasonal=list(order=c(result$P[3], 1, result$Q[3]), period=seaonal_term),
              method="ML")

# --------------------------------------------------------------------------------------------------------- #

# 6) Model Diagnostics on the top 3 models.
res <- residuals(fit3)
mean(res); var(res)

# Plot residuals.
par(mfrow=c(1, 1))
ts.plot(res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

shapiro.test(res)

modified_res <- res
modified_res[which(modified_res <= min(modified_res) + 0.2)] = 0
modified_res[which(modified_res >= max(modified_res)- 0.1)] = 0

par(mfrow=c(1, 1))
ts.plot(modified_res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(modified_res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

# shapiro-wilk normality test.
shapiro.test(modified_res)

# histogram and qq plot of residuals.
par(mfrow=c(1, 2))
hist(modified_res,main = "Histogram", breaks=50)
qqnorm(modified_res)
qqline(modified_res,col ="blue")

acf(res, lag.max = 100)
title(main = "ACF of Residuals")
pacf(res, lag.max = 100)
title(main = "Partial ACF of Residuals")

# testing residuals for independence.
Box.test(res, lag = floor(sqrt(length(res))), type = c("Box-Pierce"), fitdf = 3)
Box.test(res, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 3)
Box.test(res^2, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 3)

# --------------------------------------------------------------------------------------------------------- #

# 6) Forecasting SPY Prices.
# We want to remember to reverse the boxcox transform and check prediction on true values.
num_to_predict <- 24
mypred <- predict(fit1, n.ahead=num_to_predict)
plot(c(orig_spy),
     col = c("black"),
     xlim=c(length(train_spy) - 2*num_to_predict, length(train_spy) + 2*num_to_predict),
     ylim=c(min(exp(mypred$pred - 1.96*mypred$se), na.rm = TRUE)*0.99, max(exp(mypred$pred + 1.96*mypred$se), na.rm = TRUE)*1.01),
     xlab = "Index",
     ylab = "Price",
     pch = 0, lty=1)
points((length(train_spy) + 1):(length(test_spy) + length(train_spy)),
       test_spy,
       col="blue",
       pch = 0)
points((length(train_spy) + 1):(length(train_spy) + num_to_predict),
       exp(mypred$pred),
       col="red",
       pch = 1)
lines((length(train_spy) + 1):(length(train_spy) + num_to_predict),
      exp(mypred$pred + 1.96*mypred$se),
      lty=2, col = c("red4"))
lines((length(train_spy) + 1):(length(train_spy) + num_to_predict),
      exp(mypred$pred - 1.96*mypred$se),
      lty=2, col = c("red4"))
lines((length(train_spy) + 1):(length(train_spy) + num_to_predict),
      exp(mypred$pred + 1.645*mypred$se),
      lty=2, col = c("red"))
lines((length(train_spy) + 1):(length(train_spy) + num_to_predict),
      exp(mypred$pred - 1.645*mypred$se),
      lty=2, col = c("red"))

title(main = "Predicting SPY Price with SARIMA(0,1,0)x(4,1,1)[10]")
legend("topleft",
       legend = c("Train Data", "Test Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "blue", "red", "red", "red4"),
       lty = c(-1, -1, -1, 2, 2),
       pch = c(0, 0, 1, -1, -1))

# ------------------------------------------------------ #
# Predicting out-of-sample spy prices.
# Retrain the model on the full data.
fit <- arima(log(orig_spy), order=c(result$p[2], 1, result$q[2]),
              seasonal=list(order=c(result$P[2], 1, result$Q[2]), period=seaonal_term),
              method="ML")

mypred <- predict(fit, n.ahead=num_to_predict)
pred_ts <- ts(exp(mypred$pred),
              start = c(end(orig_spy)[1],
                        end(orig_spy)[2] + 1),
              
              frequency = 12)
upper_pred_ts_95 <- ts(exp(mypred$pred + 1.96*mypred$se),
                    start = c(end(orig_spy)[1],
                              end(orig_spy)[2] + 1), 
                    frequency = 12)
lower_pred_ts_95 <- ts(exp(mypred$pred - 1.96*mypred$se),
                    start = c(end(orig_spy)[1],
                              end(orig_spy)[2] + 1),
                    frequency = 12)
upper_pred_ts_90 <- ts(exp(mypred$pred + 1.645*mypred$se),
                       start = c(end(orig_spy)[1],
                                 end(orig_spy)[2] + 1), 
                       frequency = 12)
lower_pred_ts_90 <- ts(exp(mypred$pred - 1.645*mypred$se),
                       start = c(end(orig_spy)[1],
                                 end(orig_spy)[2] + 1),
                       frequency = 12)
ts.plot(orig_spy,
        pred_ts,
        upper_pred_ts_95,
        lower_pred_ts_95,
        upper_pred_ts_90,
        lower_pred_ts_90,
        col = c("black", "red", "red4", "red4", "red", "red"),
        ylim=c(min(orig_spy)*0.8, max(orig_spy)*1.2),
        xlab = "Index",
        ylab = "Price",
        lty = c(1, 1, 2, 2))

title(main = "Out-of-Sample SPY Forecast Using SARIMA(0,1,0)x(4,1,1)[10]")
legend("topleft",
       legend = c("Actual Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "red", "red", "red4"),
       lty = c(1, 1, 2, 2))
# ------------------------------------------------------ #

# ------------------------GOOGLE------------------------ #
train_goog_log <- log(train_goog)
test_goog_log <- log(test_goog)
# boxcox transform.
t = 1:length(train_goog)
boxcox_transform <- boxcox(train_goog ~ t, plotit = FALSE)
lambda <- boxcox_transform$x[which(boxcox_transform$y == max(boxcox_transform$y))]
train_goog_boxcox <- (1/lambda)*(train_goog^lambda-1)
test_goog_boxcox <- (1/lambda)*(test_goog^lambda-1)
orig_goog_boxcox <- (1/lambda)*(orig_goog^lambda-1)

par(mfrow=c(1,2))
ts.plot(train_goog_boxcox, 
        test_goog_boxcox,
        ylab = "Boxcox(Price)",
        col = c("black", "red"), 
        main="Boxcox Transform of GOOG Daily Open Prices")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))
ts.plot(train_goog_log, 
        test_goog_log,
        ylab = "Log(Price)",
        col = c("black", "red"), 
        main="Log Transform of GOOG Daily Open Prices")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# check acf and pacf.
par(mfrow=c(1,2))
acf(train_goog_boxcox,
    lag.max = 60)
title(main = "goog Autocorrelations")
pacf(train_goog_boxcox,
     lag.max = 60)
title(main = "goog Partial Autocorrelations")

# difference to remove seasonality.
seaonal_term <- 12
y10 <- diff(train_goog_boxcox, seaonal_term)
y10.2 <- diff(y10, seaonal_term)
var(y10); var(y10.2)

# difference to remove trend.
par(mfrow=c(1,1))
y1 <- diff(y10, 1)
var(y10); var(y1)
ts.plot(y1, main="De-trended and De-seaonalized goog Open Prices")

t = 1:length(y1)
linear_fit <- lm(y1 ~ t)
abline(linear_fit, t, col = c("red"))
abline(h = mean(y1), col = c("blue"))
legend("topleft",
       legend = c("goog Data",
                  "Linear Fit of Residuals",
                  "Mean of Residuals"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 1))

# --------------------------------------------------------------------------------------------------------- #

# 5) Estimate parameters.
# check acf and pacf of de-trended de-seasonalized series.
par(mfrow=c(1,2))
acf(y1, lag.max = 100, na.action = na.pass)
title(main = "De-trended goog Autocorrelations")
pacf(y1, lag.max = 100, na.action = na.pass)
title(main = "De-trended goog Partial Autocorrelations")

# Identifying possible models.
df <- expand.grid(p=c(0), q=c(0), P=c(0,1,2), Q=c(1))
df <- cbind(df, AICc=NA)
df

# Compute AICc.
for (i in 1:nrow(df)) {
  sarima.obj <- NULL
  try(arima.obj <- arima(train_goog_boxcox, order=c(df$p[i], 1, df$q[i]),
                         seasonal=list(order=c(df$P[i], 1, df$Q[i]), period=seaonal_term),
                         method="ML"))
  if (!is.null(arima.obj)) { df$AICc[i] <- AICc(arima.obj) }
  print(df[i, ])
}
# sort the dataframe by AICc in the increasing order.
df <- df[order(df$AICc),]
# we will work on the 3 models with the lowest AICc.
result <- df[1:3,]
result

# Top 3 candidate models.
fit1 <- arima(train_goog_boxcox, order=c(result$p[1], 1, result$q[1]),
              seasonal=list(order=c(result$P[1], 1, result$Q[1]), period=seaonal_term),
              method="ML")
fit2 <- arima(train_goog_boxcox, order=c(result$p[2], 1, result$q[2]),
              seasonal=list(order=c(result$P[2], 1, result$Q[2]), period=seaonal_term),
              method="ML")
fit3 <- arima(train_goog_boxcox, order=c(result$p[3], 1, result$q[3]),
              seasonal=list(order=c(result$P[3], 1, result$Q[3]), period=seaonal_term),
              method="ML")

# --------------------------------------------------------------------------------------------------------- #

# 6) Model Diagnostics on the top 3 models.
res <- residuals(fit3)
mean(res); var(res)

# Plot residuals.
par(mfrow=c(1, 1))
ts.plot(res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

shapiro.test(res)

modified_res <- res
modified_res[which(modified_res <= min(modified_res) + 0.2)] = 0
modified_res[which(modified_res >= max(modified_res)- 0.1)] = 0

par(mfrow=c(1, 1))
ts.plot(modified_res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(modified_res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

# shapiro-wilk normality test.
shapiro.test(modified_res)

# histogram and qq plot of residuals.
par(mfrow=c(1, 2))
hist(modified_res,main = "Histogram", breaks=50)
qqnorm(modified_res)
qqline(modified_res,col ="blue")

acf(res, lag.max = 100)
title(main = "ACF of Residuals")
pacf(res, lag.max = 100)
title(main = "Partial ACF of Residuals")

# testing residuals for independence.
Box.test(res, lag = floor(sqrt(length(res))), type = c("Box-Pierce"), fitdf = 0)
Box.test(res, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 0)
Box.test(res^2, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 0)

# --------------------------------------------------------------------------------------------------------- #

# 6) Forecasting goog Prices.
# We want to remember to reverse the boxcox transform and check prediction on true values.
par(mfrow=c(1, 1))
num_to_predict <- 12
mypred <- predict(fit3, n.ahead=num_to_predict)
plot(c(orig_goog),
     col = c("black"),
     xlim=c(length(train_goog) - 2*num_to_predict, length(train_goog) + 2*num_to_predict),
     ylim=c(min(((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda)), na.rm = TRUE)*0.99, max(((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda)), na.rm = TRUE)*1.01),
     xlab = "Index",
     ylab = "Price",
     pch = 0, lty=1)
points((length(train_goog) + 1):(length(test_goog) + length(train_goog)),
       test_goog,
       col="blue",
       pch = 0)
points((length(train_goog) + 1):(length(train_goog) + num_to_predict),
       ((lambda*mypred$pred + 1)^(1/lambda)),
       col="red",
       pch = 1)
lines((length(train_goog) + 1):(length(train_goog) + num_to_predict),
      ((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red4"))
lines((length(train_goog) + 1):(length(train_goog) + num_to_predict),
      ((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red4"))
lines((length(train_goog) + 1):(length(train_goog) + num_to_predict),
      ((lambda*(mypred$pred + 1.645*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red"))
lines((length(train_goog) + 1):(length(train_goog) + num_to_predict),
      ((lambda*(mypred$pred - 1.645*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red"))

title(main = "Predicting GOOG Price with SARIMA(0,1,0)x(2,1,0)[12]")
legend("topleft",
       legend = c("Train Data", "Test Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "blue", "red", "red", "red4"),
       lty = c(-1, -1, -1, 2, 2),
       pch = c(0, 0, 1, -1, -1))

# ------------------------------------------------------ #
# Predicting out-of-sample goog prices.
# Retrain the model on the full data.
fit <- arima(orig_goog_boxcox, order=c(result$p[3], 1, result$q[3]),
             seasonal=list(order=c(result$P[3], 1, result$Q[3]), period=seaonal_term),
             method="ML")

mypred <- predict(fit, n.ahead=num_to_predict)
pred_ts <- ts(((lambda*mypred$pred + 1)^(1/lambda)),
              start = c(end(orig_goog)[1],
                        end(orig_goog)[2] + 1),
              
              frequency = 12)
upper_pred_ts_95 <- ts(((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_goog)[1],
                                 end(orig_goog)[2] + 1), 
                       frequency = 12)
lower_pred_ts_95 <- ts(((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_goog)[1],
                                 end(orig_goog)[2] + 1),
                       frequency = 12)
upper_pred_ts_90 <- ts(((lambda*(mypred$pred - 1.645*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_goog)[1],
                                 end(orig_goog)[2] + 1), 
                       frequency = 12)
lower_pred_ts_90 <- ts(((lambda*(mypred$pred + 1.645*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_goog)[1],
                                 end(orig_goog)[2] + 1),
                       frequency = 12)
ts.plot(orig_goog,
        pred_ts,
        upper_pred_ts_95,
        lower_pred_ts_95,
        upper_pred_ts_90,
        lower_pred_ts_90,
        col = c("black", "red", "red4", "red4", "red", "red"),
        ylim=c(min(orig_goog)*0.8, max(orig_goog)*1.2),
        xlab = "Index",
        ylab = "Price",
        lty = c(1, 1, 2, 2))

title(main = "Out-of-Sample GOOG Forecast Using SARIMA(0,1,0)x(2,1,0)[12]")
legend("topleft",
       legend = c("Actual Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "red", "red", "red4"),
       lty = c(1, 1, 2, 2))
# ------------------------------------------------------ #

# ------------------------APPLE------------------------ #
train_aapl_log <- log(train_aapl)
test_aapl_log <- log(test_aapl)
# boxcox transform.
t = 1:length(train_aapl)
boxcox_transform <- boxcox(train_aapl ~ t, plotit = FALSE)
lambda <- boxcox_transform$x[which(boxcox_transform$y == max(boxcox_transform$y))]
train_aapl_boxcox <- (1/lambda)*(train_aapl^lambda-1)
test_aapl_boxcox <- (1/lambda)*(test_aapl^lambda-1)
orig_aapl_boxcox <- (1/lambda)*(orig_aapl^lambda-1)

par(mfrow=c(1,2))
ts.plot(train_aapl_boxcox, 
        test_aapl_boxcox,
        ylab = "Boxcox(Price)",
        col = c("black", "red"), 
        main="Boxcox Transform of aapl Daily Open Prices")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))
ts.plot(train_aapl_log, 
        test_aapl_log,
        ylab = "Log(Price)",
        col = c("black", "red"), 
        main="Log Transform of aapl Daily Open Prices")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# check acf and pacf.
par(mfrow=c(1,2))
acf(train_aapl_boxcox,
    lag.max = 60)
title(main = "aapl Autocorrelations")
pacf(train_aapl_boxcox,
     lag.max = 60)
title(main = "aapl Partial Autocorrelations")

# difference to remove seasonality.
seaonal_term <- 10
y10 <- diff(train_aapl_boxcox, seaonal_term)
y10.2 <- diff(y10, seaonal_term)
var(y10); var(y10.2)

# difference to remove trend.
par(mfrow=c(1,1))
y1 <- diff(y10, 1)
var(y10); var(y1)
ts.plot(y1, main="De-trended and De-seaonalized aapl Open Prices")

t = 1:length(y1)
linear_fit <- lm(y1 ~ t)
abline(linear_fit, t, col = c("red"))
abline(h = mean(y1), col = c("blue"))
legend("topleft",
       legend = c("AAPL Data",
                  "Linear Fit of Residuals",
                  "Mean of Residuals"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 1))

# --------------------------------------------------------------------------------------------------------- #

# 5) Estimate parameters.
# check acf and pacf of de-trended de-seasonalized series.
par(mfrow=c(1,2))
acf(y1, lag.max = 100, na.action = na.pass)
title(main = "De-trended aapl Autocorrelations")
pacf(y1, lag.max = 100, na.action = na.pass)
title(main = "De-trended aapl Partial Autocorrelations")

# Identifying possible models.
df <- expand.grid(p=c(0,2,4), q=c(0,2,3), P=c(0,1,2), Q=c(1))
df <- cbind(df, AICc=NA)
df

# Compute AICc.
for (i in 1:nrow(df)) {
  sarima.obj <- NULL
  try(arima.obj <- arima(train_aapl_boxcox, order=c(df$p[i], 1, df$q[i]),
                         seasonal=list(order=c(df$P[i], 1, df$Q[i]), period=seaonal_term),
                         method="ML"))
  if (!is.null(arima.obj)) { df$AICc[i] <- AICc(arima.obj) }
  print(df[i, ])
}
# sort the dataframe by AICc in the increasing order.
df <- df[order(df$AICc),]
# we will work on the 3 models with the lowest AICc.
result <- df[1:3,]
result

# Top 3 candidate models.
fit1 <- arima(train_aapl_boxcox, order=c(result$p[1], 1, result$q[1]),
              seasonal=list(order=c(result$P[1], 1, result$Q[1]), period=seaonal_term),
              method="ML")
fit2 <- arima(train_aapl_boxcox, order=c(result$p[2], 1, result$q[2]),
              seasonal=list(order=c(result$P[2], 1, result$Q[2]), period=seaonal_term),
              method="ML")
fit3 <- arima(train_aapl_boxcox, order=c(result$p[3], 1, result$q[3]),
              seasonal=list(order=c(result$P[3], 1, result$Q[3]), period=seaonal_term),
              method="ML")

# --------------------------------------------------------------------------------------------------------- #

# 6) Model Diagnostics on the top 3 models.
res <- residuals(fit1)
mean(res); var(res)

# Plot residuals.
par(mfrow=c(1, 1))
ts.plot(res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

shapiro.test(res)

modified_res <- res
modified_res[which(modified_res <= min(modified_res) + 0.2)] = 0
modified_res[which(modified_res >= max(modified_res)- 0.1)] = 0

par(mfrow=c(1, 1))
ts.plot(modified_res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(modified_res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

# shapiro-wilk normality test.
shapiro.test(modified_res)

# histogram and qq plot of residuals.
par(mfrow=c(1, 2))
hist(modified_res,main = "Histogram", breaks=50)
qqnorm(modified_res)
qqline(modified_res,col ="blue")

acf(res, lag.max = 100)
title(main = "ACF of Residuals")
pacf(res, lag.max = 100)
title(main = "Partial ACF of Residuals")

# testing residuals for independence.
Box.test(res, lag = floor(sqrt(length(res))), type = c("Box-Pierce"), fitdf = 2)
Box.test(res, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 2)
Box.test(res^2, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 2)

# --------------------------------------------------------------------------------------------------------- #

# 6) Forecasting aapl Prices.
# We want to remember to reverse the boxcox transform and check prediction on true values.
par(mfrow=c(1, 1))
num_to_predict <- 6
mypred <- predict(fit1, n.ahead=num_to_predict)
plot(c(orig_aapl),
     col = c("black"),
     xlab = "Index",
     ylab = "Price",
     xlim=c(length(train_aapl) - 2*num_to_predict, length(train_aapl) + 2*num_to_predict),
     ylim=c(min(((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda)), na.rm = TRUE)*0.99, max(((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda)), na.rm = TRUE)*1.01),
     pch = 0, lty=1)
points((length(train_aapl) + 1):(length(test_aapl) + length(train_aapl)),
       test_aapl,
       col="blue",
       pch = 0)
points((length(train_aapl) + 1):(length(train_aapl) + num_to_predict),
       ((lambda*mypred$pred + 1)^(1/lambda)),
       col="red",
       pch = 1)
lines((length(train_aapl) + 1):(length(train_aapl) + num_to_predict),
      ((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red4"))
lines((length(train_aapl) + 1):(length(train_aapl) + num_to_predict),
      ((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red4"))
lines((length(train_aapl) + 1):(length(train_aapl) + num_to_predict),
      ((lambda*(mypred$pred + 1.645*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red"))
lines((length(train_aapl) + 1):(length(train_aapl) + num_to_predict),
      ((lambda*(mypred$pred - 1.645*mypred$se) + 1)^(1/lambda)),
      lty=2, col = c("red"))

title(main = "Predicting AAPL Price with SARIMA(2,1,2)x(1,1,1)[10]")
legend("topleft",
       legend = c("Train Data", "Test Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "blue", "red", "red", "red4"),
       lty = c(-1, -1, -1, 2, 2),
       pch = c(0, 0, 1, -1, -1))

# ------------------------------------------------------ #
# Predicting out-of-sample aapl prices.
# Retrain the model on the full data.
fit <- arima(orig_aapl_boxcox, order=c(result$p[1], 1, result$q[1]),
             seasonal=list(order=c(result$P[1], 1, result$Q[1]), period=seaonal_term),
             method="ML")

mypred <- predict(fit, n.ahead=num_to_predict)
pred_ts <- ts(((lambda*mypred$pred + 1)^(1/lambda)),
              start = c(end(orig_aapl)[1],
                        end(orig_aapl)[2] + 1),
              
              frequency = 12)
upper_pred_ts_95 <- ts(((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_aapl)[1],
                                 end(orig_aapl)[2] + 1), 
                       frequency = 12)
lower_pred_ts_95 <- ts(((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_aapl)[1],
                                 end(orig_aapl)[2] + 1),
                       frequency = 12)
upper_pred_ts_90 <- ts(((lambda*(mypred$pred - 1.645*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_aapl)[1],
                                 end(orig_aapl)[2] + 1), 
                       frequency = 12)
lower_pred_ts_90 <- ts(((lambda*(mypred$pred + 1.645*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_aapl)[1],
                                 end(orig_aapl)[2] + 1),
                       frequency = 12)
ts.plot(orig_aapl,
        pred_ts,
        upper_pred_ts_95,
        lower_pred_ts_95,
        upper_pred_ts_90,
        lower_pred_ts_90,
        col = c("black", "red", "red4", "red4", "red", "red"),
        ylim=c(min(orig_aapl)*0.8, max(orig_aapl)*1.2),
        xlab = "Index",
        ylab = "Price",
        lty = c(1, 1, 2, 2))

title(main = "Out-of-Sample AAPL Forecast Using SARIMA(2,1,2)x(1,1,1)[10]")
legend("topleft",
       legend = c("Actual Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "red", "red", "red4"),
       lty = c(1, 1, 2, 2))
# ------------------------------------------------------ #

# ------------------------TSLA------------------------ #
train_tsla_log <- log(train_tsla)
test_tsla_log <- log(test_tsla)
# boxcox transform.
t = 1:length(train_tsla)
boxcox_transform <- boxcox(train_tsla ~ t, plotit = FALSE)
lambda <- boxcox_transform$x[which(boxcox_transform$y == max(boxcox_transform$y))]
train_tsla_boxcox <- (1/lambda)*(train_tsla^lambda-1)
test_tsla_boxcox <- (1/lambda)*(test_tsla^lambda-1)
orig_tsla_boxcox <- (1/lambda)*(orig_tsla^lambda-1)

par(mfrow=c(1,2))
ts.plot(train_tsla_boxcox, 
        test_tsla_boxcox,
        ylab = "Boxcox(Price)",
        col = c("black", "red"), 
        main="Boxcox Transform of TSLA Daily Open Prices")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))
ts.plot(train_tsla_log, 
        test_tsla_log,
        ylab = "Log(Price)",
        col = c("black", "red"), 
        main="Log Transform of TSLA Daily Open Prices")
legend("topleft",
       legend = c("Train", "Test"),
       col = c("black", "red"),
       lty = c(1, 1))

# check acf and pacf.
par(mfrow=c(1,2))
acf(train_tsla_log,
    lag.max = 60)
title(main = "tsla Autocorrelations")
pacf(train_tsla_log,
     lag.max = 60)
title(main = "tsla Partial Autocorrelations")

# difference to remove seasonality.
seaonal_term <- 12
y10 <- diff(train_tsla_log, seaonal_term)
y10.2 <- diff(y10, seaonal_term)
var(y10); var(y10.2)

# difference to remove trend.
par(mfrow=c(1,1))
y1 <- diff(y10, 1)
var(y10); var(y1)
ts.plot(y1, main="De-trended and De-seaonalized tsla Open Prices")

t = 1:length(y1)
linear_fit <- lm(y1 ~ t)
abline(linear_fit, t, col = c("red"))
abline(h = mean(y1), col = c("blue"))
legend("topleft",
       legend = c("TSLA Data",
                  "Linear Fit of Residuals",
                  "Mean of Residuals"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 1))

# --------------------------------------------------------------------------------------------------------- #

# 5) Estimate parameters.
# check acf and pacf of de-trended de-seasonalized series.
par(mfrow=c(1,2))
acf(y1, lag.max = 100, na.action = na.pass)
title(main = "De-trended TSLA Autocorrelations")
pacf(y1, lag.max = 100, na.action = na.pass)
title(main = "De-trended TSLA Partial Autocorrelations")

# Identifying possible models.
df <- expand.grid(p=c(0,15), q=c(0), P=c(0,1), Q=c(0,1))
df <- cbind(df, AICc=NA)
df

# Compute AICc.
for (i in 1:nrow(df)) {
  sarima.obj <- NULL
  try(arima.obj <- arima(train_tsla_log, order=c(df$p[i], 1, df$q[i]),
                         seasonal=list(order=c(df$P[i], 1, df$Q[i]), period=seaonal_term),
                         method="ML"))
  if (!is.null(arima.obj)) { df$AICc[i] <- AICc(arima.obj) }
  print(df[i, ])
}
# sort the dataframe by AICc in the increasing order.
df <- df[order(df$AICc),]
# we will work on the 3 models with the lowest AICc.
result <- df[1:3,]
result

# Top 3 candidate models.
fit1 <- arima(train_tsla_log, order=c(result$p[1], 1, result$q[1]),
              seasonal=list(order=c(result$P[1], 1, result$Q[1]), period=seaonal_term),
              method="ML")
fit2 <- arima(train_tsla_log, order=c(result$p[2], 1, result$q[2]),
              seasonal=list(order=c(result$P[2], 1, result$Q[2]), period=seaonal_term),
              method="ML")
fit3 <- arima(train_tsla_log, order=c(result$p[3], 1, result$q[3]),
              seasonal=list(order=c(result$P[3], 1, result$Q[3]), period=seaonal_term),
              method="ML")

# --------------------------------------------------------------------------------------------------------- #

# 6) Model Diagnostics on the top 3 models.
res <- residuals(fit2)
mean(res); var(res)

# Plot residuals.
par(mfrow=c(1, 1))
ts.plot(res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

shapiro.test(res)

modified_res <- res
modified_res[which(modified_res <= min(modified_res) + 0.2)] = 0
modified_res[which(modified_res >= max(modified_res)- 0.2)] = 0

par(mfrow=c(1, 1))
ts.plot(modified_res, main = "Fitted Residuals SARIMA(3,1,0)(4,1,1)[10]")
abline(h = mean(modified_res), col = c("blue"))
legend("topleft",
       legend = c("Residuals",
                  "Mean of Residuals"),
       col = c("black", "blue"),
       lty = c(1, 1))

# shapiro-wilk normality test.
shapiro.test(modified_res)

# histogram and qq plot of residuals.
par(mfrow=c(1, 2))
hist(modified_res,main = "Histogram", breaks=50)
qqnorm(modified_res)
qqline(modified_res,col ="blue")

acf(res, lag.max = 100)
title(main = "ACF of Residuals")
pacf(res, lag.max = 100)
title(main = "Partial ACF of Residuals")

# testing residuals for independence.
Box.test(res, lag = floor(sqrt(length(res))), type = c("Box-Pierce"), fitdf = 0)
Box.test(res, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 0)
Box.test(res^2, lag = floor(sqrt(length(res))), type = c("Ljung-Box"), fitdf = 0)

# --------------------------------------------------------------------------------------------------------- #

# 6) Forecasting tsla Prices.
# We want to remember to reverse the log transform and check prediction on true values.
par(mfrow=c(1, 1))
num_to_predict <- 12
mypred <- predict(fit2, n.ahead=num_to_predict)
plot(c(orig_tsla),
     col = c("black"),
     xlab = "Index",
     ylab = "Price",

     pch = 0, lty=1)
points((length(train_tsla) + 1):(length(test_tsla) + length(train_tsla)),
       test_tsla,
       col="blue",
       pch = 0)
points((length(train_tsla) + 1):(length(train_tsla) + num_to_predict),
       exp(mypred$pred),
       col="red",
       pch = 1)
lines((length(train_tsla) + 1):(length(train_tsla) + num_to_predict),
      exp(mypred$pred + 1.96*mypred$se),
      lty=2, col = c("red4"))
lines((length(train_tsla) + 1):(length(train_tsla) + num_to_predict),
      exp(mypred$pred - 1.96*mypred$se),
      lty=2, col = c("red4"))
lines((length(train_tsla) + 1):(length(train_tsla) + num_to_predict),
      exp(mypred$pred + 1.645*mypred$se),
      lty=2, col = c("red"))
lines((length(train_tsla) + 1):(length(train_tsla) + num_to_predict),
      exp(mypred$pred - 1.645*mypred$se),
      lty=2, col = c("red"))

title(main = "Predicting TSLA Price with SARIMA(0,1,0)x(1,1,1)[10]")
legend("topleft",
       legend = c("Train Data", "Test Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "blue", "red", "red", "red4"),
       lty = c(-1, -1, -1, 2, 2),
       pch = c(0, 0, 1, -1, -1))

# ------------------------------------------------------ #
# Predicting out-of-sample tsla prices.
# Retrain the model on the full data.
fit <- arima(orig_tsla_log, order=c(result$p[1], 1, result$q[1]),
             seasonal=list(order=c(result$P[1], 1, result$Q[1]), period=seaonal_term),
             method="ML")

mypred <- predict(fit, n.ahead=num_to_predict)
pred_ts <- ts(((lambda*mypred$pred + 1)^(1/lambda)),
              start = c(end(orig_tsla)[1],
                        end(orig_tsla)[2] + 1),
              
              frequency = 12)
upper_pred_ts_95 <- ts(((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_tsla)[1],
                                 end(orig_tsla)[2] + 1), 
                       frequency = 12)
lower_pred_ts_95 <- ts(((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_tsla)[1],
                                 end(orig_tsla)[2] + 1),
                       frequency = 12)
upper_pred_ts_90 <- ts(((lambda*(mypred$pred - 1.645*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_tsla)[1],
                                 end(orig_tsla)[2] + 1), 
                       frequency = 12)
lower_pred_ts_90 <- ts(((lambda*(mypred$pred + 1.645*mypred$se) + 1)^(1/lambda)),
                       start = c(end(orig_tsla)[1],
                                 end(orig_tsla)[2] + 1),
                       frequency = 12)
ts.plot(orig_tsla,
        pred_ts,
        upper_pred_ts_95,
        lower_pred_ts_95,
        upper_pred_ts_90,
        lower_pred_ts_90,
        col = c("black", "red", "red4", "red4", "red", "red"),
        ylim=c(min(orig_tsla)*0.8, max(orig_tsla)*1.2),
        xlab = "Index",
        ylab = "Price",
        lty = c(1, 1, 2, 2))

title(main = "Out-of-Sample TSLA Forecast Using SARIMA(2,1,2)x(1,1,1)[10]")
legend("topleft",
       legend = c("Actual Data", "Prediction", "90% CI", "95% CI"),
       col = c("black", "red", "red", "red4"),
       lty = c(1, 1, 2, 2))
# ------------------------------------------------------ #