setwd("/Volumes/MICROSD")

library(jsonlite)
library(MASS)
library(forecast)
library(astsa)
library(greybox)

ticker = "aapl"
timeframe = "daily"
path = paste("data/", timeframe, "/", ticker, ".json", sep="")
price_data <- subset(fromJSON(path, flatten=TRUE), select = -c(high, low, close, volume))

partial <- data.frame(
  date = as.Date(price_data$datetime),
  price = price_data$open
)

# fill in missing weekend dates for consistency.
start = as.Date("2002/06/10")
end = as.Date("2022/11/30")
full <- seq(start, by='7 day', length=(end-start)/7)

cleaned_data <- with(partial, price[match(full, date)])
# fill in NA values manually.
for (i in 1:length(cleaned_data)) {
  if (is.na(cleaned_data[i])) {
    cleaned_data[i] = cleaned_data[i-1]
  }
}

par(mfrow=c(1,2))
orig <- ts(cleaned_data, start = c(2002, 06, 10), frequency = 52)
ts.plot(orig, main="AAPL Daily Open Prices 2002-2022")

# Boxcox Transform of the Data.
t = 1:length(orig)
boxcox_transform <- boxcox(orig ~ t, plotit = FALSE)
lambda <- boxcox_transform$x[which(boxcox_transform$y == max(boxcox_transform$y))]
orig_boxcox <- (1/lambda)*(orig^lambda-1)
ts.plot(orig_boxcox, main="Boxcox Transform of AAPL Daily Open Prices")

orig_boxcox_comp <- decompose(orig_boxcox)
plot(orig_boxcox_comp)

y1 <- diff(orig_boxcox, 1)
hist(y1, breaks=50)
mean(y1, trim = 0, na.rm = TRUE)
ts.plot(y1, main="De-Trended AAPL Open Prices")

t = 1:length(y1)
linear_fit <- lm(y1 ~ t)
abline(linear_fit, t, col = c("red"))
abline(mean(linear_fit$residuals), t, col = c("blue"))

acf(y1, lag.max = 40, na.action = na.pass)
pacf(y1, lag.max = 40, na.action = na.pass)

y12 <- diff(y1, 12)
ts.plot(y12, main="De-Trended AAPL Open Prices")
hist(y12, breaks=50)
mean(y12, trim = 0, na.rm = TRUE)

acf(y12, lag.max = 72)
pacf(y12, lag.max = 72)

# ------------------------------------------------------ #
# Possible models.
df <- expand.grid(p=c(0,3), q=c(0,3), P=c(0,4), Q=1)
df <- cbind(df, AICc=NA)

# split data into train and test.
years_to_train <- 19
train <- ts(orig_boxcox[1:(52*years_to_train)], start = c(2002, 06, 10), frequency = 52)
test <- ts(orig_boxcox[(52*years_to_train + 1):length(orig_boxcox)], start = c(2017, 07, 10), frequency = 52)

# Compute AICc.
for (i in 1:nrow(df)) {
  sarima.obj <- NULL
  try(arima.obj <- arima(train, order=c(df$p[i], 1, df$q[i]),
                         seasonal=list(order=c(df$P[i], 1, df$Q[i]), period=12),
                         method="ML"))
  if (!is.null(arima.obj)) { df$AICc[i] <- AICc(arima.obj) }
  # print(df[i, ])
}
df[which.min(df$AICc), ]

# Final model identified by lowest AICc
ind <- which.min(df$AICc)
fit <- arima(train, order=c(df$p[ind], 0, df$q[ind]),
             seasonal=list(order=c(df$P[ind], 1, df$Q[ind]), period=12),
             method="ML")

# ------------------------------------------------------ #

# Residual plots:
res <- residuals(fit)
mean(res); var(res)

par(mfrow=c(1, 1))
ts.plot(res, main="Fitted Residuals")
t <- 1:length(res)
fit.res = lm(res~ t)
abline(fit.res)
abline(h = mean(res), col = "red")

par(mfrow=c(1, 2))
acf(res, main = "ACF of Residuals")
pacf(res, main = "Partial ACF of Residuals")

# white noise assumption for residuals.
shapiro.test(res)

# histogram and qq plot of residuals.
hist(res,main = "Histogram", breaks=50)
qqnorm(res)
qqline(res,col ="blue")

# Predict 20 future observations and compare to our test data.
num_to_predict <- 20
par(mfrow=c(1, 1))
mypred <- predict(fit, n.ahead=num_to_predict)
plot(c(train),
     col = c("black"),
     xlim=c(length(train) - num_to_predict, length(train) + num_to_predict),
     ylim=c(min(orig_boxcox[(length(train) - num_to_predict):(length(train) + num_to_predict)])*0.8, max(orig_boxcox[(length(train) - num_to_predict):(length(train) + num_to_predict)])*1.2),
     xlab = "Index",
     ylab = "Price",
     pch = 0, lty=1)
points((length(train) + 1):(length(orig_boxcox)),
       test,
       col="blue",
       pch = 0)
points((length(train) + 1):(length(train) + num_to_predict),
       mypred$pred,
       col="red",
       pch = 0)
lines((length(train) + 1):(length(train) + num_to_predict),
      mypred$pred + 1.96*mypred$se,
      lty=2, col = c("red"))
lines((length(train) + 1):(length(train) + num_to_predict),
      mypred$pred - 1.96*mypred$se,
      lty=2, col = c("red"))

title(main = "Predicting AAPL Price with SARIMA(0,1,0)x(0,1,1)[12]")
legend("topleft",
       legend = c("Train Data", "Test Data", "Prediction", "95% CI"),
       col = c("black", "blue", "red", "red"),
       lty = c(-1, -1, -1, 2),
       pch = c(0, 0, 0, -1))

# ------------------------------------------------------ #
# Predicting out-of-sample values.
# Redo the fit on the full data.
fit <- arima(orig_boxcox, order=c(df$p[ind], 0, df$q[ind]),
             seasonal=list(order=c(df$P[ind], 1, df$Q[ind]), period=12),
             method="ML")

num_to_predict <- 20
par(mfrow=c(1, 1))
mypred <- predict(fit, n.ahead=num_to_predict)
plot(c(orig),
     col = c("black"),
     xlim=c(1, length(orig) + num_to_predict),
     ylim=c(min(orig)*0.8, max(orig)*1.2),
     xlab = "Index",
     ylab = "Price",
     pch = 1)
points((length(orig) + 1):(length(orig) + num_to_predict),
       (lambda*mypred$pred + 1)^(1/lambda),
       col="red",
       pch = 1)
lines((length(orig) + 1):(length(orig) + num_to_predict),
      (lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda),
      lty=2, col = c("red"))
lines((length(orig) + 1):(length(orig) + num_to_predict),
      (lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda),
      lty=2, col = c("red"))

title(main = "Predicting AAPL Price with SARIMA(0,1,0)x(0,1,1)[12]")
legend("topleft",
       legend = c("Actual Data", "Prediction", "95% CI"),
       col = c("black", "red", "red"),
       lty = c(-1, -1, 2),
       pch = c(1, 1, -1))
# ------------------------------------------------------ #
# We want to change the circles to lines to make the chart cleaner.
fit <- arima(orig_boxcox, order=c(df$p[ind], 0, df$q[ind]),
             seasonal=list(order=c(df$P[ind], 1, df$Q[ind]), period=12),
             method="ML")

num_to_predict <- 20
par(mfrow=c(1, 1))
mypred <- predict(fit, n.ahead=num_to_predict)
pred_ts <- ts((lambda*mypred$pred + 1)^(1/lambda),
                    start = c(2022, 35, 10), 
                    frequency = 52)
upper_pred_ts <- ts((lambda*(mypred$pred + 1.96*mypred$se) + 1)^(1/lambda),
              start = c(2022, 35, 10), 
              frequency = 52)
lower_pred_ts <- ts((lambda*(mypred$pred - 1.96*mypred$se) + 1)^(1/lambda),
                    start = c(2022, 35, 10), 
                    frequency = 52)
ts.plot(orig, pred_ts, upper_pred_ts, lower_pred_ts,
     col = c("black", "red", "red", "red"),
     ylim=c(min(orig)*0.8, max(orig)*1.2),
     xlab = "Index",
     ylab = "Price",
     lty = c(1, 1, 2, 2))

title(main = "Predicting AAPL Price with SARIMA(0,1,0)x(0,1,1)[12]")
legend("topleft",
       legend = c("Actual Data", "Prediction", "95% CI"),
       col = c("black", "red", "red"),
       lty = c(1, 1, 2))

# ------------------------------------------------------ #