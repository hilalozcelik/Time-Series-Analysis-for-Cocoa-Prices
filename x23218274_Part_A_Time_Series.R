################################################################################    
#                    Required Libraries and Functions                          #
################################################################################ 
getwd()
setwd("C:/Users/Hilal/Desktop/NCI classes notes/Statistic for Data Analysis/TABA_Final_Project/")


install.packages("TTR")
install.packages("ggplot2")
install.packages('forecast')
install.packages('aTSA')
library(TTR)
library(ggplot2)
library(forecast)
library(aTSA)


################################################################################       
#                                  Loading Data                                #
################################################################################  
cocoa <- read.csv("CocoaPrice.csv")
head(cocoa)




################################################################################      
#                          Cocoa Prices Time Series                            #
################################################################################ 
tcocoa <- ts(c(cocoa$Price), start=c(1994,10), frequency=12)
tcocoa

plot(tcocoa,col="blue",lwd=3, main="Cocoa Prices Raw Plot")




##################      Smoothing
################## 
smoothed_tcocoa3 <- SMA(tcocoa, n = 3)
plot(smoothed_tcocoa3)

smoothed_tcocoa8 <- SMA(tcocoa, n = 8)
plot(smoothed_tcocoa8)



##################      Decomposition
################## 
decompose_tcocoa <- decompose(tcocoa, "additive")
plot(decompose_tcocoa)

decompose_tcocoa$seasonal   #Now that we decomposed, these components can also store seperately
decompose_tcocoa$trend
decompose_tcocoa$random



################################################################################  
#                         Simple Time Series Models                            #
################################################################################
train <- window(tcocoa, end=c(2023,9))
train

test <- window(tcocoa, start=c(2023,10))
test


##################  Training and Test Sets
################## 
plot(tcocoa, ylim=c(1000,7000), lwd=0, main='Training and Test Sets') 

x <- as.vector(time(train))
y <- train
lines(x, y, col='blue', lwd=3)
x <- as.vector(time(test))
y <- test
lines(x, y, col='red', lwd=3)



##################      Mean Model
################## 
forecast_mean <- meanf(train,h=6)
forecast_mean
round(accuracy(forecast_mean, test),2)

plot(forecast_mean, col='blue', lwd=3, ylim=c(1000,7000),
     main='Forecast using meanf')
x <- c(time(train)[1], time(train)[348])
y <- c(mean(train), mean(train))
lines(x, y, col='black', lwd=3)
x <- as.vector(time(test))
y <- test
lines(x, y, col='red', lwd=3)
abline(v=seq(1994, 2024, length.out=36), lwd=0.5)
abline(h=seq(1000,7000, length.out=7), lwd=0.5)



##################      Naive Model
################## 
forecast_naive <- naive(train, h=6)
forecast_naive
round(accuracy(forecast_naive, test), 2)

plot(forecast_naive, col='blue', lwd=3, ylim=c(1000, 7000))
x <- time(train)[length(train)]
y <- train[length(train)]
points(x, y, col='black', lwd=5)
x <- as.vector(time(test))
y <- test
lines(x, y, col='red', lwd=3)
abline(v=seq(1994, 2024, length.out=36), lwd=0.5)
abline(h=seq(1000,7000, length.out=7), lwd=0.5)



##################      Seasonal Naive Model
################## 
forecast_snaive <- snaive(train,h=6)
forecast_snaive
round(accuracy(forecast_snaive, test), 2)

plot(forecast_snaive, col='blue', lwd=3, ylim=c(1000, 7000))
x <- time(train)[length(train)]
y <- train[length(train)]
points(x, y, col='black', lwd=5)
x <- as.vector(time(test))
y <- test
lines(x, y, col='red', lwd=3)
abline(v=seq(1994, 2024, length.out=36), lwd=0.5)
abline(h=seq(1000,7000, length.out=7), lwd=0.5)



##################      Drift (Random Walk) Model
################## 
forecast_drift <- rwf(train, h=6, drift=TRUE)
forecast_drift
round(accuracy(forecast_drift, test),2)

plot(forecast_drift, col='blue', lwd=3, ylim=c(1000, 7000))
x <- c(time(train)[1], time(train)[length(train)])
y <- c(train[1], train[length(train)])
lines(x, y, col='black', lwd=5)
x <- as.vector(time(test))
y <- test
lines(x, y, col='red', lwd=3)
abline(v=seq(1994, 2024, length.out=36), lwd=0.5)
abline(h=seq(1000,7000, length.out=7), lwd=0.5)



################################################################################  
#                          Exponential Smoothing                               #
################################################################################


##################      Both Additive and Multiplicative 
################## 
hw1 <- hw(train, h = 6, seasonal = c("additive"))
hw1
round(accuracy(hw1, test),2)
checkresiduals(hw1)
autoplot(hw1)


hw2 <- hw(train, h = 6, seasonal = c("multiplicative"))
hw2
round(accuracy(hw2, test),2)
checkresiduals(hw2)
autoplot(hw2)


##################      Plotting the Holt-Winters Models
################## 
autoplot(tcocoa) +
  autolayer(hw1, series = "Additive") +
  autolayer(hw2, series = "Multiplicative") +
  labs(x = "Year", y = "Cocoa Prices", title = "Holt-Winters Forecasts") +
  guides(colour = guide_legend(title = "Forecasts")) +
  theme_bw()


##################      Autofit Model
################## 
autofit <- ets(train, model = "ZZZ")
summary(autofit)
checkresiduals(autofit)
accuracy(autofit)
autoplot(autofit)



################################################################################
#                                  ARIMA                                       #
################################################################################


##################      Plotting time series
##################  
plot(tcocoa,col="blue",lwd=3, main="Cocoa Prices Raw Plot")


##################      Stationary
##################  
ndiffs(tcocoa)   # two difference needs to be done

tcocoadiff1 <- diff(tcocoa, differences=1)  #First difference
plot(tcocoadiff1)

tcocoadiff2 <- diff(tcocoa, differences=2)  #Second difference
plot(tcocoadiff2)

par(mfrow=c(2,1))     # comparing two time difference
plot(tcocoa)
plot(tcocoadiff2)




##################      acf and pacf
##################  
plot.acf.pacf <- function(ts)
{ layout(mat = matrix(c(1,1,2,3),
                      nrow=2, ncol=2, byrow=TRUE))
  plot(ts)
  Acf(ts, main='ACF')
  Pacf(ts, main='PACF')
}

plot.acf.pacf(tcocoadiff2)



##################      Building ARIMA Model
################## 
arima_cocoa <- arima(tcocoa, order=c(2,2,1))
arima_cocoa


##################      Checking Residuals
################## 
# Q-Q Plot
qqnorm(arima_cocoa$residuals)
qqline(arima_cocoa$residuals)


# Residuals Plot
plot(arima_cocoa$residuals, type='p', main='Residual Plot')
rlm <- lm(arima_cocoa$residuals ~ time(tcocoa))
x <- as.vector(time(tcocoa))
y <- rlm$fitted
lines(x, y, col='red')


# Actual Cocoa Prices vs Predicted Cocoa Prices
plot(tcocoa, main="Actual Cocoa Prices vs Predicted Cocoa Prices with Arima(2,2,1)")
x <- as.vector(time(tcocoa))
y <- fitted(arima_cocoa)
lines(x, y, col='green', lw=1)


# Test RMSE
round(accuracy(fitted(arima_cocoa), test),2)


# Forecasting with the Fitted Model
tail(tcocoa)                        
tail(round(fitted(arima_cocoa),2))


