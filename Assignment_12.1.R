#Assignment_12.1

#Import the data set

data <- read.csv(file.choose())
data
head(data)

data$Date <- as.Date(data$Date)

df <- ts(data$Close)
test = df[62:73]
df = df[1:61]

plot(df, mmain = "Daily Close Price")

df = ts(data$Close, frequency = 365)
plot(df, main = "Daily Close Price")

decompose(df)
decompose(df, type = "multi")

# creating seasonal forecast
library(forecast)
par(mfrow=c(1,1))
seasonplot(data)


# lags
lag(df,10)
lag.plot(df)

# Partial auto correlation
pac <- pacf(df)
pac$acf


# Auto correlation
ac <- acf(df)
ac$acf

# looking at ACF and PACF graph it is clear that the time series is not stationary
#------------------------------------------
model <- lm(df ~ c(1:length(df)))
summary(model)


plot(resid(model), type = 'l')
accuracy(model)

# deseasonlise the time series

tbl <- stl(df, 'periodic')
stab <- seasadj(tbl)
seasonplot(stab, 12)

# unit root for stationarity
# The Augmented Dicky Fuller Test for  
library(tseries)
adf.test(df)
# P value is greater than 0.05 , hence we fail to reject the null hypo
# there is unit root in time series hence the time series is not stationary

#----------------------------------------------

# Automatic ARIMA Model 
model2 <- auto.arima(df)
model2
plot(forecast(model2, h=12))
accuracy(model2)
#----------------------------------------------



# running model on diff data
# difference method to smoothen the data with lag = 5
adf.test(diff(df, lag = 5))
plot(diff(df, lag = 5))

model3 <- auto.arima(diff(df, lag = 5))
accuracy(model3)

acf(diff(df, lag = 5))
pacf(diff(df, lag = 5))


#-------------------------------------------------

# taking random order
model4 <- Arima(diff(df, lag = 5), order = c(4,0,5))
model4
accuracy(model4)
plot(forecast(model4, h=12))
#---------------------------------------------------

# taking random order
model5 <- Arima(diff(df, lag = 5), order = c(4,0,4))
model5
accuracy(model5)
plot(forecast(model5, h=12))

#---------------------------------------------------

# taking random order
model6 <- Arima(diff(df, lag = 5), order = c(3,0,5))
model6
accuracy(model6)
plot(forecast(model6, h=12))


#---------------------------------------------------

# taking random order
model7 <- Arima(diff(df, lag = 5), order = c(0,0,1))
model7
accuracy(model7)
plot(forecast(model7, h=12))

#---------------------------------------------------

# taking random order
model8 <- Arima(diff(df, lag = 5), order = c(1,0,0))
model8
accuracy(model8)
plot(forecast(model8, h=12))

#---------------------------------------------------

# Holt Winters Exponential Smoothing Model
model9 <- HoltWinters(df, gamma = F)
summary(model9)
plot(forecast(model9, h=12))
accuracy(forecast(model9, h=12))

#-----------------------------------------------------

# ETS
model10 <- ets(df)
summary(model10)
plot(forecast(model10, h=12))
accuracy(forecast(model10, h=12))

#---------------------------------------------------------------
#  model2 ( Automatic ARIMA) is most accurate with MAPE 1.15
#---------------------------------------------------------------

# Making predictions for next 15 days
predicted <- forecast(model2, 15)

# comparing data with forecast
predicted$residuals[62:73]

#-------------------------------------------------------------------
