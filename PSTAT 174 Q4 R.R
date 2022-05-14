library(xts)
library(forecast)

#rename columns
data <- multiTimeline
data$date <- data$`Category: All categories` 
data$views <- data$...2
keeps <- c("date", "views")
data = data[keeps]

view(data)

#remove missing values 
data <- na.omit(data)

# convert to ts
data$date <- as.Date(data$date)
ts <- xts(data$views, data$date)
ts
is.xts(ts)
head(ts)

# plot the data
plot(ts, main ="Relevance of Kim Kardashian") 

#log
ts_log <- log(ts)
plot(ts_log)
var(ts_log)
ts_log_diff1 <- diff(ts_log, lag=1)
# plot transformed and differenced time series
plot(ts_log_diff1)
# remove missing values 
ts_log_diff1 <- na.omit(ts_log_diff1)
# test p value to see if stationary 
adf.test(ts_log_diff1)
# acf and pacf plots 
acf(ts_log_diff1, lag.max=20)
pacf(ts_log_diff1, lag.max=20)

# arima modeling
auto.arima(ts_log)


