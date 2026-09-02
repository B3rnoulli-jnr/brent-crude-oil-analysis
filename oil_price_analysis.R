#=================================================
# Phase 3 Project: Brent Crude Oil Price Analysis
#=================================================
# Load Library
library(quantmod)
library(forecast)
library(xts)

# Import data
getSymbols("BZ=F", src="yahoo")
oil_price <- Cl(`BZ=F`)
oil_price_clean <- na.omit(oil_price)
oil_ts <- ts(as.numeric(oil_price_clean))

plot(oil_ts, main="Brent Crude Oil - I am back!", col="steelblue", lwd=1.5)

# Explore data
cat("Number of observations:", length(oil_ts), "\n")
cat("Maximum price:", max(oil_ts), "\n")
cat("Minimum price:", min(oil_ts), "\n")
cat("Current price:", tail(oil_ts), "\n")

# Visualization
plot(oil_ts, main="Brent Crude Oil Price (Full History)",
     col="steelblue", lwd=1.5, xlab="Time", ylab="Price (USD)")

# Check ACF
acf(oil_ts, main="Autocorrelation - Oil Price")

#From the acf plot, the x-axis shows how many days back you are looking
# the y-axis shows the strength of the relationship,
#1.0 meaning perfect relationship and 0 meaning no relationship
# the blue dotted line is the boundary, any bar that crosses outside is 
# statistically significant.
# the plot is non-stationary.

# Fit Arima model
oil_arima <- auto.arima(oil_ts)
summary(oil_arima)

# Forecast for 30 days
oil_forecast <- forecast(oil_arima, h=30)

plot(oil_forecast, xlim=c(length(oil_ts)-180, length(oil_ts)+ 30),
     main = "30-Day Brent Crude oil forecast",
     xlab="Time", ylab="Price(USD)")