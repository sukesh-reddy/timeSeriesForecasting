#######################################
# Predicting the airlines ticket sales
# data ranges from 1949 - 1960
##########################################

# Clear all the variables in workingSpace
rm(list = ls())

# Setting the working envoirnment

# Loading the packages
library(forecast)
library(ggplot2)

# Loading the data
data("AirPassengers")

############### Data Exploration and preparation ##################

# Checking the class
class(AirPassengers)

# Looking at the data
head(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)

# Checking for missing Values
sum(is.na(AirPassengers))

# five - number Summary
summary(AirPassengers)


################## Preliminary Analysis / Components ####################

# Trend Component
autoplot(AirPassengers) + 
  ggtitle('Time Plot: Air Ticket Sales') +
  ylab('Sales')
# Data has strong trend

# Seasonal Component
ggseasonplot(AirPassengers)

ggsubseriesplot(AirPassengers)

boxplot(AirPassengers~cycle(AirPassengers))
# There is seasonal component

# decomposing the data
data <- decompose(AirPassengers,'multiplicative')

################# Modelling ###############

# ARIMA
mymodel <- auto.arima(AirPassengers)
mymodel
auto.arima(AirPassengers,trace = T)

# For testing
install.packages("tseries")
library(tseries)


