###################################
# Data: retail sales per month
# We will use this data to forecast 
# next two years
#####################################

# Clear all variables in workspace
rm(list = ls())

# Setting the working envoirnment
setwd('D:\\github\\timeseries_youtube\\timeSeriesForecasting\\retail.Industry.USA')

# loading necessary libraries
library(readr)
#install.packages('fpp2')
library(fpp2)

# Loading the data
df.retails <- read_csv('data/RSXFS.csv',col_names = c('Dates','Sales.Per.Month'),skip = 1)

# Declare this as time series data  - ***
sales <- ts(df.retails[,2],start = c(1992,1),frequency = 12)

########################################
# Preliminary Analysis
########################################

# Time plot
autoplot(sales) +ggtitle('Time Plot: Real US  Retail Sales per Day') +
  ylab('Miilion of dollars')

# Data has a strong trend. investigate transformations.

# Take the first difference of the data to remove the trend
d.sales <- diff(sales)

# Time plot
autoplot(d.sales) +ggtitle('Time Plot: Change in Real US  Retail Sales per Day') +
  ylab('Miilion of dollars')

# Series appears trend-stationary, used to investigate seasonality
ggseasonplot(d.sales) +
  ggtitle('Seasonal Plot: Change in Retail Sales') +
  ylab('Millions of sales in dollars')

# let's look another seasonal plot, the subseries plot
ggsubseriesplot(d.sales)

##############################################
# our series , Y, has trend and seasonality
# to remove trend , we take the first difference 
# the first difference series still has seasonlity
#
# Forecase with various methods
##############################################


################
# Use a benchmark method to forecast
# let's use easonal naive method  as our benchamrk
################