###################################
# Data: Covid-19
# Time Series Analysis on Covid-19 Confirmed cases
#####################################

# Clear all variables in workspace
rm(list = ls())

# Setting the working envoirnment
setwd('D:\\IIIT\\Intro to timeseries\\home_work5\\covid-19')

# loading necessary libraries
library(readr)
#library('ggplot2')
library('forecast')
#library('tseries')
library(dplyr)

# loading the data
df.covid <- read_csv('data/covid_19_data.csv')

# Viewing the data
head(df.covid)
str(df.covid)

##########################
# Data Wrangling
#########################

#Converting the 'ObservationDate' column to Date Type Column for  latter analysis
df.covid$ObservationDate <- as.Date(df.covid$ObservationDate,format = '%m/%d/%Y')

#Creating total cases per month column
df.total.cases <- df.covid %>%
  group_by(ObservationDate) %>%
  summarise( Total.Cases= sum(Confirmed))

# Converting the metric into thousands
df.total.cases$Total.Cases <- df.total.cases$Total.Cases / 1000

# Creating a time series object
df.cases <- ts(df.total.cases[,2],
               start = c(2020,as.numeric(format(df.total.cases$ObservationDate[1], "%j"))),
               frequency = 365)
  
############################
# Preliminary analysis
###########################
autoplot(df.cases) +
  ggtitle('Time Plot: Total Confirmed Cases in USA From JAN 2020') +
  ylab('Confirmed Cases in ( K )')

ggplot(data = df.total.cases,aes(x=ObservationDate,y=Total.Cases)) +
  geom_line() +
  ggtitle('Time Plot: Total Confirmed Cases in USA From JAN 2020') +
  scale_x_date('Days') +
  ylab('Total cases in (K)')


##############################################
# ACF and PACF 
###############################################

Acf(df.cases)
Pacf(df.cases)


