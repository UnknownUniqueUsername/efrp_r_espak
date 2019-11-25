# just some libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)

source("parameters_assetlist_timeframe_lag_window.R")
source("calc_correlation.R")
source("dinamic_cross_correlation.R")


rawdata <- read_excel("WTI2.xlsx") #reading the data from the given excel file

colnames(rawdata)<-c('date', 'cl1','cl2','cl3','cl4','cl5','cl6','cl7','cl8','cl9','cl10',
                     'cl11','cl12','cl13','cl14','cl15','cl16','cl17','cl18','cl19','cl20',
                     'cl21','cl22','cl23','cl24') #naming the columns for easier handling

parameter_1<-parameters_assetlist_timeframe_lag_window(c('cl1','cl3'),"2010-01-01","2016-12-31",5,c(100))#add the parameters
#parameter_2<-parameters_assetlist_timeframe_lag_window(c('cl1','cl3'),"2010-01-01","2016-12-31",10,c(100))
#parameter_3<-parameters_assetlist_timeframe_lag_window(c('cl1','cl3'),"2010-01-01","2016-12-31",100,c(100))

# Select the relevant dates and save it in relevant_dates
startdate = parameter_1[[2]]
enddate = parameter_1[[3]]
lag = parameter_1[[4]]

relevant_dates <- rawdata %>% 
  filter(as.Date(startdate) <= as.Date(date) & as.Date(date) <= as.Date(enddate) ) %>% select(date)

relevant_dates <- relevant_dates[(parameter_1[[5]]+lag):nrow(relevant_dates),1]

# This is the matrix that has every correlation pair
everything_1 <- calc_correlation(rawdata,parameter_1)

maxima_time_1 <- c(1:nrow(relevant_dates))
minima_time_1 <- c(1:nrow(relevant_dates))
mean_time_1 <- c(1:nrow(relevant_dates))


# calculating the min, max, and mean
for (f in 1:(nrow(everything_1))){
  maxima_time_1[f]<-max(everything_1[f,])
  minima_time_1[f]<-min(everything_1[f,])
  mean_time_1[f]<-mean(everything_1[f,])
}
#get every data in one variable
datas_1<-cbind(relevant_dates, maxima_time_1, minima_time_1, mean_time_1)

#plotting
ggplot(data = datas_1) + 
  geom_line(aes(x = date, y = maxima_time_1), color = 'blue') + 
  geom_line(aes(x = date, y = minima_time_1), color = 'red') + 
  geom_line(aes(x = date, y = mean_time_1), color = 'green') +
  labs(x = "Date", y = "Correlation", title = "Min, Mean, Max of correlations in time")
