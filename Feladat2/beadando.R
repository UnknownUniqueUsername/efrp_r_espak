# just some libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)

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
everything_1<-calc_correlation(rawdata,parameter_1)


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



dinamic_cross_correlation <-function(analysis_data,parameters){
  #first argument contains the data selected based on the given data parameters (time frame and assetlist)
  #second argument contains the analysis parameters: lag and window size in this order as defined above
  lag=parameters[[4]]
  window=parameters[[5]]
  
  output<- list()
  
  for (i in 1:length(window)){
    
    correl<-c(1:(nrow(analysis_data)-window[i]-lag+1))
    
    
    for (t in 1:(nrow(analysis_data)-window[i]-lag+1)){
      correl[t]<-cor(analysis_data[t:(t+window[i]-1),2],analysis_data[(t+lag[1]):(t+lag+
                                                                                    window[i]-1),3])
    }
    analysis_dates<-analysis_data[(window[i]+lag):nrow(analysis_data),1]
    
    
    result<-data.frame(x=analysis_dates, y=correl)
    
    output[[i]]=result #the output of this function is a list of data frames, each data frame belongs to a certain window size given in the parameters
    names(output)[i]=toString(window[i]) #each data frames are named after the windowsize they belong to
  }
  
  return(output)
}


#This is where we calculate all the correlations
calc_correlation <- function(rawdata,parameter){
  
  results <- NULL
  startdate = parameter[[2]]
  enddate = parameter[[3]]
  
  for (k in 2:(ncol(rawdata))) {
    for (j in (k):(ncol(rawdata))) {
      # Creating the column names so it can be filtered by the selecting_analysis_data func
      par[[1]] = c(paste('cl', k-1, sep=""), paste('cl', j-1, sep=""))
      # We get back the filtered data
      
      analysis_data1<- rawdata %>%
        filter(as.Date(startdate) <= as.Date(date) & as.Date(date) <= as.Date(enddate) )%>%
        select(date, par[[1]][1])
      analysis_data2<- rawdata %>%
        filter(as.Date(startdate) <= as.Date(date) & as.Date(date) <= as.Date(enddate) )%>%
        select(par[[1]][2])
      analysis_data <- cbind(analysis_data1, analysis_data2)
      
      
      # This is where it should bind all the data together
      results = cbind(results, dinamic_cross_correlation(analysis_data, parameter)[[1]][,2])
    }
  }
  return(results)
}