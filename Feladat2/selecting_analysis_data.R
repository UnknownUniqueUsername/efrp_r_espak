library(dplyr)
###selecting_analysis_data###


selecting_analysis_data <- function(raw,parameters){
  
  #'raw' argument contains the raw data
  #'parameters' argument must be a list containing the asset list, starting date and end date in this order, as defined above
  
  assetlist = parameters[[1]]
  startdate=parameters[[2]]
  enddate=parameters[[3]]
  
  assetlist=c('date',assetlist)
  #selecting the data:  
  analysis_data<- raw %>%
    filter(as.Date(startdate) <= as.Date(date) & as.Date(date) <= as.Date(enddate) ) %>%
    select(assetlist) 
  
  
  return(analysis_data)
}