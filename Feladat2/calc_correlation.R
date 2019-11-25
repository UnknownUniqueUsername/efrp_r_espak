#This is where we calculate all the correlations
calc_correlation <- function(rawdata,parameters){
  
  results <- NULL
  startdate = parameters[[2]]
  enddate = parameters[[3]]
  
  for (k in 2:(ncol(rawdata))) {
    for (j in (k):(ncol(rawdata))) {
      # Creating the column names so it can be filtered by the selecting_analysis_data func
      parameters[[1]] = c(paste('cl', k-1, sep=""), paste('cl', j-1, sep=""))
      # We get back the filtered data
      
      analysis_data1<- rawdata %>%
        filter(as.Date(startdate) <= as.Date(date) & as.Date(date) <= as.Date(enddate) )%>%
        select(date, parameters[[1]][1])
      analysis_data2<- rawdata %>%
        filter(as.Date(startdate) <= as.Date(date) & as.Date(date) <= as.Date(enddate) )%>%
        select(parameters[[1]][2])
      analysis_data <- cbind(analysis_data1, analysis_data2)
      
      
      # This is where it should bind all the data together
      results = cbind(results, dinamic_cross_correlation(analysis_data, parameters)[[1]][,2])
    }
  }
  return(results)
}