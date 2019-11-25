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