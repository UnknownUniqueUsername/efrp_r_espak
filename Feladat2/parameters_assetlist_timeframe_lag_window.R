###parameters_assetlist_timeframe_lag_window###


parameters_assetlist_timeframe_lag_window <-function(asset = c('cl1','cl2'), 
                                                     start="2010-01-01",end="2016-12-31",lag=1,window=c(500,1000)){
  
  asset_list<-asset #giving the list of assets we would like to run the analysis with
  
  start_date <- start #starting date of the analysis
  end_date <- end #ending date of the analysis
  
  timespan<-(as.Date(end)-as.Date(start))
  
  lag<-lag
  windowsize<-window
  
  for (i in 1:length(windowsize)){
    if (windowsize[i]>timespan) stop(paste0("Bigger windowsize (", windowsize[i], ") than timespan (", timespan ,")"))
  }
  
  
  return(list(asset_list,start_date,end_date,lag,windowsize))
}