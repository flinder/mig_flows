##########################
##Date: 3.22.2015
##Author: Joshua Snoke
##Purpose: Function to select flows out of the smoothed flows.
##Details: Implements Alogrithms 1. Flow Selection from Dr. Guo's paper "Origin Destination Flow Smoothing
##        and Mapping"
##########################

## main_dat and county_info are given from previous functions. 
## numFlows and distThresh are chosen based on desired map output.
flowSelection = function(main_dat,county_info,numFlows,distThresh){
  Ts = matrix(NA,nrow=numFlows,ncol=3)
  Tf = main_dat[order(main_dat$size),]  ##Order by largest smoothed flow
  Ts[1,] = Tf[1,]
  k = 1
  for(i in 2:nrow(Tf)){
    select = TRUE
    for(j in 1:k){
      origLongLat = matrix(c(county_info$lon[county_info$id==Tf$orig[i]],county_info$lat[county_info$id==Tf$orig[i]],
                             county_info$lon[county_info$id==Ts$orig[j]],county_info$lat[county_info$id==Ts$orig[j]]),
                           nrow=2,ncol=2,byrow=TRUE)
      destLongLat = matrix(c(county_info$lon[county_info$id==Tf$dest[i]],county_info$lat[county_info$id==Tf$dest[i]],
                             county_info$lon[county_info$id==Ts$dest[j]],county_info$lat[county_info$id==Ts$dest[j]]),
                           nrow=2,ncol=2,byrow=TRUE)
      origBand = (county_info$bandwidth[county_info$id==Ts$orig[j]]+county_info$bandwidth[county_info$id==Tf$orig[i]])
      destBand = (county_info$bandwidth[county_info$id==Ts$dest[j]]+county_info$bandwidth[county_info$id==Tf$dest[i]])      
      if(dist(origLongLat,method="euclidean") < origBand
         || dist(destLongLat,method="euclidean") < destBand
         || dist(origLongLat,method="euclidean") < distThresh
         || dist(destLongLat,method="euclidean") < distThresh
         )
        select = FALSE
    }
    if(select == TRUE && k < numFlows){
      Ts[k+1,] = Tf[i,]
      k = k+1
    }
  }
  return(Ts)
}







