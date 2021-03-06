##########################
##Date: 3.22.2015
##Author: Joshua Snoke
##Purpose: Function to select flows out of the smoothed flows.
##Details: Implements Alogrithms 1. Flow Selection from Dr. Guo's paper "Origin Destination Flow Smoothing
##        and Mapping"
##########################
library(dplyr)
load("out_1/county_info.RData")
load("data/edges_2010.RData")
load("out_2/smoothed_flows.RData")
load("out_2/excl_flows.RData")
smoothed_flows <- smoothed_flows[out]
#smoothed_edges = data.matrix(cbind(edges[out, ], smoothed_flows))
smoothed_edges <- data.frame(edges[out, ], smoothed_flows, stringsAsFactors = FALSE)

## main_dat and county_info are given from previous functions. 
## numFlows and distThresh are chosen based on desired map output.
flowSelection = function(numFlows,distThresh){
  Tf = smoothed_edges[order(smoothed_edges[,4],decreasing=T),]  ##Order by largest smoothed flow
 
  Ts <- data_frame(orig_geocode = as.character(c(Tf[1, 1], rep(NA, (numFlows - 1)))),
                   dest_geocode = as.character(c(Tf[1, 2], rep(NA, (numFlows - 1)))),
                   filings = c(Tf[1, 3], rep(NA, (numFlows - 1))),
                   smoothed_flows = c(Tf[1, 4], rep(NA, (numFlows - 1))),
                   lon_orig = c(county_info$lon[county_info$GEOID==Tf[,1][1]], rep(NA, (numFlows - 1))),
                   lat_orig = c(county_info$lat[county_info$GEOID==Tf[,1][1]], rep(NA, (numFlows - 1))),
                   lon_dest = c(county_info$lon[county_info$GEOID==Tf[,2][1]], rep(NA, (numFlows - 1))),
                   lat_dest = c(county_info$lat[county_info$GEOID==Tf[,2][1]], rep(NA, (numFlows - 1))),
                   stringsAsFactors = FALSE)
                   
  # One suggestion for optimizing this nested loop:  R-loops are notoriously (but not always) slow --> this should/will 
  # be re-mapped to apply(.) (or list-apply-like operation that maps a function to a collection of inputs)
  k = 1
  for(i in 2:nrow(Tf)){
    select = TRUE
    for(j in 1:k){
      origLongLat = matrix(c(county_info$lon[county_info$GEOID==Tf[i ,1]],county_info$lat[county_info$GEOID==Tf[,1][i]],
                             county_info$lon[county_info$GEOID==as.character(Ts[j, 1])],county_info$lat[county_info$GEOID==as.character(Ts[j, 1])]),
                           nrow=2,ncol=2,byrow=TRUE)
      destLongLat = matrix(c(county_info$lon[county_info$GEOID==Tf[,2][i]],county_info$lat[county_info$GEOID==Tf[,2][i]],
                             county_info$lon[county_info$GEOID==as.character(Ts[j, 2])], county_info$lat[county_info$GEOID==as.character(Ts[j, 2])]),
                           nrow=2,ncol=2,byrow=TRUE)
      origBand = county_info$bandwidth[county_info$GEOID== as.character(Ts[j, 1])] +
                 county_info$bandwidth[county_info$GEOID == Tf[i, 1]]
      destBand = county_info$bandwidth[county_info$GEOID== as.character(Ts[j, 2])] +
                 county_info$bandwidth[county_info$GEOID==Tf[i, 2]]
      
      if(dist(origLongLat,method="euclidean") < origBand
         || dist(destLongLat,method="euclidean") < destBand
         || dist(origLongLat,method="euclidean") < distThresh
         || dist(destLongLat,method="euclidean") < distThresh
         )
        select = FALSE
    
    }
    
    if(select == TRUE && k <= numFlows){
      Ts[k+1,] = c(Tf[i,],origLongLat[1,],destLongLat[1,])
      k = k+1
    }
    
    if(k == numFlows){
      break
    }
  
  }
  
  return(Ts)
}

####TEST/Timing
time = proc.time()
mapFlows = flowSelection(100,1)
out = proc.time() - time

save(mapFlows, file = "out_2/map_flows_100.RData")


y = c(log(16.437),log(90.104),log(1370.421))
x = c(log(10),log(20),log(50))
lm(y ~ x) ##Not great ~ cubic time per number of flows.
