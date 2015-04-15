# These functions implement the flow and location neighborhood smoothing methods described in
# Guo 2014 (see readme). Eqn (x) refers to the equation numbers in the paper.

# Input : - list of flow and county neighborhoods as created by 1_neighborhood_selection.R
#         - county information table (coordinates populations etc.)
# Output:

# Load input from step one
load("out_1/county_info.RData")
load("out_1/county_neighborhoods.RData")
load("out_1/flow_neighborhoods.RData")
load("data/edges_2010.RData")

# Neighborhood size
p <- 1e6

# Gaussian location model o(Eqn 1)
gauss_loc <- function(x_0, x_q, s_xq) {
    d <- as.numeric(dist(matrix(c(x_0, x_q), nc = 2, byrow = TRUE)))
    if(d > s_xq) {
       out <- 0
    } else {
        out <- 1 / (2*pi) * exp(- d^2 / (2 * s_xq^2))
    }
    return(out)
}

## Calculate theta (Eqn (2))
theta <- function(loc_neigh) {
    # neighborhood id
    id <- names(loc_neigh)
    # ids of counties in neighborhood
    neigh <- loc_neigh[[1]]
    # neighborhood size
    k <- length(neigh)
    ## Lookup coordinates
    # center location
    x_0 <- c(county_info$lon[county_info$GEOID == id],
              county_info$lat[county_info$GEOID == id])
    # neighborhood locations
    ind <- is.element(county_info$GEOID, neigh)
    x_q <- data.frame("lon" = county_info$lon[ind],
                       "lat" = county_info$lat[ind])
    ## Lookup population and weights
    w_q <- c(rep(1, (k - 1)), county_info$weight[county_info$GEOID == neigh[k]])
    s_q <- county_info$pop[ind]
    # center bandwith
    sig_x0 <- county_info$bandwidth[county_info$GEOID == id]
    ## Calculate theta
    # df for apply calculattion
    thet_df <- cbind(matrix(rep(x_0, k), nc = 2, byrow = T),
                     x_q, w_q, s_q, sig_x0)
    sumfun <- function(row) {
        gauss <- gauss_loc(row[c(1, 2)], row[c(3, 4)], row[7])
        gauss * row[5] * row[6]
    }
    out <- sum(apply(thet_df, 1, sumfun))
    return(out)
}

# loc_smooth (Eqn (3))
loc_smooth <- function(x_q_id, x_0_id) {
    x_q <- county_info[county_info$GEOID == x_q_id, c(3, 4)]
    x_0 <- county_info[county_info$GEOID == x_0_id, c(3, 4)]
    # the value of this weight depends on if q is the last county in a neighborhood
    neigh <- county_neigh[[x_0_id]]
    last <- x_q_id == neigh[length(neigh)]
    if(last) w_q <- county_info[county_info$GEOID == x_0_id, "weight"]
        else w_q <- 1
    sig_x0 <- county_info[county_info$GEOID == x_0_id, "bandwidth"]
    gauss_loc(x_q, x_0, sig_x0) * w_q * p / theta(county_neigh[x_0_id])
}

## Flow smoother (Eqn (4))
# T_q and T_0 are (character) flow ids
flow_smooth <- function(T_q, T_0) {
    ## Get origin and destination ids for the flows
    x_Oq <- edges[T_q, 'orig_geocode']
    x_O0 <- edges[T_0, 'orig_geocode']
    x_Dq <- edges[T_q, 'dest_geocode']
    x_D0 <- edges[T_0, 'dest_geocode']
    ## Calculate location weights
    L_O <- loc_smooth(x_Oq, x_O0)
    L_D <- loc_smooth(x_Dq, x_D0)
    return(L_O * L_D)
}

## Volume smoother Eqn (5)
# T_f is the (character) flow id
vol_smooth <- function(T_f){
    ## Get the flow neighborhood of T_f
    neigh <- flow_neigh[[T_f]]
    ## Get sizes of each flow in the neighborhood
    sizes <- edges[neigh, "filings"]
    weights <- sapply(neigh, function(x) flow_smooth(as.character(x), T_f))
    out <- sum(sizes * weights)
    return(out)
}

## Apply volume smoother to flow neighborhoods
#OUT <- sapply(as.character(seq(1, length(flow_neigh))), vol_smooth)

# benchmark
library(microbenchmark)

t <- microbenchmark(
        vol_smooth("1"),
        times = 10
    )

# Unit: seconds
#            expr      min       lq     mean   median       uq      max neval
# vol_smooth("1") 13.36506 13.43061 13.49197 13.43985 13.62927 13.69672    10
# That would be 296 hours ...
