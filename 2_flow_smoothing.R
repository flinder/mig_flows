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
theta <- function(loc_neigh, county_info) {

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

    out <- apply(thet_df, 1, sumfun)
    return(out)
}

# loc_smooth (Eqn (3))

loc_smooth <- function(x_q, x_0, w_q, sig_x0, p) {
    gauss_loc(x_q, x_0, sig_x0) * w_q * p * theta()
}



#================================================================================
# From here on it needs work


## Flow smoother (Eqn (4)) 
# neigh: flow neighborhood
# neigh_c: county neighborhood

flow_smooth <- function(neigh, neigh_c, main_dat, county_info, county_neigh) {

    flow <- as.integer(name(neigh))
    f <- length(neigh)

    ## Lookup data for location based model
    
    
    ## Lookup data for flow based model
    # Center flow
    orig_cf <- main_dat$orig_geocode[flow]
    dest_cf <- main_dat$dest_geocode[flow]

    # Neighbor flows
    orig_f <- main_dat$orig_geocode[neigh]
    dest_f <- main_dat$dest_geocode[neigh]

    ## Lookup coordinates
    # center flow
    x_o0 <- c(county_info$lon[county_info$id == orig_cf],
              county_info$lat[county_info$id == orig_cf])
    x_d0 <- c(county_info$lon[county_info$id == dest_cf],
              county_info$lat[county_info$id == dest_cf])
    # neighbor flows
    x_oq <- data.frame("lon" = county_info$lon[is.element(county_info$id, orig_f)],
                       "lat" = county_info$lat[is.element(county_info$id, orig_f)])
    x_dq <- data.frame("lon" = county_info$lon[is.element(county_info$id, dest_f)],
                       "lat" = county_info$lat[is.element(county_info$id, dest_f)])


    # lookup bandwith
    sig_x0 <- county_info$bandwith[flow]
    
    # G(origin)
    
    # G(destination)
}
