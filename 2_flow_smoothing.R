# Input : list of




# Gaussian location model o(Eqn 1)
gauss_loc <- function(x_0, x_q, s_xq) {
    d <- as.numeric(dist(cbind(x_0, x_q)))
    if(d > s_xq) {
       out <- 0
    } else {
        out <- 1 / (2*pi) * exp(- d / (2 * s_xq^2))
    }
    return(out)
}

## Calculate theta (Eqn (2))
# make data frame to sum over

theta <- function(loc_neigh, county_info) {

    # neighborhood size
    k <- length(loc_neigh)
    
    ## Lookup coordinates
    # center location
    x_0 <- c(county_info$lon[county_info$id == names(loc_neigh)],
              county_info$lat[county_info$id == names(loc_neigh)])

    # neighborhood locations
    x_q <- data.frame("lon" = county_info$lon[is.element(county_info$id, loc_neigh)],
                       "lat" = county_info$lat[is.element(county_info$id, loc_neigh)])

    ## Lookup population and weights
    w_q <- c(rep(1, k), county_info$weight[county_id == loc_neigh[k]))
    s_q <- county_info$pop[is.element(county_info$id, loc_neigh)]

    # center bandwith
    sig_x0 <- county_info$bandwith[county_info$id == names(loc_neigh)]
    
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

loc_smooth <- function(x_q, x_0, w_q, sig_x0) {
    
}


    



## Weight for each  T_q in the neighborhood of  T_0
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
