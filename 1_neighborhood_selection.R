# Find the neighborhoods of counties and of flows, according to
# specified neighborhood size p

data <- read.csv("data/County2010wCoordPOP.csv", header = TRUE,
                 colClasses = c("character", NA, NA, NA))
migration <- read.csv("data/countyMigration1978_2011.csv", header = TRUE)

self <- as.integer(migration$dest_geocode) == as.integer(migration$orig_geocode)
edges <- migration[which(!self & migration$year == 2010), ]
edges <- edges[ , c(18, 17)]
rownames(edges) <- seq(nrow(edges))
pop <- data[, 2]
dist <- as.matrix(dist(as.matrix(data[, c(3, 4)])))

n <- length(pop)
p <- 1000000
county <- vector(mode = "list", length = n)
weight <- rep(NA,n)
bandwidth <- rep(NA,n)
names(county) <- data[,1]

for(i in 1:n){
    cat(i, "\r")
    dist_ord <- order(dist[i, ])
    ord_pop <- pop[dist_ord]
    total = cumsum(ord_pop)
    last_c <- which(total>=p)[1]
    ngbrhd <- dist_ord[1:last_c]
    pop_n <- pop[ngbrhd]
    weight[i] <- (p - (sum(pop_n[-length(pop_n)]))) / pop_n[length(pop_n)]  
    bandwidth[i] <- dist[i, dist_ord[last_c]]
    county[[i]] <- data[ngbrhd[-1], 1]
}

a <- function(i) {
    cat(i, "\r")
    origin <- as.character(edges[i, 1])
    dest <- as.character(edges[i, 2])
    orig_n <- c(county[[origin]], origin)
    dest_n <- c(county[[dest]], dest)
    origin_set <- which(is.element(edges[, 1], orig_n))
    dest_set <- origin_set[is.element(edges[origin_set, 2], dest_n)]
    return(dest_set)
}

data$weight <- weight
data$bandwidth <- bandwidth

flow_neigh <- lapply(seq(1:e), a)
county_info <- data
county_neigh <- county

save(county_info, file = "out_1/county_info.RData")
save(county_neigh, file = "out_1/county_neighborhoods.RData")
save(flow_neigh, file = "out_1/flow_neighborhoods.RData")
