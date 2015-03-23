###Testing

data=read.csv("data/County2010wCoordPOP.csv",header=T)

edges=Migration[-which(Migration$dest_geocode==Migration$orig_geocode),]
edges=edges[,c(18,17)]
rownames(edges)=seq(nrow(edges))
pop=data[,2]
dist=as.matrix(dist(as.matrix(data[,c(3,4)])))
dist_test=dist(data[,c(3,4)])


n=length(pop)
p=1000000
county=list()
weight=rep(NA,n)
bandwidth=rep(NA,n)
names(county)=data[,1]
for(i in 1:n){
		total=cumsum(pop[order(dist[i,])])
		which(total>=p)[1]
		weight[i]=(p-(sum(pop[order(dist[i,])[1:which(total>=p)[1]]]
			[-length(pop[order(dist[i,])[1:which(total>=p)[1]]])])))/
			pop[order(dist[i,])[1:which(total>=p)[1]]][length(pop[order(dist[i,])[1:which(total>=p)[1]]])]
			bandwidth[i]=dist[i,order(dist[i,])[which(total>=p)[1]]]
			county[[i]]=data[order(dist[i,])[1:which(total>=p)[1]][-1],1]
}

flow=list()
e=length(edges[,1])
for(i in 1:e){
	origin=as.character(edges[i,1])
	dest=as.character(edges[i,2])
	origin_set=seq(nrow(edges))[!is.na(match(edges[,1],c(county[[origin]],origin)))]
	dest_set=origin_set[!is.na(match(edges[origin_set,2],c(county[[dest]],dest)))]
	flow[[i]]=dest_set
	}

