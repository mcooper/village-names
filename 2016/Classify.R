setwd('/Users/matthewcooper/Creativitea/village-names/2016/')
setwd('D:/Documents and Settings/mcooper/GitHub/village-names/2016')

library(dplyr)
library(rgdal)

##################
### Read in Data & Prep names
##################

vills <- read.delim('ML.txt', stringsAsFactors = F)
x <- c('geonameid','name','asciiname','alternatenames','latitude','longitude','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')
names(vills) <- x

#Select villages, cities, towns, etc, leaving out universities, forests, etc
vills <- vills[vills$feature_class=='P', ]

indexCapitalize <- function(str, index){
  if (index==1){
    end <- substr(str, index+1, nchar(str))
    chr <- substr(str, index, index)
    newStr <- paste0(toupper(chr), end)
  }
  else if (index==nchar(str)){
    begin <- substr(str, 1, index-1)
    chr <- substr(str, index, index)
    newStr <- paste0(begin, toupper(chr))
  }
  else{
    begin <- substr(str, 1, index-1)
    end <- substr(str, index+1, nchar(str))
    chr <- substr(str, index, index)
    newStr <- paste0(begin, toupper(chr), end)
  }
  return(newStr)
}

makeLastUpper <- function(str){
  newStr <- indexCapitalize(str, nchar(str))
  ind <- c(gregexpr(pattern=' ', str)[[1]]-1)
  for (i in ind){
    newStr <- indexCapitalize(newStr, i)
  }
  return(newStr)
}

vills$NamE <- sapply(vills$asciiname, makeLastUpper)

vills <- vills[ , c('NamE', 'latitude', 'longitude')] %>% unique

################
###Get 3-grams
###############

#Might want to first filter names less than n + 1 characters when dealing with n grams
#Might also want to vary the number of grams?

getThreeGrams <- function(str){
  len <- nchar(str)
  mapply(substr, start=1:(len-2), stop=3:len, x=str)
}

threeGrams <- sapply(vills$NamE, getThreeGrams) %>% unlist %>% unique

######################
###Get Binary Matrix
######################

binmat <- sapply(threeGrams,grepl,vills$NamE)

row.names(binmat) <- vills$NamE

#######################
###Select only Variables with significant spatial clustering
#######################
coordinates(vills) <- c('longitude', 'latitude')
proj4string(vills) <- CRS("+proj=longlat +datum=WGS84")
vills <- spTransform(vills, CRS("+proj=aeqd +lat_0=0 +lon_0=-0"))

weight <- 1/as.matrix(dist(vills@coords))
diag(weight) <- 0
weight[is.infinite(weight)] <- 0

ROWSUM <- rowSums(weight)
ROWSUM[ROWSUM == 0] <- 1
weight <- weight/ROWSUM
s <- sum(weight)
s.sq <- s^2
S1 <- 0.5 * sum((weight + t(weight))^2)
S2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)

rm(ROWSUM)

source("Moran.I.Alt.R")

clustering <- lapply(X = colnames(binmat), FUN = Moran.I.Alt, binmat=binmat, weight=weight, S1=S1, S2=S2, s.sq=s.sq) %>% bind_rows

write.csv(clustering, '3-gram MoransI.csv', row.names=F)

spatial_grams <- clustering$gram[clustering$p.value < 0.05]

binmatsel <- binmat[ , colnames(binmat) %in% spatial_grams]

############################
###Classify Using ROCK Model
############################

#http://stats.stackexchange.com/questions/70113/cluster-large-boolean-dataset

library(cba)

rockmod1 <- rockCluster(binmatsel, 1, beta = 0.9, theta = 0.99, fun = "dist", funArgs = list(method="binary"), debug = FALSE)


vills$clust <- rockmod$cl

multiclust <- table(rockmod$cl)[table(rockmod$cl) > 1]

vills$multiclust <- vills$clust %in% names(multiclust)

sub <- vills[vills$multiclust, ]

plot(sub$longitude, sub$latitude, col=sub$clust)



#####################################
###Try converting to graph 
#####################################

library(igraph)

distmat <- dist(binmatsel, method='binary')

adjmat <- as.matrix(distmat) < .75
diag(adjmat) <- FALSE

g  <- graph.adjacency(adjmat)

getGroups <- function(graph, clusterFunc, size){
  #graph is a graph of toponyms, connected by similar features
  
  #clusterFunc is the clustering function.  Can be:
  #cluster_fast_greedy
  #cluster_walktrap
  #cluster_spinglass
  #cluster_leading_eigen
  #cluster_edge_betweenness
  
  #size is the size of a distinct group that will by analyzed for clusters
  
  dg <- decompose.graph(g)
  
  df <- 
  
}

#first find isolated communities
dg <- decompose.graph(g) 
clusters(g)

g1 <- dg[[1]]

#Then find communities
fc <- fastgreedy.community(as.undirected(g1))

df <- data.frame(NamE=fc$name, group=fc$membership)

final <- merge(df, vills, by='NamE') %>% unique

