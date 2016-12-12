setwd('/Users/matthewcooper/Creativitea/village-names/data')
setwd('D:/Documents and Settings/mcooper/GitHub/village-names/data')

library(dplyr)
library(rgdal)
library(doParallel)

##################
### Read in Data & Prep names
##################

CI <- read.delim('CI.txt', stringsAsFactors = F, header = F)
ML <- read.delim('ML.txt', stringsAsFactors = F, header = F)
BF <- read.delim('BF.txt', stringsAsFactors = F, header = F)
SN <- read.delim('SN.txt', stringsAsFactors = F, header = F)
GN <- read.delim('GN.txt', stringsAsFactors = F, header = F)
MR <- read.delim('MR.txt', stringsAsFactors = F, header = F)

x <- c('geonameid','name','asciiname','alternatenames','latitude','longitude','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')

names(CI) <- x
CI <- CI[ , c('geonameid', 'asciiname', 'latitude', 'longitude', 'feature_class')]
names(ML) <- x
ML <- ML[ , c('geonameid', 'asciiname', 'latitude', 'longitude', 'feature_class')]
names(BF) <- x
BF <- BF[ , c('geonameid', 'asciiname', 'latitude', 'longitude', 'feature_class')]
names(SN) <- x
SN <- SN[ , c('geonameid', 'asciiname', 'latitude', 'longitude', 'feature_class')]
names(GN) <- x
GN <- GN[ , c('geonameid', 'asciiname', 'latitude', 'longitude', 'feature_class')]
names(MR) <- x
MR <- MR[ , c('geonameid', 'asciiname', 'latitude', 'longitude', 'feature_class')]

vills <- bind_rows(CI, ML, BF, SN, GN, MR)

#Select villages, cities, towns, etc, leaving out universities, forests, etc
vills <- vills[vills$feature_class=='P', ]

vills <- vills[nchar(vills$asciiname) > 6, ]

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

vills <- vills[ , c('geonameid', 'NamE', 'latitude', 'longitude')] %>% unique

################
###Get 3-grams
###############

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

row.names(binmat) <- vills$geonameid

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

source("../scripts/Moran.I.Alt.R")

clustering <- lapply(X = colnames(binmat), FUN = Moran.I.Alt, binmat=binmat, weight=weight, S1=S1, S2=S2, s=s, s.sq=s.sq) %>% bind_rows

write.csv(clustering, '3-gram MoransI.csv', row.names=F)

rm(weight)

spatial_grams <- clustering$gram[clustering$p.value < 0.05]

binmatsel <- binmat[ , colnames(binmat) %in% spatial_grams]
rm(binmat)

#####################################
###Try converting to graph 
#####################################

library(igraph)

distmat_space <- as.matrix(dist(vills@coords)) < 10000
distmat_lang <- dist(binmatsel, method='binary')
adjmat <- as.matrix(distmat_lang) < .75
diag(adjmat) <- FALSE

distmat <- distmat_space | adjmat

g  <- graph.adjacency(adjmat)

#first find isolated communities
dg <- decompose.graph(g)

g1 <- dg[[1]]

#Then find communities
fc <- fastgreedy.community(as.undirected(g1))

df1 <- data.frame(NamE=fc$name, group=fc$membership)
mg1 <- merge(df1, vills, by='geonameid') %>% unique
write.csv(mg1, 'spaceORland25km.csv', row.names = F)