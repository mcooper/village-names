setwd('/Users/matthewcooper/Creativitea/village-names/2016/')

library(dplyr)
library(rgdal)

##################
### Read in Data & Prep names
##################

DAT <- read.delim('ML.txt', stringsAsFactors = F)
x <- c('geonameid','name','asciiname','alternatenames','latitude','longitude','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')
names(DAT) <- x

#Select villages, cities, towns, etc, leaving out universities, forests, etc
DAT <- DAT[DAT$feature_class=='P', ]

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

DAT$NamE <- sapply(DAT$asciiname, makeLastUpper)

DAT <- DAT[ , c('NamE', 'latitude', 'longitude')] %>% unique
                 
################
###Get 3-grams
###############

getThreeGrams <- function(str){
  len <- nchar(str)
  mapply(substr, start=1:(len-2), stop=3:len, x=str)
}

threeGrams <- sapply(DAT$NamE, getThreeGrams) %>% unlist %>% unique

######################
###Get Binary Matrix
######################

binmat <- sapply(threeGrams,grepl,DAT$NamE)

row.names(binmat) <- DAT$NamE

#######################
###Select only Variables with significant spatial clustering
#######################
coordinates(DAT) <- c('longitude', 'latitude')
proj4string(DAT) <- CRS("+proj=longlat +datum=WGS84")
DAT <- spTransform(DAT, CRS("+proj=aeqd +lat_0=0 +lon_0=-0"))

inverseDistMat <- 1/as.matrix(dist(DAT@coords))
diag(inverseDistMat) <- 0
inverseDistMat[is.infinite(inverseDistMat)] <- 0

##To do - rewrite Moran.I so that all the binmat cv stuff is only calculated once

getMorans <- function(gram, mat){
  Moran.I(as.numeric(binmat[ ,gram]),inverseDistMat)

