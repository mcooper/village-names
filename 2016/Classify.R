setwd('/Users/matthewcooper/Creativitea/village-names/JustMali')

library(dplyr)

##################
### Read in Data & Prep names
##################

ML <- read.delim('ML.txt', stringsAsFactors = F)
x <- c('geonameid','name','asciiname','alternatenames','latitude','longitude','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')
names(ML) <- x

#Select villages, cities, towns, etc, leaving out universities, forests, etc
ML <- ML[ML$feature_class=='P', ]

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

ML$NamE <- sapply(ML$asciiname, makeLastUpper)
                 
################
###Get 3-grams
###############

getThreeGrams <- function(str){
  len <- nchar(str)
  mapply(substr, start=1:(len-2), stop=3:len, x=str)
}

threeGrams <- sapply(ML$NamE, getThreeGrams) %>% unlist %>% unique

######################
###Get Binary Matrix
######################

binmat <- sapply(threeGrams,grepl,ML$NamE)

row.names(binmat) <- ML$NamE

#######################
###Select only Variables with significant spatial clustering
#######################

inverseDistMat <- 1/as.matrix(dist(binmat[ , c('longitude', 'latitude')]))
diag(inverseDistMat) <- 0

getMorans <- function(gram, binmat){
  Moran.I(binm
  

