options(stringsAsFactors = F)

library(tidyverse)
library(readxl)
library(sf)
library(raster)
library(tokenizers)
library(pbapply)
library(rgdal)

IPA <- 'C://Users/matt/village-names/ipa.xlsx'

read_geonames_file <- function(filename, 
                               featureclass=c('P', 'V', 'H', 'T'),
                               include_alt=FALSE){
  
  # feature classes:
  # A: country, state, region,...
  # H: stream, lake, ...
  # L: parks,area, ...
  # P: city, village,...
  # R: road, railroad 
  # S: spot, building, farm
  # T: mountain,hill,rock,... 
  # U: undersea
  # V: forest,heath,...
  
  topodf <- read.delim(filename, header=F, encoding="UTF-8")
  
  x <- c('geonameid','name','asciiname','alternatenames','latitude','longitude','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')
  
  names(topodf) <- x
  
  topodf <- topodf %>%
    filter(feature_class %in% featureclass) %>%
    dplyr::select(name, alternatenames, latitude, longitude)
  
  if (include_alt){
    topodf <- topodf %>%
      mutate(name=paste0(name, ',', alternatenames)) %>%
      dplyr::select(-alternatenames) %>%
      mutate(name=str_split(name, ',')) %>%
      unnest() %>%
      filter(name!='')
  } else{
    topodf <- topodf %>%
      dplyr::select(-alternatenames)
  }
  
  topodf <- topodf %>%
    mutate(name=tolower(name))
  
  return(topodf)
  
}

convert_ipa <- function(topodf, ref){
  for (i in 1:nrow(ref)){
    topodf$name <- gsub(pattern=ref$str[i], ref$rep[i], topodf$name, perl=TRUE)
  }
  topodf
}


make_raster <- function(e, n, proj){
  l <- as.list(e)
  
  xmn <- l[1]
  xmx <- l[2]
  ymn <- l[3]
  ymx <- l[4]
  
  height <- ymx - ymn
  width <- xmx - xmn
  
  r <- height/width
  
  y <- sqrt(r*n)
  
  x <- n/y
  
  ncell <- floor(x)*floor(y)
  
  rast <- raster(matrix(1:ncell, nrow=floor(y), ncol=floor(x)),
                 xmx=xmx, xmn=xmn, ymx=ymx, ymn=ymn,
                 crs=proj)
  
  rast
}

getNGrams <- function(str, n){
  len <- nchar(str)
  mapply(substr, start=1:(len-(n-1)), stop=n:len, x=str)
}


Moran.I.Alt <- function (gram, binmat, weight, S1, S2, s.sq, na.rm = FALSE, s=s, alternative = "two.sided"){
  x <- as.numeric(binmat[ ,gram])
  n <- length(x)
  ei <- -1/(n - 1)
  m <- mean(x)
  y <- x - m
  cv <- sum(weight * y %o% y)
  v <- sum(y^2)
  obs <- (n/s) * (cv/v)
  k <- (sum(y^4)/n)/(v/n)^2
  sdi <- sqrt((n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * s.sq) - 
                 k * (n * (n - 1) * S1 - 2 * n * S2 + 6 * s.sq))/((n - 
                                                                     1) * (n - 2) * (n - 3) * s.sq) - 1/((n - 1)^2))
  alternative <- match.arg(alternative, c("two.sided", "less", 
                                          "greater"))
  pv <- pnorm(obs, mean = ei, sd = sdi)
  if (alternative == "two.sided") 
    pv <- if (obs <= ei) 
      2 * pv
  else 2 * (1 - pv)
  if (alternative == "greater") 
    pv <- 1 - pv
  data.frame(gram = gram, observed = obs, expected = ei, sd = sdi, p.value = pv, stringsAsFactors = F)
}

run_moran <- function(distmat, binmat){
  weight <- 1/distmat
  diag(weight) <- 0
  weight[is.infinite(weight)] <- 0
  
  ROWSUM <- rowSums(weight)
  ROWSUM[ROWSUM == 0] <- 1
  weight <- weight/ROWSUM
  s <- sum(weight)
  s.sq <- s^2
  S1 <- 0.5 * sum((weight + t(weight))^2)
  S2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
  
  clustering <- pblapply(X = colnames(binmat), FUN = Moran.I.Alt, binmat=binmat, weight=weight, S1=S1, S2=S2, s=s, s.sq=s.sq) %>% bind_rows
}