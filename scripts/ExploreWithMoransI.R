setwd("/Users/matthewcooper/R workspace")

library(raster)
library(maps)

newgrams <- read.csv('newMorans.csv')
vills <- read.csv('villsforshiny.csv')
load('binmat3.rda')

rast <- function(res=5){
  to <- 34*23*res^2
  m <- matrix(seq(1,to),ncol=34*res, nrow=23*res, byrow=TRUE)
  r <- raster(m, xmn=-18,xmx=16,ymn=4,ymx=27)
  return(r)
}

r <- rast(5)

getcellnumbycoords <- function(lat,lon,r){
  sp <- SpatialPoints(matrix(c(lon,lat),nrow=1,ncol=2))
  e <- extract(r,sp)
  return(as.numeric(e))
}

datatoraster <- function(data,ordcol,plotcol,res=5){
  #data is the data with columns corresponding to the raster values in raster r (ordcol)
  #and to the data to be plotted in the output raster (plotcol)
  ordered <- data[order(data[,ordcol]),]
  m <- matrix(data[,plotcol], ncol=34*res, nrow=23*res, byrow=TRUE)
  n <- raster(m, xmn=-18,xmx=16,ymn=4,ymx=27)
  return(n)
}

plotm <- function(name,morans,res=5,basegrams=T){
  #morans < 0 includes NA trigrams, with only one or two points
  if(morans >= 0){
    trigramsel <- c("vills", as.character(newgrams$V1[which(newgrams$V2>morans & !is.na(newgrams$V2))]), "cell", "len", "xmin", "xmax", "ymin", "ymax", "possiblestrs")
  } else {
    trigramsel <- names(binmat3)
  }
  binmat <- binmat3[,trigramsel]
  if(basegrams){
    possiblengrams <- function(string, n){
      grams <- ('')
      for (i in 0:(nchar(string)-n)){
        gram <- substr(string,1+i,n+i)
        if (!grepl('\\.| ',gram) & !(gram %in% grams) & (gram %in% trigramsel)){
          grams <- c(gram,grams)
        }
      }
      return(length(grams)-1)
    }
    binmat$possiblestrs <- lapply(as.character(binmat$vills), possiblengrams, n=3)
  }
  lat <- vills$latitude[vills$asciiname==name]
  lon <- vills$longitude[vills$asciiname==name]
  cellind <- getcellnumbycoords(lat,lon,r)
  row <- binmat[binmat$cell==cellind,]
  row <- row[!is.na(row$cell),]
  sel <- binmat[,c(1,which(row[1,]==TRUE),ncol(row)-6,ncol(row))]
  sel$count <- rowSums(sel[,names(sel)[nchar(names(sel))==3]])
  cell <- seq(1,19550)
  tomerge <- data.frame(cell)
  merged <- merge(tomerge, sel, by='cell', all.x=TRUE)
  merged$percmatch <- 0
  merged$percmatch[!(is.na(merged$count)|is.na(merged$possiblestrs))] <- merged$count[!(is.na(merged$count)|is.na(merged$possiblestrs))]/as.numeric(merged$possiblestrs[!(is.na(merged$count)|is.na(merged$possiblestrs))])
  merged[merged$cell==cellind,'percmatch'] <- 0
  merged[is.nan(merged$percmatch),] <- NA
  vr <- datatoraster(merged,1,ncol(merged),5)
  newr <- focal(vr, matrix(rep(1,res^2),nrow=res), fun=mean, na.rm=TRUE)*100
  plot(newr)
  map(database='world',col='black',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
  print(binmat$possiblestrs[binmat$cell==cellind])
}
  