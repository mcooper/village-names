setwd("/Users/matthewcooper/Creativitea/village-names/")

#libraries for functions
library(stringr)
library(maps)
library(mapdata)
library(raster)
library(MASS)
library(rgeos)
library(rgdal)
library(sp)
library(geosphere)
library(ape)

##Read in text files for all countries
BF <- read.delim('Toponyms/BF.txt', header=F)
CI <- read.delim('Toponyms/CI.txt', header=F)
GN <- read.delim('Toponyms/GN.txt', header=F)
GW <- read.delim('Toponyms/GW.txt', header=F)
LR <- read.delim('Toponyms/LR.txt', header=F)
MR <- read.delim('Toponyms/MR.txt', header=F)
NE <- read.delim('Toponyms/NE.txt', header=F)
SL <- read.delim('Toponyms/SL.txt', header=F)
SN <- read.delim('Toponyms/SN.txt', header=F)
ML <- read.delim('Toponyms/ML.txt', header=F)
NG <- read.delim('Toponyms/NG.txt', header=F)
GM <- read.delim('Toponyms/GM.txt', header=F)
TG <- read.delim('Toponyms/TG.txt', header=F)
BJ <- read.delim('Toponyms/BJ.txt', header=F)
GH <- read.delim('Toponyms/GH.txt', header=F)

#Rename all the coulumns
x <- c('geonameid','name','asciiname','alternatenames','latitude','longitude','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')
names(ML)<-x
names(BF)<-x
names(CI)<-x
names(GN)<-x
names(GW)<-x
names(LR)<-x
names(MR)<-x
names(NE)<-x
names(SL)<-x
names(SN)<-x
names(NG)<-x
names(TG)<-x
names(BJ)<-x
names(GM)<-x
names(GH)<-x

#Add a column to identify which country a point is from
ML$country <- 'Mali'
BF$country <- 'Burkina Faso'
CI$country <- 'Cote d\'ivoire'
GN$country <- 'Guinea'
GW$country <- 'Guinea-Bissau'
LR$country <- 'Liberia'
MR$country <- 'Mauritania'
NE$country <- 'Niger'
SL$country <- 'Sierra Leone'
SN$country <- 'Senegal'
NG$country <- 'Nigeria'
TG$country <- 'Togo'
BJ$country <- 'Benin'
GH$country <- 'Ghana'
GM$country <- 'Gambia'

#Create a dataset of all geonames
geonames <- rbind(ML,BF,CI,GN,GW,LR,MR,NE,SL,SN,NG,TG,BJ,GH,GM)

#Select villages, cities, towns, etc, leaving out universities, forests, etc
vills <- geonames[geonames$feature_class=='P',]

#Read in orthography-normalized villages
new <- read.delim('data/fixvills.txt', header=FALSE, sep='$')
#Because I added $$$ to them in python as a delimitter, only the first column is added to vills
names(new) <- c('a','b')
vills$standname <- new$a

coordselect <- vills[,2:3]
unique <- duplicated(coordselect)
vills <- vills[!unique,]


#map villages by search string with raster
villsearchraster <- function(str,res=5){
  m <- matrix(c(vills$longitude[str_detect(vills$asciiname,str)],vills$latitude[str_detect(vills$asciiname,str)]),ncol=2)
  r <- raster(ncols=34*res, nrows=19*res, xmn=-18,xmx=16,ymn=4,ymx=27)
  density <- rasterize(m, r, fun=function(x,...)length(x))
  plot(density,colNA='black',axes=FALSE)
  map(database='world',col='white',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27), add=TRUE)
  return(density)
}

popdensity <- function(res=5){
  m <- matrix(c(vills$longitude,vills$latitude),ncol=2)
  r <- raster(ncols=34*res, nrows=19*res, xmn=-18,xmx=16,ymn=4,ymx=27)
  popdensity <- rasterize(m, r, fun=function(x,...)length(x))
  plot(popdensity)
  map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27), add=TRUE)
  return(popdensity)
}

#map vills raster, normalized by population
villsearchrasternorm <- function(str,res=5){
  m <- matrix(c(vills$longitude,vills$latitude),ncol=2)
  r <- raster(ncols=34*res, nrows=19*res, xmn=-18,xmx=16,ymn=4,ymx=27)
  popdensity <- rasterize(m, r, fun=function(x,...)length(x))
  m <- matrix(c(vills$longitude[str_detect(vills$asciiname,str)],vills$latitude[str_detect(vills$asciiname,str)]),ncol=2)
  r <- raster(ncols=34*res, nrows=19*res, xmn=-18,xmx=16,ymn=4,ymx=27)
  density <- rasterize(m, r, fun=function(x,...)length(x))
  plot(density/popdensity, axes=FALSE, colNA='black')
  map(database='world',col='white',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
}

#map villages by search string with contours
villsearchcontours <- function(str,H,N){
  map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27))
  points(vills$longitude[str_detect(vills$asciiname,str)],vills$latitude[str_detect(vills$asciiname,str)],pch=19,col="red",cex=.25)
  matrix(c(vills$longitude[str_detect(vills$asciiname,str)],vills$latitude[str_detect(vills$asciiname,str)]),ncol=2)->X
  z <- kde2d(X[,1], X[,2], h=H,n=N) #h determines the bandwith of the contours. A higher h results in looser contours, a low h results in tighter contours.
  library(RColorBrewer)
  k <- 11
  my.cols <- rev(brewer.pal(k, "RdYlBu"))
  contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
  return(z)
}

#map villages by search string with points
villsearchpoints <- function(str){
  map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27))
  points(vills$longitude[str_detect(vills$standname,str)],vills$latitude[str_detect(vills$standname,str)],pch=19,col="red",cex=.25)
}

#create a buffer, where the size of the buffer depends on the popdensity
buffer <- function(str,width=.2){
  m <- matrix(c(vills$longitude[str_detect(vills$asciiname,str)],vills$latitude[str_detect(vills$asciiname,str)]),ncol=2)
  msp <- SpatialPoints(m)
  #crs(msp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  proj4string(msp) = CRS("+init=epsg:3395") 
  plot(gBuffer(msp,width=width))
  map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
}

#return a sample of twenty points
villsearchtablesample <- function(str,amount=10){
  villsearchpoints(str)
  select <- vills[str_detect(vills$asciiname,str),]
  num <- sample(1+amount:dim(select)[1]-amount,1)
  bum <- num+amount
  return(select[num:bum,2:3])
}

villsearchpoints <- function(str){
  map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27))
  points(vills$longitude[str_detect(vills$asciiname,str)],vills$latitude[str_detect(vills$asciiname,str)],pch=19,col="red",cex=.25)
}

villfindclust <- function(str,dist=40,method='single'){
  vills[str_detect(vills$asciiname,str),]->select
  dist <- dist*1000
  xy <- SpatialPointsDataFrame(matrix(c(select$longitude,select$latitude),ncol=2),select,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  xy <- spTransform(xy, CRS("+init=epsg:27700 +datum=WGS84"))
  chc <- hclust(dist(data.frame(rownames=rownames(xy@data), x=coordinates(xy)[,1],
                                y=coordinates(xy)[,2])), method=method)
  # Distance with a 40m threshold  
  chc.d40 <- cutree(chc, h=dist) 
  
  # Join results to meuse sp points
  xy@data <- data.frame(xy@data, Clust=chc.d40)
  
  # Plot results
  map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27))
  points(x=xy@data[,6],y=xy@data[,5], col=factor(xy@data$Clust), pch=19, cex=.25)
}
#Now you need to do a concave hull! One option is to buffer, and then inverse buffer the result

# Neighbor row indices and add neighbor attribute ID's   
'(knn1 <- knearneigh(coordinates(spselect), k=1)$nn)
spselect@data$nnID <- spselect@data[knn1,]$IDS 

# Add neighbor distance
spselect.dist <- dnearneigh(coordinates(spselect), 0, 20)
dist.list <- nbdists(spselect.dist, coordinates(spselect))
spselect@data$nnDist <- unlist(lapply(dist.list, FUN=function(x) min(x)))

spselect@data  '

###

mcmoran <- function(str,n=1000){
  samp <- vills[sample(nrow(vills),n),]
  m <- matrix(c(samp$latitude,samp$longitude),nrow=n,ncol=2)
  dis <- distm(m)
  moran <- 1/dis
  diag(moran) <- 0
  moran[is.infinite(moran)]<-0
  Moran.I(str_detect(samp$asciiname,str),moran)->out
  return(out)
}

getmorans <- function(grams){
  t <- grams[1]
  t <- c(t, dim(vills[str_detect(vills$asciiname,grams[1]),])[1])
  for (i in 1:3){
    t <- c(t,try(mcmoran(grams[1],5000),silent=TRUE)[c(1,4)])
  }
  t <- data.frame(as.list(t))
  names(t)<- c('str','count','o1','p1','o2','p2','o3','p3')
  for (i in 2:length(grams)){
    u <- grams[i]
    u <- c(u, dim(vills[str_detect(vills$asciiname,grams[i]),])[1])
    for (j in 1:3){
      u <- c(u,try(mcmoran(grams[i],5000),silent=TRUE)[c(1,4)])
    }
    u <- data.frame(as.list(u))
    names(u) <- c('str','count','o1','p1','o2','p2','o3','p3')
    t <- rbind(t,u)
    print(c(i,grams[i]))
  }
  t$minp <- pmin(t$p1, t$p2, t$p3)
  return(t)
}

### Create the variance raster

rast <- function(res=5){
  to <- 34*23*res^2
  m <- matrix(seq(1,to),ncol=34*res, nrow=23*res, byrow=TRUE)
  r <- raster(m, xmn=-18,xmx=16,ymn=4,ymx=27)
  return(r)
}

cellnames <- function(res=5){
  s <- (1/res)
  lonmin <- seq(-18, 16-s, s)
  lonmax <- seq(-18+s, 16, s)
  latmin <- seq(27-s, 4, -s)
  latmax <- seq(27, 4-s, -s)
  cell <- 0
  str <- ''
  len <- 0
  xmin <- 0
  xmax <- 0
  ymin <- 0
  ymax <- 0
  df <- data.frame(cell, str, len, xmin, xmax, ymin, ymax)
  for (i in 1:length(latmin)){
    latselect <- vills[which(vills$latitude > latmin[i] & vills$latitude < latmax[i]),]
    for (j in 1:length(lonmin)){
      selection <- latselect[which(latselect$longitude > lonmin[j] & latselect$longitude < lonmax[j]),]
      str <- ''
      len <- dim(selection)[1]
      cell <- cell + 1
      ymin <- latmin[i]
      ymax <- latmax[i]
      xmin <- lonmin[j]
      xmax <- lonmax[j]
      for (k in 1:length(selection$asciiname)){
        str <- paste(str,selection$asciiname[k],sep='.')
      }
      f <- data.frame(cell, str, len, xmin, xmax, ymin, ymax)
      df <- rbind(df,f)
    }
  }
  return(df[2:nrow(df),])
}

#cn <- cellnames(5)
#write.csv(cn, 'cellnames.csv', row.names=F)
cn <- read.csv('cellnames.csv')

r <- rast(5)


##Run this to get summary of ngrams, or just read in table since it takes ~12 hours
grams <- read.table('threegrams.txt',header=F,sep='$')
#gm <- getmorans(grams)

#... or read in table
gm <- read.csv('grammoran.csv')


##

binmat <- function(a,b){
  #Create a binary matrix of columns list of strings a, and rows list of strings b
  #if sub string a is in long string b, a 1 is put, otherwise a 0 is put
  all <- NULL
  for (j in 1:length(b)){
    for (i in 1:length(a)){
      all <- c(all, str_detect(b[j],a[i]))
    }
    print(paste0(as.character(j/length(b)*100),' percent complete!'))
  }
  m <- matrix(all,nrow=length(b),ncol=length(a))
  d <- data.frame(m)
  names(d) <- a
  d$longstr <- b
  return(d)
}

completestr <- cn[!cn$str=='.NA.',]

#binmatoutput <- str_detect(as.character(gm$str[1:10]),as.character(completestr$str[1:15]))
#binmat$X <- NULL
#binmatoutput$cellind <- completestr$cell
##binmatoutput really not working in R, will try Python
#write.table(completestr$str,'completevills.txt', row.names=FALSE, col.names=FALSE, quote=FALSE)
#write.table(gm$str,'threegramsforbinmat.txt',row.names=FALSE, col.names=FALSE, quote=FALSE)

possiblengrams <- function(string, n){
  grams <- ('')
  for (i in 0:(nchar(string)-n)){
    gram <- substr(string,1+i,n+i)
    if (!grepl('\\.| ',gram) & !(gram %in% grams)){
      grams <- c(gram,grams)
    }
  }
  return(length(grams)-1)
}

binmat <- read.csv('binmattest2.txt', header=TRUE)
binmat$X <- NULL
binmat <- merge(binmat, cn, by.x='vills', by.y='str', all.x=TRUE, all.y=FALSE)
binmat$possiblestrs <- lapply(as.character(binmat$vills), possiblengrams, n=3)

datatoraster <- function(data,ordcol,plotcol,res=5){
  #data is the data with columns corresponding to the raster values in raster r (ordcol)
  #and to the data to be plotted in the output raster (plotcol)
  ordered <- data[order(data[,ordcol]),]
  m <- matrix(data[,plotcol], ncol=34*res, nrow=23*res, byrow=TRUE)
  n <- raster(m, xmn=-18,xmx=16,ymn=4,ymx=27)
  return(n)
}

villsplotbinrast <- function(str, cellnames){
  cellnames$cells <- str_detect(cellnames$str,str)
  r <- datatoraster(cellnames[,c('cell','cells')],'cell','cells',5)
  plot(r)
  map(database='world',col='black',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
  return(r)
}
  
  
##Kissa is at 13656! 10.9165471,-6.8834685

getcellnumbycoords <- function(lat,lon,r){
  sp <- SpatialPoints(matrix(c(lon,lat),nrow=1,ncol=2))
  e <- extract(r,sp)
  return(as.numeric(e))
}

#cellind <- getcellnumbycoords(10.9165471,-6.8834685,r)

varianceraster <- function(binmat,cellind){
  #takes a binary matrix a given cell and generates a raster of % of 3grams in given cell
  #that are also present for each other cell
  row <- binmat[binmat$cell==cellind,]
  row <- row[!is.na(row$cell),]
  sel <- binmat[,c(1,which(row[1,]==1,),2643,2649)]
  sel$count <- rowSums(sel[,names(sel)[nchar(names(sel))==3]])
  cell <- seq(1,19550)
  tomerge <- data.frame(cell)
  merged <- merge(tomerge, sel, by='cell', all.x=TRUE)
  merged$percmatch <- 0
  merged$percmatch[!(is.na(merged$count)|is.na(merged$possiblestrs))] <- merged$count[!(is.na(merged$count)|is.na(merged$possiblestrs))]/as.numeric(merged$possiblestrs[!(is.na(merged$count)|is.na(merged$possiblestrs))])
  merged[merged$cell==cellind,'percmatch'] <- 0
  vr <- datatoraster(merged,1,ncol(merged),5)
  return(vr)
}

plotfromcoords <- function(lat,lon){
  vr <- varianceraster(binmat,getcellnumbycoords(lat,lon,r))
  plot(vr)
  map(database='world',col='black',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
  return(as.character(cn$str[cn$cell==getcellnumbycoords(lat,lon,r)]))
}
 

#plotfromcoords(11.3093737,-5.6638287)

plotfromname <- function(name){
  lat <- vills$latitude[vills$name==name]
  lon <- vills$longitude[vills$name==name]
  map(database='world',col='black',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
  plotfromcoords(lat,lon)
}

#Clearly need to normalize by number of possible trigrams in a cell.


getres <- function(n){
  sample(as.character(gm$str[gm$minp<n]),1)->chk
  print(chk)
  print(gm$minp[gm$str==chk])
  villsearchtablesample(chk)
}

smoothplotfromcoords <- function(lat, lon, res=5){
  vr <- varianceraster(binmat,getcellnumbycoords(lat,lon,r))
  plot(focal(vr, matrix(rep(1,res^2),nrow=res), fun=mean, na.rm=TRUE))
  map(database='world',col='black',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
  return(as.character(cn$str[cn$cell==getcellnumbycoords(lat,lon,r)]))
}

smoothplotfromname <- function(name, res=5){
  lat <- vills$latitude[vills$name==name]
  lon <- vills$longitude[vills$name==name]
  vr <- varianceraster(binmat,getcellnumbycoords(lat,lon,r))
  plot(focal(vr, matrix(rep(1,res^2),nrow=res), fun=mean, na.rm=TRUE))
  map(database='world',col='black',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27),add=TRUE)
  return(as.character(cn$str[cn$cell==getcellnumbycoords(lat,lon,r)]))
}

smoothplotfromname('Bamako')
