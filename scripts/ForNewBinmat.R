setwd("/Users/matthewcooper/R workspace")

library(raster)

grams <- read.table('threegrams.txt',header=F,sep='$')
vills <- read.csv('villsforshiny.csv')

villsMoran <- function(str,res=5){
  m <- matrix(c(vills$longitude[grep(str,vills$standname)],vills$latitude[grep(str,vills$standname)]),ncol=2)
  r <- raster(ncols=34*res, nrows=19*res, xmn=-18,xmx=16,ymn=4,ymx=27)
  density <- rasterize(m, r, fun=function(x,...)length(x))
  return(Moran(density))
}

grams$V2 <- lapply(grams$V1,villsMoran)

grams$V2[grams$V2=='NaN'] <- NA
grams$V2 <- as.numeric(grams$V2)

write.csv(grams,'newMorans.csv',row.names=FALSE)

allgrams <- grams[!is.na(grams$V2),]

newgrams <- allgrams[order(allgrams$V2, decreasing=T),]


binmat2 <- binmat['vills']

for (i in grams$V1){
  binmat2[i] <- grepl(i,binmat2$vills)
}

write.csv(binmat2m, 'binmat2.csv', row.names=F)

binmat2m <- merge(binmat2, binmat, by='vills')

save(binmat3, file='binmat3.rda')

         
