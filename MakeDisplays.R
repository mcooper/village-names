########################
#Display Classification
########################
library(rgdal)

df <- read.csv('~/Creativitea/village-names/data/spaceORlangFastgreedy.csv')

borders <- readOGR('/Users/matthewcooper/Creativitea/Basic Data/TM_WORLD_BORDERS-0.3', 'TM_WORLD_BORDERS-0.3')

spdf <- SpatialPointsDataFrame(coords=df[ , c('longitude', 'latitude')], data=df, proj4string = CRS("+proj=aeqd +lat_0=0 +lon_0=-0"))

spdf_proj <- spTransform(spdf, CRS("+proj=longlat +datum=WGS84"))

plot(x = spdf_proj$longitude, y=spdf_proj$latitude, col=spdf_proj$group, xlim=c(-17.5, 2.5), ylim=c(4.5, 18), xlab='', ylab='', axes=F, pch=19 ,cex=.25)
plot(borders, add=T)


###########################
#Display individual 3-grams
###########################

setwd('/Users/matthewcooper/Creativitea/village-names/data/')

CI <- read.delim('CI.txt', stringsAsFactors = F, header = F)
ML <- read.delim('ML.txt', stringsAsFactors = F, header = F)
BF <- read.delim('BF.txt', stringsAsFactors = F, header = F)
SN <- read.delim('SN.txt', stringsAsFactors = F, header = F)
GN <- read.delim('GN.txt', stringsAsFactors = F, header = F)
MR <- read.delim('MR.txt', stringsAsFactors = F, header = F)

library(maps)
library(mapdata)
library(dplyr)

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

sel <- vills[grepl('ama', vills$asciiname), ]

map(database='world', regions=c('Senegal','Guinea-Bissau', 'Sierra Leone', 'Liberia', 'Guinea','Ivory Coast','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'), xlim=c(-17.5, 2.5), ylim=c(4, 20), main='Place names containing the 3-gram "kro"')
points(sel$longitude, sel$latitude, cex=0.25, pch=19, col=2)

             