setwd('/Users/matthewcooper/Creativitea/village-names/2016/')
setwd('D:/Documents and Settings/mcooper/GitHub/village-names/2016')

library(dplyr)
library(rgdal)


##################
### Read in Data & Prep names
##################

CI <- read.delim('CI.txt', stringsAsFactors = F, header = F)
ML <- read.delim('ML.txt', stringsAsFactors = F, header = F)
BF <- read.delim('BF.txt', stringsAsFactors = F, header = F)
SN <- read.delim('SN.txt', stringsAsFactors = F, header = F)
GN <- read.delim('GN.txt', stringsAsFactors = F, header = F)
MR <- read.delim('MR.txt', stringsAsFactors = F, header = F)


x <- c('Id','name','asciiname','alternatenames','Lat','Lon','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')

names(CI) <- x
CI <- CI[ , c('Id', 'asciiname', 'Lat', 'Lon', 'feature_class')]
names(ML) <- x
ML <- ML[ , c('Id', 'asciiname', 'Lat', 'Lon', 'feature_class')]
names(BF) <- x
BF <- BF[ , c('Id', 'asciiname', 'Lat', 'Lon', 'feature_class')]
names(SN) <- x
SN <- SN[ , c('Id', 'asciiname', 'Lat', 'Lon', 'feature_class')]
names(GN) <- x
GN <- GN[ , c('Id', 'asciiname', 'Lat', 'Lon', 'feature_class')]
names(MR) <- x
MR <- MR[ , c('Id', 'asciiname', 'Lat', 'Lon', 'feature_class')]

vills <- bind_rows(CI, ML, BF, SN, GN, MR)

#Select villages, cities, towns, etc, leaving out universities, forests, etc
vills <- vills[vills$feature_class=='P', ]

vills <- vills[nchar(vills$asciiname) > 6, ]

coordinates(vills) <- c('Lon', 'Lat')
proj4string(vills) <- CRS("+proj=longlat +datum=WGS84")
vills <- spTransform(vills, CRS("+proj=aeqd +lat_0=0 +lon_0=-0"))

system.time({
  regdist <- as.matrix(dist(vills@coords))
})