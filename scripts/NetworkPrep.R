setwd("/Users/matthewcooper/R workspace")

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

###################
###Read in Vills###
###################

##Read in text files for all countries
BF <- read.delim('BF.txt', header=F)
CI <- read.delim('CI.txt', header=F)
GN <- read.delim('GN.txt', header=F)
GW <- read.delim('GW.txt', header=F)
LR <- read.delim('LR.txt', header=F)
MR <- read.delim('MR.txt', header=F)
NE <- read.delim('NE.txt', header=F)
SL <- read.delim('SL.txt', header=F)
SN <- read.delim('SN.txt', header=F)
ML <- read.delim('ML.txt', header=F)
NG <- read.delim('NG.txt', header=F)
GM <- read.delim('GM.txt', header=F)
TG <- read.delim('TG.txt', header=F)
BJ <- read.delim('BJ.txt', header=F)
GH <- read.delim('GH.txt', header=F)

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
new <- read.delim('fixvills.txt', header=FALSE, sep='$')
#Because I added $$$ to them in python as a delimitter, only the first column is added to vills
names(new) <- c('a','b')
vills$standname <- new$a

#convert all toponyms to one case
#vills$asciiname <- toupper(vills$asciiname)

#Select unique names
#nameselect <- vills[,c(3,20)]
#unique <- duplicated(nameselect)
#vills <- vills[!unique,c(3,5,6,20)]

coordselect <- vills[,2:3]
unique <- duplicated(coordselect)
vills <- vills[!unique,]

#map population density
#can also sumbmit query '' to one of the villsearch functions
"m <- matrix(c(vills$longitude,vills$latitude),ncol=2)
r <- raster(ncols=34*5, nrows=19*5, xmn=-18,xmx=16,ymn=4,ymx=27)
popdensity <- rasterize(m, r, fun=function(x,...)length(x))
plot(popdensity)
map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27), add=TRUE)
"

####################
###Create Network###
####################

load('AggregateExplorer/data/binmat.Rdata')

matchstrs <- names(binmat)[2:6380]

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

netbinmat <- binmat(as.character(vills$standname),matchstrs)





