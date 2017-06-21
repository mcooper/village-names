suppressMessages(suppressWarnings(library(dplyr, quietly=T, verbose=F)))
suppressMessages(suppressWarnings(library(rgdal, quietly=T, verbose=F)))
suppressMessages(suppressWarnings(library(optparse, quietly=T, verbose=F)))
suppressMessages(suppressWarnings(library(igraph, quietly=T, verbose=F)))

option_list = list(
  make_option(c("-f", "--file"), 
              type="character", 
              default=NULL, 
              help="dataset file name.  Dataset must have column names latitude, longitude, and name", 
              metavar="character"),
  make_option(c("-o", "--out"), 
              type="character", 
              default="out.csv", 
              help="output file name [default= %default]", 
              metavar="character"),
  make_option(c("-m", "--map"), 
              type="character", 
              default=NULL,
              help="the filename of a map to be made", 
              metavar="characer"),
  make_option(c("-l", "--length"), 
              type="integer", 
              default=6,
              help="the minimum toponym length to include", 
              metavar="integer"),
  make_option(c("-c", "--capitalize-ends"), 
              type="logical", 
              default=TRUE,
              help="should beginning and end of toponyms be capitalized", 
              metavar="logical"),
  make_option(c("-g", "--gram-size"), 
              type="integer", 
              default=3,
              help="gram size", 
              metavar="integer"),
  make_option(c("-b", "--buffer"),
              type="integer",
              default=10000,
              help="distance between villages (in meters) to include in graph",
              metavar="integer"),
  make_option(c("j", "--jaccard"),
              type="double",
              default=0.75,
              help="the cutoff in determining of two toponyms share a lexical affiliation.  See ?dist and read the docs where method='binary'",
              metavar='double')
)
##To do:
# Add mapping output option
# Do recursive cluster detection

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

options(stringsAsFactors = F)

##################
### Read in Data & Prep names
##################
cat("\nRead in Data & Prep names\n\n")

vills <- read.csv(opt[['file']])
vills$id <- 1:nrow(vills)

vills <- vills[nchar(vills$name) > opt[['length']], ]

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

vills$name <- tolower(vills$name)

vills$name <- gsub("[^[:lower:]]", "", vills$name)

if (opt[["capitalize-ends"]]){
  vills$name <- sapply(vills$name, makeLastUpper)
}

################
###Get N-grams
###############
cat("\nGet N-Grams\n\n")

getNGrams <- function(str, n){
  len <- nchar(str)
  mapply(substr, start=1:(len-(n-1)), stop=n:len, x=str)
}

nGrams <- sapply(vills$name, getNGrams, n=10) %>% 
  unlist %>% 
  unique

######################
###Get Binary Matrix
######################
cat("\nGet Binary Matrix\n\n")

binmat <- sapply(nGrams,grepl,vills$name)

row.names(binmat) <- vills$id

#######################
###Select only Variables with significant spatial clustering
#######################
cat("\nSelect Clustered N-Grams\n\n")

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
###Converting to graph 
#####################################
cat("\nConvert to Graph\n\n")

distmat_space <- as.matrix(dist(vills@coords)) < opt[['buffer']]
distmat_lang <- dist(binmatsel, method='binary')
adjmat <- as.matrix(distmat_lang) < opt[['jaccard']]
diag(adjmat) <- FALSE

distmat <- distmat_space | adjmat

g  <- graph.adjacency(adjmat)

#first find isolated communities
cat("\nFind Isolated Communities\n\n")
dg <- decompose.graph(g)

g1 <- dg[[1]]

#Then find communities
cat("\nFind All Communities\n\n")
fc <- fastgreedy.community(as.undirected(g1))

df1 <- data.frame(name=fc$name, group=fc$membership)
mg1 <- merge(df1, vills, by='id') %>% unique

write.csv(mg1, opt[['out']], row.names = F)

cat("Donezo!")