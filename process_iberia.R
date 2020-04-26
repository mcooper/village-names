source('C://Users/matt/village-names/Utils.R')

setwd('G://My Drive/village-names/')

#################################
# Set up Parameters
#################################
proj <- '+proj=eqdc +lat_1=37.059693158597284 +lat_2=42.883590830197164 +lon_0=-3.1640625'


####################################
# Read in Data & spatialize
####################################
vills <- bind_rows(read_geonames_file('iberia/ES.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'ES')),
                   read_geonames_file('iberia/PT.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'PT'))) %>%
  filter(latitude > 35.9, longitude > -10) %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
  st_transform(proj)

#####################################
# Make Raster and summarize toponyms
#####################################
r <- make_raster(extent(vills), n=10000, proj)

vills$rcode <- extract(r, vills %>% st_coordinates() %>% SpatialPoints)

rgrams <- vills %>%
  st_drop_geometry() %>%
  group_by(rcode) %>%
  summarize(rtext = paste0(name, collapse=' '))

#################################
#Get up to 5000 tokens
#################################
grams <- tokenize_character_shingles(paste0(rgrams$rtext, collapse=' '), n=3, lowercase = F, strip_non_alphanum = F)[[1]]
grams <- grams[!grepl(" ", grams)]

t <- table(grams)

grams <- names(t)[order(t, decreasing = T)][1:5000]

######################################
# Make first occurence matrix (binmat)
######################################
binmat <- pbsapply(grams,grepl,rgrams$rtext,fixed=T)
row.names(binmat) <- rgrams$rcode

####################################################
# Make first distmat and conduct Morans I
####################################################
rpts <- rasterToPoints(r) %>%
  data.frame %>%
  filter(layer %in% rgrams$rcode)

distmat <- as.matrix(dist(rpts[ , c('x', 'y')]))

clustering <- run_moran(distmat, binmat)

#########################################################
# Get new spatially autocorrelated ngrams (p < 0.01)
#
# Calculate new binmat and distmat
###########################################################

newgrams <- clustering$gram[order(clustering$observed, decreasing = T)][1:1000]

binmat <- pbsapply(newgrams, grepl, rgrams$rtext, fixed=T)
row.names(binmat) <- rgrams$rcode

binmat <- binmat[rowSums(binmat) > 0, ]

rpts <- rasterToPoints(r) %>%
  data.frame %>%
  filter(layer %in% row.names(binmat))

distmat <- as.matrix(dist(rpts[ , c('x', 'y')]))

rownames(distmat) <- rpts$layer
colnames(distmat) <- rpts$layer

mn <- min(distmat[distmat != 0]) + 0.1 #add a bit for floating point issues
distmat_r <- distmat <= mn

mn2 <- min(distmat[distmat > mn]) + 5
distmat_q <- distmat <= mn2

writeRaster(r, 'iberia/reference_grid.tif', format='GTiff', overwrite=T)
write.csv(binmat, 'iberia/occurence_mat.csv')
write.csv(distmat, 'iberia/distmat.csv')
write.csv(distmat_r, 'iberia/distmat_r.csv')
write.csv(distmat_q, 'iberia/distmat_q.csv')


