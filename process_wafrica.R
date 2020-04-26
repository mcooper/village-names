source('C://Users/matt/village-names/Utils.R')

setwd('G://My Drive/village-names/wafrica')

#################################
# Set up Parameters
#################################
proj <- '+proj=eqc +lon_0=2.4609374999999982'

####################################
# Read in Data & spatialize
####################################
vills <- bind_rows(read_geonames_file('BF.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('BJ.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('CF.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('CI.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('CM.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('GH.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'EN')),
                   read_geonames_file('GM.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'EN')),
                   read_geonames_file('GN.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('GW.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'PT')),
                   read_geonames_file('LR.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'EN')),
                   read_geonames_file('ML.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('MR.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('NE.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('NG.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'EN')),
                   read_geonames_file('SL.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'EN')),
                   read_geonames_file('SN.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('TD.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR')),
                   read_geonames_file('TG.txt') %>%
                     convert_ipa(read_xlsx(IPA, sheet = 'FR'))) %>%
  filter(latitude > 4.146543, 
         latitude < 17.138959, 
         longitude > -17.696228, 
         longitude < 18.544922) %>%
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
binmat <- pbsapply(grams,grepl,rgrams$rtext)
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

#plot(r %in% row.names(binmat)[binmat[ , 'akt']])

binmat <- pbsapply(newgrams, grepl, rgrams$rtext)
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

writeRaster(r, 'reference_grid.tif', format='GTiff', overwrite=T)
write.csv(binmat, 'occurence_mat.csv')
write.csv(distmat, 'distmat.csv')
write.csv(distmat_r, 'distmat_r.csv')
write.csv(distmat_q, 'distmat_q.csv')


