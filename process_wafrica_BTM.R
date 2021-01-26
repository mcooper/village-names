source('~/village-names/Utils.R')

setwd('~/gd/village-names/wafrica')

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
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326, remove=F) %>%
  st_transform(proj)

vills <- bind_cols(vills %>% st_drop_geometry,
                   vills %>% st_coordinates %>% data.frame)

#################################
#Get tokens
#################################
grams <- tokenize_character_shingles(vills$name, n=3, lowercase = F, strip_non_alphanum = F)
grams <- lapply(grams, FUN=function(x) x[!grepl(" |\\-|\\.", x)])

t <- table(unlist(grams))

grams <- names(t)[t > 10]

##############################
# Run Moran's I on every gram
###############################
library(doParallel)

cl <- makeCluster(32, outfile = '')
registerDoParallel(cl)

all <- foreach(g=grams, .packages=c('moranfast'), .combine=bind_rows) %dopar% {
  cat(round(which(grams == g)/length(grams)*100, 4), 'percent done\n') 
  res <- moranfast(grepl(g, vills$name), vills$X, vills$Y)
  res$gram <- g
  write.csv(res, paste0('~/gd/village-names/wafrica/moran/', 
                        paste0(sample(c(LETTERS, letters, 0:9), 8), collapse=''), 'XX'), 
            row.names=F)
  res
}
write.csv(res, '~/gd/village-names/wafrica/final_moran.csv', row.names=F)

system('~/telegram.sh "Done with moran processing"')
system('sudo poweroff')

###################################
# Read in results
###################################

moran <- list.files('~/gd/village-names/wafrica/moran', full.names=T) %>%
  lapply(function(x){read.csv(x, colClasses=c("numeric", "numeric", "numeric","numeric","character"))}) %>%
  bind_rows

gramdf <- data.frame(gram=names(t), count=as.vector(t))

moran <- merge(moran, gramdf, all.x=T, all.y=F)

write.csv(moran, 'moran_results.csv', row.names=F)
write.csv(vills, 'villages.csv', row.names=F)



