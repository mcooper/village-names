ES <- read.delim('D://Documents and Settings/mcooper/Desktop/ES/ES.txt', header=F, stringsAsFactors = F, encoding="UTF-8")
PT <- read.delim('D://Documents and Settings/mcooper/Desktop/PT/PT.txt', header=F, stringsAsFactors = F, encoding="UTF-8")

#Rename all the coulumns
x <- c('geonameid','name','asciiname','alternatenames','latitude','longitude','feature_class','feature_code','country_code','cc2','admin1_code','admin2_code','admin3_code','admin4_code','population','elevation','dem','timezone','modification date')

names(ES) <- x
names(PT) <- x

all <- rbind(ES, PT)

write.csv(all[all$feature_class=='P', c('name', 'latitude', 'longitude')], 'D://Documents and Settings/mcooper/Google Drive/Creativitea/IberiaRaw.csv',
          row.names=F)

all$name <- all$asciiname

write.csv(all[all$feature_class=='P', c('name', 'latitude', 'longitude')], 'D://Documents and Settings/mcooper/Google Drive/Creativitea/IberiaAscii.csv',
          row.names=F)
