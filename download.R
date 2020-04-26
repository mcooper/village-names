library(countrycode)

setwd('G://My Drive/village-names/wafrica/')

iso2 <- countrycode(c('Mali', 'Mauritania', 'Senegal', 
                    'Gambia', 'Guinea-Bissau', 'Guinea',
                    'Sierra Leone', 'Liberia', 'Ivory Coast', 
                    'Ghana', 'Burkina Faso', 'Niger', 'Nigeria', 
                    'Togo', 'Benin', 'Chad', 'Cameroon', 
                    'Central African Republic'),
                    'country.name', 'iso2c')

for (i in iso2){
  url <- paste0("http://download.geonames.org/export/dump/", i, ".zip")
  
  download.file(url, paste0(i, '.zip'))
}