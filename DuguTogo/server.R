library(maps)
library(mapdata)
library(stringr)
vills <- readRDS('data/vills.Rda')

villsearchpoints <- function(str,norm){
  map(database='world',regions=c('Senegal','Guinea-Bissau','Guinea','Liberia','Sierra Leone','Ivory Coast','Niger','Mauritania','Burkina Faso','Mali','Gambia','Ghana','Benin','Togo'),xlim=c(-18,16),ylim=c(4,27))
  if (norm==FALSE){
    points(vills$longitude[str_detect(vills$asciiname,str)],vills$latitude[str_detect(vills$asciiname,str)],pch=19,col="red",cex=.25)
  }
  if (norm==TRUE){
    points(vills$longitude[str_detect(vills$new,str)],vills$latitude[str_detect(vills$new,str)],pch=19,col="red",cex=.25)
  }
}

shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      villsearchpoints(toupper(input$str),input$norm)
    })
    
  }
)