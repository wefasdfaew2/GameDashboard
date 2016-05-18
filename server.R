library(shinydashboard)
library(leaflet)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(sp)

animal <- read_csv('data/animal.csv')
capture <- read_csv('data/capture.csv')

mapdf <- as.data.frame(head(capture, 500))

xyConv <- function(df, xy = c('long_x', 'lat_y'), CRSin = '+proj=longlat',
                   CRSout = '+proj=utm +zone=11', outclass = 'data.frame') {
  
  df <- df[complete.cases(df[, xy]), ]
  conv <- SpatialPoints(cbind('x' = as.numeric(df[, xy[1]]),
                              'y' = as.numeric(df[, xy[2]])),
                        proj4string = CRS(CRSin))
  conv <- spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  df <- cbind(df, conv)
  
  if (outclass == 'spdf') {
    df <- SpatialPointsDataFrame(conv, df, proj4string = CRS(CRSout))
  }
  return(df)
}

mapdf <- xyConv(mapdf, xy = c('capE', 'capN'), 
             CRSin = '+init=epsg:26911', CRSout = '+init=epsg:4326',
             outclass = 'spdf')

server <- function(input, output) {

  output$mpEncounter <- renderLeaflet({
    leaflet(mapdf) %>% 
      addProviderTiles('Esri.WorldGrayCanvas', 
                       options = providerTileOptions(attribution = NA)) %>% 
      addCircleMarkers(stroke = FALSE, radius = 4)
  })
  
  output$tbEncounter <- DT::renderDataTable({
    datatable(head(animal, 50))
  })
  
  output$plSpeciesBar <- renderPlot({
    animal %>% group_by(Species) %>% 
      summarize(Total = n()) %>% 
      filter(Total >= 100) %>% 
      ggplot(aes(x = Species, y = Total)) +
      geom_bar(stat = 'identity', color = 'royalblue', fill = 'royalblue') +
      theme_bw()
  })
}
