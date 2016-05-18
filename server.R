library(shinydashboard)
library(leaflet)

server <- function(input, output) {

  output$mpEncounter <- renderLeaflet({
    leaflet() %>% addProviderTiles('Esri.WorldGrayCanvas') %>% 
      addMarkers(lng = -119.8219, lat = 39.5272)
  })
  
  output$tbEncounter <- DT::renderDataTable({
    datatable(head(animal))
  })
}
