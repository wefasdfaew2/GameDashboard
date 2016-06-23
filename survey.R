dets <- read_csv('data/tbl_survey_details.txt')
srvy <- read_csv('data/tbl_survey_comp.txt')
idaho <- filter(srvy, UNIT == '061ID')






srvy_coords <- xyConv(srvy, xy = c('EASTING_X', 'NORTHING_Y'), '+init=epsg:26911', '+init=epsg:4326')

mu13 <- srvy %>% 
  filter(SURVEYID == 'MU013_20140909')






leaflet() %>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lng = mu13$x, lat = mu13$y, 
                   stroke = F,
                   fillOpacity = .4,
                   radius = mu13$TOTAL)
pal <- leaflet::colorQuantile(viridis::viridis(), domain = mu13$TOTAL)
previewColors(pal, mu13$TOTAL)







ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('slYear', 'Year', selected = '', choices = ''),
      selectInput('slUnit', 'Unit', selected = '', choices = ''),
      selectInput('slSpecies', 'Species', selected = '', choices = '')
    ),
    mainPanel(
      leaflet::leafletOutput('map')
      DT::datatable('table')
      plotOutput('plot')
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles('Esri.WorldTopoMap')
  })
  output$table <- DT::renderDataTable({
    
  })
}