library(dplyr)
library(shiny)
library(magrittr)
dets <- read_csv('data/tbl_survey_details.txt')
srvy <- read_csv('data/tbl_survey_comp.txt')
idaho <- filter(dets, UNIT %in% c('061ID', '066ID'))
dets <- filter(dets, !(UNIT %in% c('061ID', '066ID')))  ## remove idaho

dat <- left_join(srvy, dets[, 1:5], by = ('SURVEYID' = 'SURVEYID'))
dat$TIME <- strftime(lubridate::mdy_hms(dat$TIME), format = '%H:%M:%S')
dat$UNIT <- as.numeric(dat$UNIT)
dat$SURVEYDATE <- as_date(mdy_hms(dat$SURVEYDATE))
dat$YEAR <- year(dat$SURVEYDATE)

map_dat <- dat %>%  select(SURVEYID, SURVEYDATE, TIME, EASTING_X, NORTHING_Y, SPECIES, TOTAL,
                           ADULT, JUVENILE, MALE, FEMALE, UNIT)
map_coords <- xyConv(map_dat, xy = c('EASTING_X', 'NORTHING_Y'), '+init=epsg:26911', '+init=epsg:4326')



srvyid <- dat %>% select(SURVEYID) %>% magrittr::extract2(1) %>% unique() %>% sort()
units <- dat %>% select(UNIT) %>% extract2(1) %>% unique() %>% sort()
species <- dat %>% select(UNIT) %>% extract2(1) %>% unique() %>% sort()
yrs <- dat %>% select(YEAR) %>% extract2(1) %>% unique() %>% sort()
vBios <- bios %>% select(Biologist) %>% extract2(1) %>%  unique() %>% sort()

tst <- map_coords %>% 
  filter(UNIT == 164 & SPECIES == 'DBHS')


leaflet() %>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lng = mu13$x, lat = mu13$y, 
                   stroke = F,
                   fillOpacity = .4,
                   radius = mu13$TOTAL)
pal <- leaflet::colorQuantile(viridis::viridis(), domain = mu13$TOTAL)
previewColors(pal, mu13$TOTAL)



## dynamic filter survey data by biologist area of responsibility
resp <- bios %>% filter(Biologist == 'Joe Bennett')
vUnit <- resp %>% select(HuntUnit) %>% extract2(1) %>% unique() %>% sort()
vSp <- resp %>% select(Species) %>% extract2(1) %>% unique() %>% sort()
### filtering survey data by Joes area and species of responsibility
srvy_resp <- dat %>% 
  filter(UNIT %in% vUnit & 
         SPECIES %in% vSp)
### get lookups for hunt units and species
vnUnit <- srvy_resp %>% select(UNIT) %>% extract2(1) %>% unique() %>% sort()
vnSp <- srvy_resp %>% select(SPECIES) %>% extract2(1) %>% unique() %>% sort()

lkp <- list(unit = vUnit, species = vSp)
lkp$unit

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('slBios', 'Biologist', selected = '', choices = vBios),
      selectInput('slUnit', 'Unit', selected = '', choices = ''),
      selectInput('slSpecies', 'Species', selected = '', choices = ''),
      selectInput('slYear', 'Year', selected = '', choices = ''),
      selectInput('slSurvey', 'Survey ID', selected = '', choices = srvyid)
    ),
    mainPanel(
      leaflet::leafletOutput('map'),
      DT::dataTableOutput('table'),
      plotOutput('plot')
    )
  )
)

server <- function(input, output, session) {
  lkp <- eventReactive(input$slBios, {
    resp <- bios %>% filter(Biologist == input$slBios)
    vUnit <- resp %>% select(HuntUnit) %>% extract2(1) %>% unique() %>% sort()
    vSp <- resp %>% select(Species) %>% extract2(1) %>% unique() %>% sort()
    srvy_resp <- dat %>% 
      filter(UNIT %in% vUnit & 
               SPECIES %in% vSp)
    ### get lookups for hunt units and species
    vUnit <- srvy_resp %>% select(UNIT) %>% extract2(1) %>% unique() %>% sort()
    vSp <- srvy_resp %>% select(SPECIES) %>% extract2(1) %>% unique() %>% sort()
    return(list(unit = vUnit, species = vSp))
  })
  
  observeEvent(input$slBios, {
    updateSelectInput(session, 'slUnit', choices = lkp()$unit, selected = '')
    updateSelectInput(session, 'slSpecies', choices = lkp()$species, selected = '')
  })
  
  output$map <- renderLeaflet({
    srv_map <- map_coords %>% filter(UNIT == input$slUnit & SPECIES == input$slSpecies)
    leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>%
      addCircleMarkers(lng = srv_map$x, lat = srv_map$y,
                       stroke = FALSE,
                       fillOpacity = .4,
                       radius = sqrt(srv_map$TOTAL) + 2)
  })
  output$table <- DT::renderDataTable({
    srv_map <- map_coords %>% filter(UNIT == input$slUnit & SPECIES == input$slSpecies)
    datatable(srv_map, rownames = F, options = list(dom = 't'))
  })
}

shiny::shinyApp(ui = ui, server = server)
