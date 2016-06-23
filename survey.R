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
                           ADULT, JUVENILE, MALE, FEMALE, UNIT, YEAR)
map_coords <- xyConv(map_dat, xy = c('EASTING_X', 'NORTHING_Y'), '+init=epsg:26911', '+init=epsg:4326')



srvyid <- dat %>% select(SURVEYID) %>% magrittr::extract2(1) %>% unique() %>% sort()
vBios <- bios %>% select(Biologist) %>% extract2(1) %>%  unique() %>% sort()

tst <- map_coords %>% 
  filter(UNIT == 77 & SPECIES == 'MULD')
yrs <- tst %>% select(YEAR) %>% extract2(1) %>% unique() %>% sort()
pal <- colorFactor(ggthemes::gdocs_pal()(length(yrs)), yrs)
previewColors(pal, tst$YEAR)

leaflet() %>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lng = tst$x, lat = tst$y, 
                   stroke = F,
                   fillOpacity = .6,
                   radius = sqrt(tst$TOTAL) + 2,
                   color = pal(tst$YEAR)) %>% 
  addLegend('bottomright', pal, tst$YEAR)




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
      selectInput('slSurvey', 'Survey ID', selected = '', choices = srvyid),
      actionButton('acButton', 'Map')
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
  
  mapdat <- eventReactive(input$acButton, {
    map_coords %>% filter(UNIT == input$slUnit & SPECIES == input$slSpecies)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>%
      addCircleMarkers(lng = mapdat()$x, lat = mapdat()$y,
                       stroke = FALSE,
                       fillOpacity = .4,
                       radius = sqrt(mapdat()$TOTAL) + 2)
  })
  output$table <- DT::renderDataTable({
    smry <- mapdat() %>% group_by(YEAR) %>% 
      summarize(male = sum(MALE, na.rm = T),
                female = sum(FEMALE, na.rm = T),
                juvenile = sum(JUVENILE, na.rm = T),
                adult = sum(ADULT, na.rm = T),
                total = sum(TOTAL, na.rm = T),
                groups = n())
    datatable(smry, rownames = F, options = list(dom = 't'))
  })
}

shiny::shinyApp(ui = ui, server = server)

# SURVEY STANDARD ERROR
svy_sum <- tst %>% 
  group_by(YEAR) %>% 
  summarise(male = sum(MALE, na.rm = T),
            female = sum(FEMALE, na.rm = T),
            juvenile = sum(JUVENILE, na.rm = T),
            adult = sum(ADULT, na.rm = T),
            ttl = sum(JUVENILE + ADULT, na.rm = T),
            fsq = sum(JUVENILE**2, na.rm = T),
            t = sum(ADULT, na.rm = T),
            tsq = sum(ADULT**2, na.rm = T),
            fxt = sum(JUVENILE * ADULT, na.rm = T),
            pf = sum(JUVENILE, na.rm = T) / (sum(ADULT, na.rm = T) + sum(JUVENILE, na.rm = T)),
            n = n()) %>%  
  mutate(femjuv = juvenile + adult,
         pf1 = juvenile / (adult + juvenile),
         r = pf / (1 - pf),
         ttl1 = juvenile + adult,
         fsq1 = juvenile**2,
         tsq1 = adult**2,
         fxt1 = juvenile * adult
         )

(n*((fsq+(r^2*tsq))-(2*r*fxt))/(t^2*(n-1)))^0.5