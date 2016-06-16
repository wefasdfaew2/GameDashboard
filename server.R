library(shinydashboard)
library(shinyjs)
library(magrittr)
library(leaflet)
library(DT)
library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(sp)
library(RSQLServer)
library(DBI)
source('db_config.R')

xyConv <- function(df, xy = c('long_x', 'lat_y'), CRSin = '+proj=longlat',
                   CRSout = '+proj=utm +zone=11') {
  df <- df[complete.cases(df[, xy]), ]
  coord <- data.frame(df[, xy])
  colnames(coord) <- c('x', 'y') 
  coord[, 1] <- as.numeric(coord[, 1])
  coord[, 2] <- as.numeric(coord[, 2])
  conv <- SpatialPoints(coordinates(coord),
                        proj4string = CRS(CRSin))
  conv <- spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  return(df)
}
colPalette <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6", 
                "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99", 
                "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262", 
                "#5574A6", "#3B3EAC")

server <- function(input, output, session) {
#####################
# PAGE 1: ENCOUNTER #
#####################
  ## encounter data
  dat <- eventReactive(input$abGetData, {
    dat <- tbl(src, 'data_Capture') %>% 
       filter(CapHuntUnit == input$slHuntUnit) %>% 
       select(EncounterID, AnimalKey, CapDate, Status, Age, 
              capE, capN, CapMtnRange, CapHuntUnit, CapLocDesc) %>% 
       inner_join(tbl(src, 'data_Animal')) %>% 
       collect()
    return(dat)
  })
  
  ## species vector for selected data
  vSpecies <- eventReactive(input$abGetData, {
    dat() %>% select(Species) %>% extract2(1) %>% unique() %>% sort()
  })
  observeEvent(input$abGetData, {
    updateSelectInput(session, 'slSpecies_map', choices = c('All', vSpecies()), selected = 'All')
  })
  
  ## encounter map
  output$mpEncounter <- renderLeaflet({
    dat <- xyConv(dat(), c('capE', 'capN'), '+init=epsg:26911', '+init=epsg:4326')
    
    if (input$slSpecies_map != 'All') {
      dat <- filter(dat, Species == input$slSpecies_map)
    }
    
    leaflet() %>%
      addProviderTiles('Esri.WorldTopoMap',
                       options = providerTileOptions(attribution = NA)) %>%
      addCircleMarkers(lng = dat$x, lat = dat$y, stroke = FALSE, radius = 4)
  })
  
  ## encounter summary table
  output$tbEncounter <- DT::renderDataTable({
    datatable(head(dat()[, c('Species', 'ndowID', 'Sex', 'Status', 'CapDate', 'CapMtnRange', 'CapHuntUnit')], 50))
  })
  
  ## species distribution for selected input
  output$plSpeciesBar <- renderPlot({
    dat() %>% group_by(Species) %>%
      summarize(Total = n()) %>%
      ggplot(aes(x = Species, y = Total)) +
      geom_bar(stat = 'identity', color = 'royalblue', fill = 'royalblue') +
      theme_bw()
  })
  
  ## species time series for selected input
  output$plSpeciesTS <- renderPlot({
    dat <- dat() %>% 
      group_by(Species, Year = year(as_date(CapDate))) %>% 
      summarize(Total = n())
    ggplot(dat, aes(x = Year, y = Total, group = Species, fill = Species)) +
      geom_bar(stat = 'identity') +
      ggthemes::scale_fill_gdocs() +
      scale_x_continuous(breaks = seq(min(dat$Year), max(dat$Year), 1),
                         labels = seq(min(dat$Year), max(dat$Year), 1)) +
      theme_bw()
  })
  ## species distribution table
  output$tbSppDist <- DT::renderDataTable({
    dat <- dat() %>% group_by(Species, Sex) %>% 
      summarize(Total = n()) %>% 
      arrange(Species, Sex)
    dat$Sex[dat$Sex == ''] <- 'Unk'
    datatable(dat, rownames = F, options = list(dom = 't'))
      
  })
}
