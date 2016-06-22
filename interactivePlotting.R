## get data from database
source('db_config.R')
dat <- tbl(src, 'data_Animal') %>% 
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>% 
  select(ndowID, Species, Sex, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit, EncounterID) %>% 
  filter(Species == 'MULD' & CapHuntUnit %in% c('101', '102', '103', '105', '78', '68', '192', '194', '291')) %>% 
  collect()
# type conversions
dat$CapDate <- as_date(dat$CapDate)
dat$CapYear <- year(dat$CapDate)
dat$capE <- as.numeric(dat$capE)
dat$capN <- as.numeric(dat$capN)
dat$Age[dat$Age == ''] <- NA
dat$Sex[dat$Sex == ''] <- NA

source('db_config.R')
biometric <- tbl(src, 'data_Biometric') %>% 
  filter(EncounterID %in% dat$EncounterID) %>% 
  collect() %>% 
  left_join(dat, by = c('EncounterID' = 'EncounterID')) %>% 
  select(ndowID, Sex, Age, Biometric, Measurement, Units, CapMtnRange, 
         CapHuntUnit, CapDate, CapYear, EncounterID)
spreadBiom <- biometric %>% 
  select(ndowID, EncounterID, Biometric, Measurement) %>% 
  tidyr::spread(Biometric, Measurement)
spreadBiom <- left_join(dat, spreadBiom, by = c('EncounterID' = 'EncounterID'))

ggplot(spreadBiom, aes(x = CapHuntUnit, y = Weight, fill = Sex)) +
  geom_boxplot() +
  facet_grid(~Age)


intPlot <- function(dat, xval, yval, colval, type, fillval = colval,
                    groupval = NULL, facetval = NULL) {
  gg <- ggplot(dat, aes_string(x = xval, y = yval))
  
  if (type == 'Scatter') {
    gg <- gg + geom_point(aes_string(color = colval)) +
      scale_color_gdocs()
  } else if (type == 'Box') {
    gg <- gg + geom_boxplot(aes_string(fill = fillval)) +
      scale_fill_gdocs()
  } else if (type == 'Bar') {
    gg <- gg + geom_bar(aes_string(color = NULL, fill = fillval)) +
      scale_fill_gdocs()
  } else if (type == 'Violin') {
    gg <- gg + geom_violin(aes_string(fill = fillval)) +
      scale_fill_gdocs()
  }
  
  gg <- gg + theme_bw()
  
  if (facetval != 'None') {
    gg <- gg + facet_wrap(facetval)
  }
  
  return(gg)
}

p <- intPlot(spreadBiom, 'ChestGirth', 'Weight', 'Sex', 'Scatter')
library(plotly)
ggplotly(p)

library(shiny)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('data', 'Data', selected = 'Biometric',
                  choices = c('Biometric', 'Vitals', 'WADDL')),
      selectInput('type', 'Type', selected = 'Box', 
                  choices = c('Bar', 'Box', 'Scatter', 'Density', 'Violin')),
      selectInput('x', 'X', selected = 'CapHuntUnit', 
                  choices = c('Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear',
                              'BCS', 'ChestGirth', 'HindLeg', 'Jaw', 'NeckSize', 'Weight')),
      selectInput('y', 'Y', selected = 'Weight',
                  choices = c('Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear',
                              'BCS', 'ChestGirth', 'HindLeg', 'Jaw', 'NeckSize', 'Weight')),
      selectInput('color', 'Color', selected = 'Sex',
                  choices = c('None', 'Sex', 'Age', 'CapMtnRange', 'CapHuntUnit')),
      selectInput('facet', 'Facet', selected = '',
                  choices = c('None', 'Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear'))
    ),
    mainPanel(plotOutput('plot'),
              textOutput('txt')))
  )

server <- function(input, output) {
  output$plot <- renderPlot({
    x <- input$x
    y <- input$y
    color <- switch (input$color,
      'None' = NULL,
      'Sex' = 'Sex',
      'Age' = 'Age',
      'CapMtnRange' = 'CapMtnRange',
      'CapHuntUnit' = 'CapHuntUnit'
    )
    dat <- spreadBiom
    type <- input$type
    facet <- input$facet
    
    intPlot(dat, x, y, color, type, facetval = facet)
  })
  output$txt <- renderText({
    paste(sep = ' | ',
          input$x,
          input$y,
          input$color,
          input$data, 
          input$type,
          input$facet)
  })
}

shiny::shinyApp(ui, server)

