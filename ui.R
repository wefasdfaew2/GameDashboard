library(shinydashboard)
library(shinyjs)
library(leaflet)

dashboardPage(skin = 'green',
  dashboardHeader(title = 'Game Dashboard'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Encounter', tabName = 'encounter', icon = icon('binoculars')),
      menuItem('Health', tabName = 'widgets', icon = icon('stethoscope')),
      menuItem('Maps', tabName = 'maps', icon = icon('globe')),
      menuItem('Figures', tabName = 'figures', icon = icon('area-chart')),
      menuItem('Data', tabName = 'data', icon = icon('database'))
    )
  ),
  
  dashboardBody(useShinyjs(),
    tabItems(
      tabItem(tabName = 'encounter',
          fluidRow(title = 'Get Data', width = 12,
              column(width = 3,
                     selectInput('slBiologist', 'Biologist', choices = '')),
              column(width = 3, 
                     selectInput('slLookup', 'Search By', selected = '',
                                 choices = c('Management Area', 'Hunt Unit', 'Mountain Range'))),
              column(width = 3,
                     selectizeInput('slLookupValue', '', choices = '', multiple = TRUE)),
              column(width = 3,
                     selectizeInput('slSpecies', 'Species', choices = '', multiple = TRUE))
              ),
          fluidRow(width = 12,
              column(width = 3, offset = 9, actionButton('abGetData', 'Get Data', width = '100%', icon = icon('cloud-download')))
              ),
        br(),
        
        fluidRow(
          box(title = 'Encounter Map', width = 12, height = '600px',
              leafletOutput('mpEncounter', height = '540px'))
          ),
        fluidRow(
          tabBox(title = 'Species Count', width = 8, height = 500, side = 'right',
            tabPanel('Species', 
                     plotOutput('plSpeciesBar', height = '430px')),
            tabPanel('Annual',
                     plotOutput('plSpeciesTS', height = '430px'))),
          box(title = 'Distribution Tables', width = 4, height = 500,
              DT::dataTableOutput('tbSppDist'))
        ),
        fluidRow(
          box(title = 'Recent Encounters', width = 12, 
                 DT::dataTableOutput('tbEncounter'))
                )
    )
  )
))