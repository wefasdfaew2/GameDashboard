library(shinydashboard)
library(shinyjs)
library(leaflet)

dashboardPage(skin = 'green',
  dashboardHeader(title = 'Game Dashboard'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Encounter', tabName = 'encounter', icon = icon('binoculars')),
      menuItem('Health', tabName = 'widgets', icon = icon('stethoscope')),
      menuItem('Survey', tabName = 'survey', icon = icon('globe')),
      menuItem('Figures', tabName = 'figures', icon = icon('area-chart')),
      menuItem('Data', tabName = 'data', icon = icon('database'))
    )
  ),
  
  dashboardBody(useShinyjs(),
    tabItems(
      tabItem(tabName = 'encounter',
          div(id = 'getdata', class = 'whitebg',
          fluidRow(title = 'Get Data', width = 12,
              column(width = 3,
                     selectInput('slBiologist', 'Biologist', choices = '')),
              column(width = 3, 
                     selectInput('slLookup', 'Search By', selected = 'Hunt Unit',
                                 choices = c('Management Area', 'Hunt Unit', 'Mountain Range'))),
              column(width = 3,
                     selectizeInput('slLookupValue', '', choices = '', multiple = TRUE)),
              column(width = 3,
                     selectizeInput('slSpecies', 'Species', choices = '', multiple = TRUE))
              ),
          fluidRow(width = 12,
              column(width = 3, offset = 9, 
                     actionButton('abGetData', 'Get Data', icon = icon('cloud-download')))
              )),
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
    ),
    tabItem(tabName = 'survey',
            fluidRow(title = 'Survey Input', width = 12,
                column(width = 4,
                       selectInput('slSvyUnit', 'Survey Unit', selected = '',
                                   choices = 13:20)),
                column(width = 4,
                       selectInput('slSvySpecies', 'Survey Species', selected = '',
                                   choices = c('DBHS', 'MULD', 'RMEL')))
                )),
    tabItem(tabName = 'figures',
        fluidRow(title = 'Figure Input', width = 12,
            column(width = 4,
                   selectInput('slData', 'Data', selected = 'Biometric',
                               choices = c('Biometric')),
                   selectInput('slPlotType', 'Type', selected = 'Box', 
                               choices = c('Bar', 'Box', 'Scatter', 'Density', 'Violin'))
            ),
            column(width = 4,
                   selectInput('slXaxis', 'X', selected = 'CapHuntUnit', 
                               choices = c('Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear',
                                           'BCS', 'ChestGirth', 'HindLeg', 'Jaw', 'NeckSize', 'Weight')),
                   selectInput('slYaxis', 'Y', selected = 'Weight',
                               choices = c('Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear',
                                           'BCS', 'ChestGirth', 'HindLeg', 'Jaw', 'NeckSize', 'Weight'))
            ),
            column(width = 4,
                   selectInput('slColor', 'Color', selected = 'Sex',
                               choices = c('None', 'Sex', 'Age', 'CapMtnRange', 'CapHuntUnit')),
                   selectInput('slFacet', 'Facet', selected = '',
                               choices = c('None', 'Sex', 'Age', 'CapMtnRange', 'CapHuntUnit', 'CapYear')),
                   actionButton('abCreatePlot', 'Create Plot', icon = icon('cogs'))
            )
            ),
        fluidRow(width = 12,
                 plotOutput('plFigure'))
        ),
    tabItem(tabName = 'data',
        tabBox(title = 'Encounter Summary', width = 12,
          tabPanel('Table', 
                   DT::dataTableOutput('tbEncSummary', width = 'auto')),
          tabPanel('Summary', 
                   htmlOutput('htmlEncSummary'))
                  ),
        tabBox(title = 'Biometric Summary', width = 12,
          tabPanel('Table', 
                   DT::dataTableOutput('tbBioSummary')),
          tabPanel('Summary', 
                   htmlOutput('htmlBioSummary'),
                   DT::dataTableOutput('tbBioNumSum'))
                  )
           )
  )
))