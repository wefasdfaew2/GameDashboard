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
        fluidRow(width = 12, 
          box(title = 'Get Data', width = 3,  
              selectInput('slBiologist', 'Biologist', choices = ''),
              selectInput('slLookup', 'Search By', selected = 'Hunt Unit',
                          choices = c('Management Area', 'Hunt Unit', 'Mountain Range')),
              selectizeInput('slLookupValue', '', choices = '', multiple = TRUE),
              selectizeInput('slSpecies', 'Species', choices = '', multiple = TRUE),
              selectizeInput('slYear', 'Year', choices = '', multiple = TRUE),
              actionButton('abGetData', 'Get Data', width = '100%', icon = icon('cloud-download'))
              ),
          box(title = 'Encounter Map', width = 9, height = '600px',
              leafletOutput('mpEncounter', height = '540px')
              )
        ),
        fluidRow(
          tabBox(title = 'Species Count', width = 8, height = 500, side = 'right',
            tabPanel('Annual',
                     plotOutput('plSpeciesTS', height = '430px')),
            tabPanel('Species', 
                     plotOutput('plSpeciesBar', height = '430px'))),
          box(title = 'Distribution Tables', width = 4, height = 500,
              DT::dataTableOutput('tbSppDist'))
        ),
        fluidRow(
          box(title = 'Recent Encounters', width = 12, 
                 DT::dataTableOutput('tbEncounter'))
                )
    ),
    ##############
    # SURVEY TAB #
    ##############
    tabItem(tabName = 'survey',
        fluidRow(width = 12,
          box(title = 'Suvey Input', width = 3,
              selectInput('slSvyUnit', 'Survey Unit', selected = '', choices = ''),
              selectInput('slSvySpecies', 'Survey Species', selected = '', choices = ''),
              actionButton('abSurveyData', 'Get Survey Data', width = '100%')
              ),
          box(title = 'Survey Map', width = 9,
              leafletOutput('mpSurvey')
              )
          ),
        fluidRow(width = 12,
          tabBox(title = 'Survey Data', width = 12,
            tabPanel(title = 'Summary',
                     DT::dataTableOutput('tbSurvey')),
            tabPanel(title = 'Group',
                     DT::dataTableOutput('tbSurveyGroups'))
              )),
        fluidRow(width = 12,
          box(title = 'Survey Figure', width = 12,
              plotOutput('plSurvey'))
              )
          ),
    ###############
    # FIGURES TAB #
    ###############
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
    ############
    # DATA TAB #
    ############
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