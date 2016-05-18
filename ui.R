library(shinydashboard)
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
  dashboardBody(
    tabItems(
      tabItem(tabName = 'encounter',
        fluidRow(
          box(title = 'Encounter Map', width = 12,
              leafletOutput('mpEncounter'))
          ),
        fluidRow(
          box(title = 'Species Count', width = 9, height = 470,
              plotOutput('plSpeciesBar')),
          valueBox('5108', 'Encounter Records', icon = icon('table'), color = 'green', width = 3),
          valueBox('27559', 'Test Results', icon = icon('flask'), color = 'blue', width = 3),
          valueBox('2.2M', 'GPS locations', icon = icon('compass'), color = 'red', width = 3),
          valueBox('78', 'Species', icon = icon('paw'), color = 'purple', width = '3')
        ),
        fluidRow(
          box(title = 'Recent Encounters', width = 12, 
                 DT::dataTableOutput('tbEncounter'))
        )
      ),
      
      tabItem(tabName = 'widgets'
              
      )
    )
  )
)