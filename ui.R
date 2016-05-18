library(shinydashboard)

dashboardPage(skin = 'green',
  dashboardHeader(title = 'Game Dashboard'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Encounter', tabName = 'dashboard', icon = icon('binoculars')),
      menuItem('Health', tabName = 'widgets', icon = icon('stethoscope')),
      menuItem('Maps', tabName = 'maps', icon = icon('globe')),
      menuItem('Figures', tabName = 'figures', icon = icon('area-chart')),
      menuItem('Data', tabName = 'data', icon = icon('database'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard',
        fluidRow(
          box(title = 'EncounterMap', width = 8,
              leafletOutput('mpEncounter')),
          box(title = 'RecentEncounter', width = 4,
              DT::dataTableOutput('tbEncounter')
          )
        )
      ),
      
      tabItem(tabName = 'widgets',
              h2('Widgets tab content')
      )
    )
  )
)