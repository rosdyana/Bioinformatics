library(shinydashboard)

header <- dashboardHeader(
  title = span(tagList(
    icon("tree"), "RandomForest"
  ))
)

body <- dashboardBody(   tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Oregano")
),
tags$head(tags$style(
  HTML(
    '
        .main-header .logo {
        font-family: "Oregano", cursive;
        font-weight: bold;
        font-size: 24px;
        }
        '
  )
)),
tabItem(tabName = "randomforesttab",
        fluidPage(
          box(
            title =   tagList(shiny::icon("tree") , "Random Forest"),
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            fileInput('file1', 'Choose training File',
                     accept=c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.csv','.libsvm','.arff')),
            uiOutput("uploadTesting"),
            uiOutput("runRF"),
            tags$hr(),
            verbatimTextOutput("result1"),
            tags$hr(),
            verbatimTextOutput("result2")
          )
        ))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Random Forest",
      tabName = "randomforesttab",
      icon = icon("tree")
    ),
    actionButton("about", "About", icon = icon("info-circle")),
    actionButton(
      "github",
      "Github",
      icon = icon("github"),
      onclick = "window.open('https://github.com/rosdyana/Bioinformatics', '_blank')"
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)
