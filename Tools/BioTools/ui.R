library(shinydashboard)

header <-
  dashboardHeader(title = span(tagList(icon("code"), "Bio Tools")))

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Random Forest",
           tabName = "randomforesttab",
           icon = icon("tree")),
  menuItem("Boruta Feature Selection",
           tabName = "borutatab",
           icon = icon("optin-monster")),
  menuItem("Caret Feature Selection",
           tabName = "carettab",
           icon = icon("codiepie")),
  actionButton("about", "About", icon = icon("info-circle")),
  actionButton(
    "github",
    "Github",
    icon = icon("github"),
    onclick = "window.open('https://github.com/rosdyana/Bioinformatics', '_blank')"
  )
))

body <- dashboardBody(
  tags$head(
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
  tabItems(
  tabItem(tabName = "randomforesttab",
          fluidRow(
            column(
              width = 4,
              box(
                title =   tagList(shiny::icon("tree") , "Random Forest"),
                width = NULL,
                status = "primary",
                fileInput(
                  'file1',
                  'Choose training File',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values,text/plain',
                    '.csv',
                    '.libsvm'
                  )
                ),
                uiOutput("uploadTesting"),
                uiOutput("runRF")
              )
            ),
            column(
              width = 4,
              box(
                title = "",
                width = NULL,
                solidHeader = TRUE,
                status = "primary",
                verbatimTextOutput("result1")
              )
            ),
            column(
              width = 4,
              box(
                title = "",
                width = NULL,
                solidHeader = TRUE,
                status = "primary",
                verbatimTextOutput("result2")
              )
            )
          )),
  tabItem(tabName = "borutatab",
          fluidRow(
            column(
              width = 4,
              box(
                title =   tagList(shiny::icon("optin-monster") , "Boruta Feature Selection"),
                width = NULL,
                solidHeader = TRUE,
                status = "primary",
                fileInput(
                  'fileBrouta',
                  'Choose Dataset',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values,text/plain',
                    '.csv',
                    '.libsvm'
                  )
                ),
                uiOutput("runBoruta")
              )),
            column(
              width = 8,
              box(
                title =   tagList(shiny::icon("optin-monster") , ""),
                width = NULL,
                solidHeader = TRUE,
                status = "info",
                plotOutput("drawPlotBoruta"),
                verbatimTextOutput("resultBoruta")
              )))),
  tabItem(tabName = "carettab",
          fluidRow(
            column(
              width = 4,
              box(
                title =   tagList(shiny::icon("codiepie") , "Caret Feature Selection"),
                width = NULL,
                solidHeader = TRUE,
                status = "primary",
                selectInput(
                  "caretDataSelect",
                  "Select dataset",
                  list(
                    "My Data","Iris","Cars","Titanic"
                  )
              )))))
)
)

dashboardPage(header,
              sidebar,
              body)
