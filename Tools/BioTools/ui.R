library(shinydashboard)

header <-
  dashboardHeader(title = span(tagList(icon("code"), "Bio Tools")))

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
                    '.libsvm',
                    '.arff'
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
          ))
)

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Random Forest",
           tabName = "randomforesttab",
           icon = icon("tree")),
  actionButton("about", "About", icon = icon("info-circle")),
  actionButton(
    "github",
    "Github",
    icon = icon("github"),
    onclick = "window.open('https://github.com/rosdyana/Bioinformatics', '_blank')"
  )
))

dashboardPage(header,
              sidebar,
              body)
