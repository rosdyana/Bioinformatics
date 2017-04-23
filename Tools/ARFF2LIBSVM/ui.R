library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = span(tagList(
    icon("spinner"), "ARFF2LIBSVM"
  )),
  titleWidth = 250),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Tools",
      tabName = "",
      icon = icon("cogs"),
      menuSubItem(
        'ARFF 2 LIBSVM',
        tabName = 'arff2libsvm',
        icon = icon('forward')
      ),
      menuSubItem(
        'LIBSVM 2 ARFF',
        tabName = 'libsvm2arff',
        icon = icon('backward')
      ),
      menuSubItem(
        'LIBSVM READER',
        tabName = 'libsvmreader',
        icon = icon('eye')
      )
    ),
    actionButton("about", "About", icon = icon("info-circle")),
    actionButton(
      "github",
      "Github",
      icon = icon("github"),
      onclick = "window.open('https://github.com/rosdyana/Bioinformatics', '_blank')"
    )
  )),
  dashboardBody(
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
      # First tab content
      tabItem(tabName = "arff2libsvm",
              fluidPage(
                box(
                  title =   tagList(shiny::icon("forward") , "ARFF 2 LIBSVM"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE
                )
              )),
      tabItem(tabName = "libsvm2arff",
              fluidPage(
                box(
                  title =   tagList(shiny::icon("backward") , "LIBSVM 2 ARFF"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE
                )
              )),
      tabItem(tabName = "libsvmreader",
              fluidPage(
                box(
                  title =   tagList(shiny::icon("eye") , "LIBSVM READER"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE
                )
              ))
      )
  )
)