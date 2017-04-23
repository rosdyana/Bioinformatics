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
  dashboardBody()
)
