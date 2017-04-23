server <- function(input, output) {
  observeEvent(input$about, {
    showModal(modalDialog(
      title = span(tagList(icon("info-circle"), "About")),
      tags$div(
        HTML(
          "<img src='https://avatars1.githubusercontent.com/u/4516635?v=3&s=460' width=150><br/><br/>",
          "<p>Developer : Rosdyana Kusuma</br>Email : <a href=mailto:rosdyana.kusuma@gmail.com>rosdyana.kusuma@gmail.com</a></br>linkedin : <a href='https://www.linkedin.com/in/rosdyanakusuma/' target=blank>Open me</a></p>"
        )
      ),
      easyClose = TRUE
    ))
  })
}