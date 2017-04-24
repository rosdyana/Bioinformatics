library("e1071")
library("SparseM")
library("caret")
library("randomForest")


server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  filetraining <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.matrix.csr(infile$datapath)
  })
  
  # about section
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
  
  filetesting <- reactive({
    input$doRF
    infile <- input$file2
    if (is.null(infile)) {
      return(NULL)
    }
    read.matrix.csr(infile$datapath)
  })
  
  randomForestFunc <- function(train, test) {
    # withProgress(session, min=1, max=15, {
    #   setProgress(message = 'Calculation in progress',
    #               detail = 'This may take a while...')
    #   for (i in 1:15) {
    #     setProgress(value = i)
    #     Sys.sleep(0.5)
    #   }
    # })
    model <- svm(train$x, train$y)
    pred <- predict(model, test$x)
    # table(pred,test$y)
    
    model <-
      randomForest(as.matrix(train$x), factor(as.matrix(train$y)))
    pred <- predict(model, as.matrix(test$x))
    # table(pred,test$y)
    output$result1 <- renderPrint({
      confusionMatrix(pred, test$y)
    })
    output$result2 <- renderPrint({
      svm(train$x, train$y)
    })
  }
  
  output$uploadTesting <- renderUI({
    df <- filetraining()
    if (is.null(df))
      return(NULL)
    fileInput(
      'file2',
      'Choose Testing File',
      accept = c(
        'text/csv',
        'text/comma-separated-values,text/plain',
        '.csv',
        '.libsvm',
        '.arff'
      )
    )
  })
  
  output$runRF <- renderUI({
    df <- filetesting()
    if (is.null(df))
      return(NULL)
    actionButton("doRF", "Progress", icon = icon("microchip"))
  })
  
  observeEvent(input$doRF, {
    df <- filetesting()
    if (is.null(df))
      return(NULL)
    randomForestFunc(filetraining(), filetesting())
  })
  
}
