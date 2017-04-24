server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)  
  filetraining <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.matrix.csr(infile$datapath)
  })
  
  filetesting <- reactive({
    infile <- input$file2
    if (is.null(infile)) {
      return(NULL)
    }
    read.matrix.csr(infile$datapath)
  })
  
  randomForestFunc <- function(train, test){
    model <- svm(train$x, train$y)
    pred <- predict(model,test$x)
    # table(pred,test$y)
    
    model <- randomForest(as.matrix(train$x), factor(as.matrix(train$y)))
    pred <- predict(model,as.matrix(test$x))
    # table(pred,test$y)
    output$result1 <- renderPrint({
      confusionMatrix(pred,test$y)
    })
    output$result2 <- renderPrint({
      svm(train$x, x$y)
    })
  }
  
  output$uploadTesting <- renderUI({
    df <- filetraining()
    if (is.null(df))
      return(NULL)
    fileInput('file2', 'Choose Testing File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv','.libsvm','.arff'))
  })
  
  output$runRF <- renderUI({
    df <- filetesting()
    if (is.null(df))
      return(NULL)
    actionButton("doRF","Progress", icon = icon("microchip"))
  })
  
  observeEvent(input$doRF, {
    df <- filetesting()
    if (is.null(df))
      return(NULL)
    randomForestFunc(filetraining(),filetesting())  })
  
}
