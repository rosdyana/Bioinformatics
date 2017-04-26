library("e1071")
library("SparseM")
library("caret")
library("randomForest")
library("data.table")
library("Boruta")


server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  
  filetraining <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.matrix.csr(infile$datapath)
  })
  
  filetesting <- reactive({
    input$doRF
    infile <- input$file2
    if (is.null(infile)) {
      return(NULL)
    }
    read.matrix.csr(infile$datapath)
  })
  
  # random forest
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
  Read.LibSVM.File<- function(LibSVMFileName) {
    x <-read.matrix.csr(LibSVMFileName) # From library("e1071") package
    #Get Real Data
    Data = as.data.frame(as.matrix(x$x))
    LibSVM.Label.Class = x$y
    #Add Class to Data
    Data$class = LibSVM.Label.Class;
    
    return (Data);
  }
  # boruta
  m_fileBoruta <- reactive({
    infile <- input$fileBrouta
    if (is.null(infile)) {
      return(NULL)
    }
    Read.LibSVM.File(infile$datapath)
  })
  borutaFunc <- function(x){
     # Load the dataset 
    DATASET <- x
    
    #is.data.frame(DATASET)
    DATASET = as.data.frame(DATASET)
    #ncol(DATASET)
    
    #auto column name
    col.names <- paste("col", 1:length(DATASET), sep="")
    names(DATASET) <- col.names
    #print(str(DATASET))# Inspect the structure
    
    #Seperate data from class
    DATASET_Training <- DATASET[,1:(length(DATASET)-1)]
    DATASET_TrainClasses <- DATASET[,length(DATASET)]
    
    # summary(DATASET_Training)
    
    set.seed(5)
    boruta.train <- Boruta(DATASET_Training, DATASET_TrainClasses, doTrace = 2)
    
    output$resultBoruta <- renderPrint({
      print(boruta.train)
    })
    #print(boruta.train)
    
    #boruta.train$finalDecision
    #boruta.train$ImpHistory
    #boruta.train$pValue
    #boruta.train$maxRuns
    #boruta.train$timeTaken
    
    #boruta.train$impSource

    output$drawPlotBoruta <- renderPlot({
      input$doBoruta
      plot(boruta.train, xlab = "", xaxt = "n")
      lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i) boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
      names(lz) <- colnames(boruta.train$ImpHistory)
      
      
      Labels <- sort(sapply(lz, median))
      axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
    })
  }
  
  output$runBoruta <- renderUI({
    df <- m_fileBoruta()
    if (is.null(df))
      return(NULL)
    actionButton("doBoruta", "Run", icon = icon("microchip"))
  })
  
  observeEvent(input$doBoruta, {
    df <- m_fileBoruta()
    if (is.null(df))
      return(NULL)
    borutaFunc(m_fileBoruta())
  })
  
  # caret 
  m_fileCaret <- reactive({
    infile <- input$fileCaret
    if (is.null(infile)) {
      return(NULL)
    }
    fread(infile$datapath, header = T, sep = ',', verbose=F, colClasses = "numeric")
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
  
}
