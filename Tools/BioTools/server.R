library("e1071")
library("SparseM")
library("caret")
library("randomForest")
library("data.table")
library("Boruta")
library("class")

server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)
  set.seed(2)
  
  
  filetraining <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    # fread(infile$datapath, header = F, sep = ',', verbose=F, colClasses =
    # 'character')
    infile$datapath
  })
  
  
  filetesting <- reactive({
    input$doRF
    infile <- input$file2
    if (is.null(infile)) {
      return(NULL)
    }
    # fread(infile$datapath, header = F, sep = ',', verbose=F, colClasses =
    # 'character')
    infile$datapath
  })
  
  
  # FUNCTION FOR CALCULATE SOME STATISTICS FORMULA
  funcPerformanceEvaluation <- function(modelResult, testingClassName) {
    TP = FP = FN = TN = 0
    for (i in 1:length(modelResult)) {
      if (modelResult[i] == "1" && testingClassName[i] == "1") {
        TP = TP + 1
      }
      if (modelResult[i] == "1" && testingClassName[i] == "0") {
        FP = FP + 1
      }
      if (modelResult[i] == "0" && testingClassName[i] == "1") {
        FN = FN + 1
      }
      if (modelResult[i] == "0" && testingClassName[i] == "0") {
        TN = TN + 1
      }
    }
    
    # ------------------------------------------------------- Calculate the
    # value of statistical measurements of the performance evaluation
    # (Testing dataset)
    Sensitivity = TP/(TP + FN)
    Specificity = TN/(FP + TN)
    Precision = TP/(TP + FP)
    Accuracy = (TP + TN)/(TP + FP + TN + FN)
    MCC = ((TP * TN) - (FP * FN))/sqrt((TP + FP) * (TP + FN) * (TN + 
                                                                  FP) * (TN + FN))
    F1 = (2 * TP)/((2 * TP) + FP + FN)
    
    paste("TP : ", TP, "FP : ", FP, "FN : ", FN, "TN : ", TN, sep = " ")
  }
  
  
  # random forest
  randomForestFunc <- function(testingFilePath, trainingFilePath) {
    
    ############################################################################# DATA PREPARATION data() #check your data get the data from somewhere
    TrainingFile = trainingFilePath
    TestingFile = testingFilePath
    
    # ------------------------------------------------------- Training Data
    TrainData <- fread(TrainingFile, header = F, sep = ",", verbose = F, 
                       colClasses = "character")
    TrainData = as.matrix(TrainData)
    NumberColumn = ncol(TrainData)
    # ------------------------------------------------------- Select only
    # Data from Training dataset
    TrainingData_NoClassName = TrainData[, 1:NumberColumn - 1]
    typeof(TrainingData_NoClassName)
    # ------------------------------------------------------- Select only
    # class/category from Training Data
    TrainingData_OnlyClassName = TrainData[, NumberColumn]
    # -------------------------------------------------------
    
    # Testing Data
    TestingData <- fread(TestingFile, header = F, sep = ",", verbose = F, 
                         colClasses = "numeric")
    TestingData = as.matrix(TestingData)
    NumberColumn = ncol(TestingData)
    # ------------------------------------------------------- Select only
    # Data from testing dataset
    TestingData_NoClassName = TestingData[, 1:NumberColumn - 1]
    # ------------------------------------------------------- Select only
    # class/category from Testing Data
    TestingData_OnlyClassName = TestingData[, NumberColumn]
    
    
    ############################################################################# N-FOLD CROSS VALIDATION Number of Fold
    nrFolds <- 5
    # ------------------------------------------------------- generate
    # array containing fold-number for each sample (row)
    folds <- rep_len(1:nrFolds, nrow(TrainData))
    output1 <- NULL
    output$result1 <- renderPrint({
      # actual cross validation
      for (k in 1:nrFolds) {
        # actual split of the data
        fold <- which(folds == k)
        data.train <- TrainData[-fold, ]
        ncolumn = ncol(data.train)
        data.test <- TrainData[fold, ]
        
        # train and test your model with data.train and data.test
        model = randomForest(data.train[, 1:ncolumn - 1], factor(data.train[, 
                                                                            ncolumn]), ntree = 10)  #create model
        rf.pred = predict(model, data.test)  #predicition
        
        ## accuracy writeLines(paste('', 'Overall Accuracy N: ', k, ' Fold, ',
        ## mean(rf.pred==data.test[,ncolumn]), sep = ''))
        
        # Call function to evaluate performance
        
        # output1 = paste(k, funcPerformanceEvaluation(rf.pred,
        # data.test[,ncolumn]), sep = '');
        output1 <- rbind(output1, paste(funcPerformanceEvaluation(rf.pred, 
                                                                  data.test[, ncolumn]), sep = " "))
      }
      
      output1
    })
    ############################################################################# PREDICTION Generate Model
    model = randomForest(TrainingData_NoClassName, factor(TrainingData_OnlyClassName), 
                         ntree = 30)  #create model
    model.result = predict(model, TestingData)  #predicition
    # table(model.result, TestingData_OnlyClassName)
    
    # Overall accuracy mean(model.result==TestingData_OnlyClassName)
    
    
    
    # -------------------------------------------------------
    # writeLines(paste('', 'PERFORMANCE EVALUATION USING TEST DATASET: ',
    # '', sep = '')) Call function to evaluate performance
    output$result2 <- renderUI({
      print(paste(funcPerformanceEvaluation(model.result, TestingData_OnlyClassName), 
                  sep = ""))
    })
  }
  
  output$uploadTesting <- renderUI({
    df <- filetraining()
    if (is.null(df)) 
      return(NULL)
    fileInput("file2", "Choose Testing File", accept = c("text/csv", 
                                                         "text/comma-separated-values,text/plain", ".csv", ".libsvm", 
                                                         ".arff"))
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
  
  Read.LibSVM.File <- function(LibSVMFileName) {
    x <- read.matrix.csr(LibSVMFileName)  # From library('e1071') package
    # Get Real Data
    Data = as.data.frame(as.matrix(x$x))
    LibSVM.Label.Class = x$y
    # Add Class to Data
    Data$class = LibSVM.Label.Class
    
    
    return(Data)
    
  }
  # boruta
  m_fileBoruta <- reactive({
    infile <- input$fileBoruta
    if (is.null(infile)) {
      return(NULL)
    }
    Read.LibSVM.File(infile$datapath)
  })
  borutaFunc <- function(x) {
    # Load the dataset
    DATASET <- x
    
    # is.data.frame(DATASET)
    DATASET = as.data.frame(DATASET)
    # ncol(DATASET)
    
    # auto column name
    col.names <- paste("col", 1:length(DATASET), sep = "")
    names(DATASET) <- col.names
    # print(str(DATASET))# Inspect the structure
    
    # Seperate data from class
    DATASET_Training <- DATASET[, 1:(length(DATASET) - 1)]
    DATASET_TrainClasses <- DATASET[, length(DATASET)]
    
    # summary(DATASET_Training)
    
    set.seed(5)
    boruta.train <- Boruta(DATASET_Training, DATASET_TrainClasses, 
                           doTrace = 2)
    
    output$resultBoruta <- renderPrint({
      print(boruta.train)
    })
    
    output$drawPlotBoruta <- renderPlot({
      input$doBoruta
      plot(boruta.train, xlab = "", xaxt = "n")
      lz <- lapply(1:ncol(boruta.train$ImpHistory), function(i) boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[, 
                                                                                                                          i]), i])
      names(lz) <- colnames(boruta.train$ImpHistory)
      
      
      Labels <- sort(sapply(lz, median))
      axis(side = 1, las = 2, labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), 
           cex.axis = 0.7)
    })
  }
  
  borutaRB <- reactive({
    paste(input$rb_boruta)
  })
  
  output$uploadFieldBoruta <- renderUI({
    if (borutaRB() != "mydata") {
      selectInput("BorutaDataSelect", "Choose a dataset:", choices = c("iris", 
                                                                       "cars", "quakes", "rock", "titanic"))
    } else {
      fileInput("fileBoruta", "Choose Dataset", accept = c("text/csv", 
                                                           "text/comma-separated-values,text/plain", ".csv", ".libsvm", 
                                                           ".arff"))
    }
    
  })
  
  output$runBoruta <- renderUI({
    actionButton("doBoruta", "Run", icon = icon("microchip"))
  })
  
  borData <- reactive({
    switch(input$BorutaDataSelect, rock = rock, iris = iris, cars = cars, 
           quakes = quakes, titanic = Titanic)
  })
  
  observeEvent(input$doBoruta, {
    if (borutaRB() != "mydata") {
      borutaFunc(borData())
    } else {
      borutaFunc(m_fileBoruta())
    }
    
  })
  
  # caret
  m_fileCaret <- reactive({
    infile <- input$fileCaret
    if (is.null(infile)) {
      return(NULL)
    }
    fread(infile$datapath, header = F, sep = ",", verbose = F, colClasses = "character")
  })
  caretFunc <- function(x) {
    DATASET <- x
    is.data.frame(DATASET)
    DATASET = as.data.frame(DATASET)
    ncol(DATASET)
    
    # auto column name
    col.names <- paste("col", 1:length(DATASET), sep = "")
    names(DATASET) <- col.names
    print(str(DATASET))  # Inspect the structure
    
    # Seperate data from class
    DATASET_Training <- DATASET[, 1:length(DATASET) - 1]
    DATASET_TrainClasses <- DATASET[, length(DATASET)]
    
    # define the control using a random forest selection function
    control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
    # run the RFE algorithm
    results <- rfe(DATASET_Training, DATASET_TrainClasses, sizes = c(1:length(DATASET) - 
                                                                       1), rfeControl = control)
    # summarize the results
    output$resultCaret <- renderPrint({
      print(results)
    })
    
    # list the chosen features
    predictors(results)
    # plot the results
    output$drawPlotCaret <- renderPlot({
      plot(results, type = c("g", "o"))
    })
    
  }
  
  caretRB <- reactive({
    paste(input$rb_caret)
  })
  
  output$uploadFieldCaret <- renderUI({
    if (caretRB() != "mydata") {
      selectInput("CaretDataSelect", "Choose a dataset:", choices = c("iris", 
                                                                      "cars", "quakes", "rock", "titanic"))
    } else {
      fileInput("fileCaret", "Choose Dataset", accept = c("text/csv", 
                                                          "text/comma-separated-values,text/plain", ".csv", ".libsvm", 
                                                          ".arff"))
    }
    
  })
  
  
  
  carData <- reactive({
    switch(input$CaretDataSelect, rock = rock, iris = iris, cars = cars, 
           quakes = quakes, titanic = Titanic)
  })
  
  output$runCaret <- renderUI({
    actionButton("doCaret", "Run", icon = icon("microchip"))
  })
  
  observeEvent(input$doCaret, {
    if (caretRB() != "mydata") {
      caretFunc(carData())
    } else {
      caretFunc(m_fileCaret())
    }
  })
  
  # about section
  observeEvent(input$about, {
    showModal(modalDialog(title = span(tagList(icon("info-circle"), 
                                               "About")), tags$div(HTML("<img src='https://avatars1.githubusercontent.com/u/4516635?v=3&s=460' width=150><br/><br/>", 
                                                                        "<p>Developer : Rosdyana Kusuma</br>Email : <a href=mailto:rosdyana.kusuma@gmail.com>rosdyana.kusuma@gmail.com</a></br>linkedin : <a href='https://www.linkedin.com/in/rosdyanakusuma/' target=blank>Open me</a></p>")), 
                          easyClose = TRUE))
  })
  
  # naivebayes
  funcNaiveBayes <- function(testingFilePath, trainingFilePath) {
    
    ############################################################################# DATA PREPARATION get the data from somewhere
    
    TrainingFile = trainingFilePath
    TestingFile = testingFilePath
    
    
    # ------------------------------------------------------- Training Data
    TrainData <- read.csv(file = TrainingFile, header = FALSE, sep = ",")
    # TrainData = as.matrix(TrainData) #(Penting, rubah dlu ke matrix)
    NumberColumn = ncol(TrainData)
    # ------------------------------------------------------- Select only
    # Data from Training dataset
    TrainingData_NoClassName = TrainData[, 1:NumberColumn - 1]
    typeof(TrainingData_NoClassName)
    # ------------------------------------------------------- Select only
    # class/category from Training Data
    TrainingData_OnlyClassName = TrainData[, NumberColumn]
    # -------------------------------------------------------
    
    # Testing Data
    TestingData <- read.csv(file = TestingFile, header = FALSE, sep = ",")
    # TestingData = as.matrix(TestingData)
    NumberColumn = ncol(TestingData)
    # ------------------------------------------------------- Select only
    # Data from testing dataset
    TestingData_NoClassName = TestingData[, 1:NumberColumn - 1]
    # ------------------------------------------------------- Select only
    # class/category from Testing Data
    TestingData_OnlyClassName = TestingData[, NumberColumn]
    
    
    
    
    ############################################################################# N-FOLD CROSS VALIDATION Number of Fold
    nrFolds <- 5
    # ------------------------------------------------------- generate
    # array containing fold-number for each sample (row)
    folds <- rep_len(1:nrFolds, nrow(TrainData))
    outputBN <- NULL
    output$resultBN2 <- renderPrint({
      
      # actual cross validation
      for (k in 1:nrFolds) {
        # actual split of the data
        fold = which(folds == k)
        data.train = TrainData[-fold, ]
        ncolumn = ncol(data.train)
        data.test = TrainData[fold, ]
        
        # train and test your model with data.train and data.test
        model = naiveBayes(data.train[, 1:ncolumn - 1], as.factor(data.train[, 
                                                                             ncolumn]))
        # help(naiveBayes)
        k.result = predict(model, newdata = data.test[, 1:ncolumn - 
                                                        1])
        outputBN <- rbind(outputBN, paste(funcPerformanceEvaluation(k.result, 
                                                                    data.test[, ncolumn]), sep = ""))
        # print(paste('NaiveBayes Cross: ', k, ' [', trainingFilePath,'],',
        # funcPerformanceEvaluation(k.result, data.test[,ncolumn]), sep = ''));
      }
      outputBN
    })
    ############################################################################# PREDICTION Generate Model
    
    model = naiveBayes(TrainingData_NoClassName, as.factor(TrainingData_OnlyClassName))
    model.result = predict(model, TestingData)
    output$resultBN <- renderPrint({
      print(paste(funcPerformanceEvaluation(model.result, TestingData_OnlyClassName), 
                  sep = ""))
    })
    
    
  }
  
  
  output$uploadTestingBN <- renderUI({
    df <- filetrainingBN()
    if (is.null(df)) 
      return(NULL)
    fileInput("file2BN", "Choose Testing File", accept = c("text/csv", 
                                                           "text/comma-separated-values,text/plain", ".csv", ".libsvm", 
                                                           ".arff"))
  })
  
  output$runBN <- renderUI({
    df <- filetestingBN()
    if (is.null(df)) 
      return(NULL)
    actionButton("doBN", "Progress", icon = icon("microchip"))
  })
  
  observeEvent(input$doBN, {
    df <- filetestingBN()
    if (is.null(df)) 
      return(NULL)
    funcNaiveBayes(filetrainingBN(), filetestingBN())
  })
  
  
  filetrainingBN <- reactive({
    infile <- input$file1BN
    if (is.null(infile)) {
      return(NULL)
    }
    # fread(infile$datapath, header = F, sep = ',', verbose=F, colClasses =
    # 'character')
    infile$datapath
  })
  
  
  filetestingBN <- reactive({
    input$doBN
    infile <- input$file2BN
    if (is.null(infile)) {
      return(NULL)
    }
    # fread(infile$datapath, header = F, sep = ',', verbose=F, colClasses =
    # 'character')
    infile$datapath
  })
  
  # knn
  funcKNN <- function(testingFilePath, trainingFilePath) {
    
    
    ############################################################################# DATA PREPARATION data() #check your data get the data from somewhere
    
    TrainingFile = trainingFilePath
    TestingFile = testingFilePath
    
    # ------------------------------------------------------- Training Data
    TrainData <- fread(TrainingFile, header = F, sep = ",", verbose = F, 
                       colClasses = "character")
    TrainData = as.matrix(TrainData)
    NumberColumn = ncol(TrainData)
    # ------------------------------------------------------- Select only
    # Data from Training dataset
    TrainingData_NoClassName = TrainData[, 1:NumberColumn - 1]
    typeof(TrainingData_NoClassName)
    # ------------------------------------------------------- Select only
    # class/category from Training Data
    TrainingData_OnlyClassName = TrainData[, NumberColumn]
    # -------------------------------------------------------
    
    # Testing Data
    TestingData <- fread(TestingFile, header = F, sep = ",", verbose = F, 
                         colClasses = "numeric")
    TestingData = as.matrix(TestingData)
    NumberColumn = ncol(TestingData)
    # ------------------------------------------------------- Select only
    # Data from testing dataset
    TestingData_NoClassName = TestingData[, 1:NumberColumn - 1]
    # ------------------------------------------------------- Select only
    # class/category from Testing Data
    TestingData_OnlyClassName = TestingData[, NumberColumn]
    
    
    
    
    
    ############################################################################# N-FOLD CROSS VALIDATION Number of Fold
    nrFolds <- 5
    # ------------------------------------------------------- generate
    # array containing fold-number for each sample (row)
    folds <- rep_len(1:nrFolds, nrow(TrainData))
    outputknn <- NULL
    output$resultknn2 <- renderPrint({
      # actual cross validation
      for (k in 1:nrFolds) {
        # actual split of the data
        fold = which(folds == k)
        data.train = TrainData[-fold, ]
        ncolumn = ncol(data.train)
        data.test = TrainData[fold, ]
        # writeLines(paste('\nNumber of Train Data: ', nrow(data.train), sep =
        # '')) writeLines(paste('Number of Testing Data: ', nrow(data.test),
        # sep = ''))
        
        # train and test your model with data.train and data.test
        # tune.knn(data.train[,1:ncolumn-1], data.train[,ncolumn], k = 1:5) #
        # best K = 1
        k.result = knn(data.train[, 1:ncolumn - 1], data.test[, 
                                                              1:ncolumn - 1], data.train[, ncolumn], k = 1)
        # k.result#prediction
        
        ## accuracy writeLines(paste('', 'Overall Accuracy N: ', k, ' Fold, ',
        ## mean(data.test[,ncolumn]==k.result), sep = ''))
        
        # Call function to evaluate performance
        # funcPerformanceEvaluation(k.result, data.test[,ncolumn]);
        outputknn <- rbind(outputknn, paste(funcPerformanceEvaluation(k.result, 
                                                                      data.test[, ncolumn]), sep = ""))
      }
      outputknn
    })
    
    
    
    
    ############################################################################# PREDICTION Generate Model
    model.result = knn(TrainingData_NoClassName, TestingData_NoClassName, 
                       TrainingData_OnlyClassName, k = 5)
    output$resultknn <- renderPrint({
      print(paste(funcPerformanceEvaluation(model.result, TestingData_OnlyClassName), 
                  sep = ""))
    })
  }
  
  output$uploadTestingknn <- renderUI({
    df <- filetrainingknn()
    if (is.null(df)) 
      return(NULL)
    fileInput("file2knn", "Choose Testing File", accept = c("text/csv", 
                                                            "text/comma-separated-values,text/plain", ".csv", ".libsvm", 
                                                            ".arff"))
  })
  
  output$runknn <- renderUI({
    df <- filetestingknn()
    if (is.null(df)) 
      return(NULL)
    actionButton("doknn", "Progress", icon = icon("microchip"))
  })
  
  observeEvent(input$doknn, {
    df <- filetestingknn()
    if (is.null(df)) 
      return(NULL)
    funcKNN(filetrainingknn(), filetestingknn())
  })
  
  
  filetrainingknn <- reactive({
    infile <- input$file1knn
    if (is.null(infile)) {
      return(NULL)
    }
    # fread(infile$datapath, header = F, sep = ',', verbose=F, colClasses =
    # 'character')
    infile$datapath
  })
  
  filetestingknn <- reactive({
    input$doknn
    infile <- input$file2knn
    if (is.null(infile)) {
      return(NULL)
    }
    # fread(infile$datapath, header = F, sep = ',', verbose=F, colClasses =
    # 'character')
    infile$datapath
  })
}
