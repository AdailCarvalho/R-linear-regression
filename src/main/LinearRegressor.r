  # About ============================
  # @author: Adail Carvalho
  # @since : 2018-03-24
  # __________________________________
  
  #Required Libs ====
  checkAndInstallRequiredLib <- function(libname, extralib = NULL) {
    if (!require(libname)) {
      install.packages(libname)
    }
    
    if (!missing(extralib)) {
      if (!require(extralib)) {
        install.packages(extralib)  
      }
    }
  }
  # Plotly: library that provides beaultiful chart tools.
  library(plotly)

  # Webshot: provides chart exports capabilities.
  library(webshot)
  
  # Matrices / DFrames headers ====
  modelHeader <- c("Coefficients", "Predictions", "RMSErrorRate", "Dataset", "YDependent", "XIndependent")
  lmHeader <- c("Observed Y","Predicted Y", "Residual E")
  
  #Error messages ====
  insuficientArgsException <- simpleError("Missing one or more required variables to proceed = ")
  insuficientArgsWarning <- "Missing one or more required variables to do the requested operation = "
  illegalArgsException <- simpleError("Invalid args: ")
  notSupportedException <- simpleError("Operation with the passed args are not supported yet.")
  
  
  # Main ====
  ' Entry point for our Linear Model program
   # input
   @filePath CSV file that represents the dataset
   @delimiter The CSV file delimiter (if its a TSV, then use "\t")
   @dependentVar Represents the Y column name in the dataset, where Y represents the dependent variable in the Linear Model.
        Colum Y must be numeric.
   @... Represents one or more datasets X columns names, where X represents the independent variables in the Linear Model.
        Columns X must be numerics.
  
   # returns
   @ model A List containing both Predictions and the Linear Model coefficients.
                 $Coefficients : list containing the values founded for the linear model coeficients 
                 $Predictions  : matrix that shows the predicted values, the real values for Y and the E residuals
                 $RMSErrorRate : rating used to know how good our model is. A good model is always between -1< R <=1.
                 $Dataset: a matrix representation of the original dataset.
   # usage
  
   beearModel <- linearModel("C:/opt/beer_delivery.csv","\t", "Tempo", "NumCaixas", "Distancia")
   houses <- linearModel("C:/opt/houses_set.csv", ",", "SalePrice", "OverallQual", "YearBuilt", "MSSubClass", "BedroomAbvGr")
   
   # checking values
   
   houses$Coefficients
   houses$Predictions
  '
  linearModel <- function (filePath, delimiter, dependentVar, ...) {
    independentVarList <- list(...)
    
    if (is.null(dependentVar) || length(independentVarList) == 0) {
      stop(paste(insuficientArgsException, "{dependentVar, independentVarList}"))    
    }
    
    matrixDataSet <- getFileToMatrix(filePath, delimiter)
    if (length(independentVarList) == 1) {
      model <- simpleLinearModel(matrixDataSet, dependentVar, independentVarList[[1]])
    } else {
      model <- multiLinearModel(matrixDataSet, dependentVar, independentVarList)    
    }
    
    returnValue(model)
  }
  
  # Handle general operations to get the results
  # @returns results A list containing the coefficient values, RMSE error and the made predictions.
  simpleLinearModel <- function (matrixDataSet, dependentVar, independentVar) {
    if (!is.matrix(matrixDataSet)) {
      stop(illegalArgsException)
    }
    
    dependentIndex <- grep(dependentVar, colnames(matrixDataSet))
    independentIndex <- grep(independentVar, colnames(matrixDataSet))
    dependentVector <- matrixDataSet[,dependentIndex]
    independentVector <- matrixDataSet[,independentIndex]
    
    slope <- getSLRSlope(independentVector, dependentVector)
    intercept <- getSLRIntercept(independentVector, dependentVector, slope)
    
    resultModel <- doSimplePredictions(intercept, slope, independentVector, dependentVector)
    
    coefficientsMatrix <- matrix(nrow = 1, ncol = 2, dimnames = list(c(1),c("Intercept", independentVar)))
    coefficientsMatrix[,1] <- intercept
    coefficientsMatrix[,2] <- slope
      
    modelErrorRate <- estimateModelError(resultModel[,2])
    
    linearModel <- cbind(dependentVector,resultModel)
    colnames(linearModel) <- lmHeader
    
    # Gather generetade values
    model <- list(coefficientsMatrix, linearModel, modelErrorRate, matrixDataSet, dependentVar, independentVar)
    
    returnValue(model)
  }
  
  '
  Handle general operations to get the results
  @returns results A list containing the coefficient values, RMSE error and the made predictions. 
  
  # Usage (directly)
  
  myModel <- multiLinearModel(dataMatrix, "Y var", as.list(c("var x1", "var x2", "var xn")))
  '
  multiLinearModel <- function (matrixDataSet, dependentVar, independentVarList) {
    if (!is.matrix(matrixDataSet)) {
      stop(illegalArgsException)
    }
    
    dependentColIndex <- grep(dependentVar, colnames(matrixDataSet))
    dependentMatrix <- matrixDataSet[,dependentColIndex]
    
    independentMatrix <- getIndependentVarsAsMatrix(matrixDataSet, independentVarList)
    
    levelsMatrix <- getRegressionLevelsMatrix(as.matrix(independentMatrix))
    
    xProductMatrix <- matrixTranposeAndMultiply(levelsMatrix, NULL)
    xProductMatrix <- getInverseMatrix(xProductMatrix)
    yProductMatrix <- matrixTranposeAndMultiply(levelsMatrix, as.matrix(dependentMatrix))
    
    coefficientsMatrix <- getMLRCoefficients(xProductMatrix, yProductMatrix, independentMatrix, independentVarList)
    
    resultModel <- doMultivariantPredictions(coefficientsMatrix, independentMatrix, dependentMatrix)
    
    modelErrorRate <- estimateModelError(resultModel[,2])
    
    linearModel <- cbind(dependentMatrix,resultModel)
    colnames(linearModel) <- lmHeader
  
    model <- list(coefficientsMatrix, linearModel, modelErrorRate, matrixDataSet, dependentVar, independentVarList)
    model <- handleDataHeaders(model, modelHeader)
    
    returnValue(model)
  }
  
  # Linear Model ====
  
  # Estimates the values for Y, using the calculated coefficients.
  # Apllies the following rule: ~Yi : B0 + (B1 * Xi)
  # Where ~Yi = estimated value for dependent value Y, B0 = Slope coefficient, B1 = Intercept coefficient, Xi = observed value for X, 
  # and i, the iterator value over the X independent vector
  # returns a Matrix with the estimated values for Y and the residuals E, where E = real value of Y - estimated value for Y
  doSimplePredictions <- function(coefficientB0, coefficientB1, independentVector, dependentVector) {
    predictedY <- vector("numeric", length(dependentVector))
    residuals <- vector("numeric", length(independentVector))
    
    for (i in 1:length(independentVector)) {
      predictedY[i] <- coefficientB0 + (coefficientB1 * independentVector[i])
    }
    
    residualVector <- dependentVector - predictedY
    returnValue(cbind(predictedY, residualVector))
  }
  
  # Estimates the values for Y, using the calculated coefficients.
  # Apllies the following rule: ~Y : B0 + B1(X1 - ~X1) + B2(X2 - ~X2) + ... + Bn(Xn - ~Xn)
  # Where ~Y = estimated value for dependent value Y, Bi = Coeficient values, Xi = observed value for X, ~Xi = mean of X variable.
  # returns a Matrix with the estimated values for Y and the residuals E, where E = real value of Y - estimated value for Y
  doMultivariantPredictions <- function(coefficientsMatrix, independentMatrix, dependentMatrix = NULL) {
    predictedY <- matrix(0, nrow = nrow(independentMatrix), ncol = 1)
    residualMatrix <- matrix(0 , nrow = nrow(independentMatrix), ncol = 1)
    
    for (r in 1:nrow(independentMatrix)) {
      for (coe in 2:ncol(coefficientsMatrix)) {
        predictedY[r,1] <- predictedY[r,1] + (coefficientsMatrix[,coe] * independentMatrix[r,coe - 1])
      }
      predictedY[r,1] <- predictedY[r,1] + coefficientsMatrix[,1]
    }
    
    predictions <-NULL
    if (missing(dependentMatrix)) {
      predictions <- predictedY 
    } else {
      residualMatrix <- dependentMatrix - predictedY
      predictions <- cbind(predictedY, residualMatrix)
    }
    
    returnValue(predictions)
  }
  
  # Matrices general operations ====
  getMLRCoefficients <- function(xProductMatrix, yProductMatrix, independentMatrix, xLabels) {
    avgIndMat <- colMeans(independentMatrix)
    
    coefficientsMatrix <- matrixMultiply(xProductMatrix, yProductMatrix)
    coefficientsMatrix <- t(coefficientsMatrix)
    slope <- coefficientsMatrix[,1]
    
    for(c in 2:ncol(coefficientsMatrix)) {
      slope <- slope + (coefficientsMatrix[,c] * (-avgIndMat[c - 1]))
    }
    coefficientsMatrix[,1] <- slope
    colnames(coefficientsMatrix) <-c("[Intercept]", xLabels)
    returnValue(coefficientsMatrix)
    
  }
  # Returns the (matriz)-¹ of a given matrix.
  # inv(x) is a funtion provided by the package matlib*
  getInverseMatrix <- function(matriz) {
    invertedMatrix <- solve(matriz)
    returnValue(invertedMatrix)
  }
  
  # Returns the mean of independent variables in multivariant linear model
  getRegressionLevelsMatrix <- function(matriz) {
    avgIndependentsVarMatrix <- t(as.matrix(colMeans(matriz)))
    levelsMatrix <- cbind(getX0Dimension(matriz), matriz)
    
    for (c in colnames(matriz)) {
      x <- matriz[,c]
      l <- vector(class(x), length(x))
      for (i in 1:length(x)) {
        l[i] <- x[i] - avgIndependentsVarMatrix[,c]
      }
      levelsMatrix[,c] <- l
    }
    returnValue(levelsMatrix)  
  }
  
  # Represents the x0 coeficient in the lm equation, where x0 = 1. In the matrix of independent values, it is placed in the first column.
  getX0Dimension <- function(matriz) {
    numRows <- nrow(matriz)
    returnValue(matrix(rep(1, numRows), dimnames = list(c(1:numRows), c("X0"))))
  }
  
  
  # Return product of two matrices 
  matrixMultiply <- function(matrixA, matrixB) {
    resultMatrix <- matrix(0, nrow = nrow(matrixA), ncol = ncol(matrixB))
    for (r in 1:nrow(matrixA)) {
      for (c in 1:ncol(matrixB)) {
        for (kol in 1:ncol(matrixA)) {
          resultMatrix[r,c] <- resultMatrix[r,c] + (matrixA[r,kol] * matrixB[kol, c])
        }
      }
    }
    
    returnValue(resultMatrix)
    
  }
  
  # Returns the product of matrices
  matrixTranposeAndMultiply <- function(matriz, matrixB) {
    transpMatrix <- t(matriz)
    
    if (is.null(matrixB)) {
      matrixY <- matriz
    } else {
      matrixY <- matrixB
    } 
    
    returnValue(matrixMultiply(transpMatrix, matrixY))
  }
  
  # Linear Regression Rules Application ====
  # Estimate erros using RootMeanSquareError(RMSE).
  # @residualVector Vector that holds the E residuals of our model.
  estimateModelError <- function(residualVector) {
    rmse <- sqrt(sum(residualVector ** 2) / length(residualVector))
    returnValue(rmse)
  }
  
  # Returns SLR Slope value
  getSLRSlope <- function(independentVector, dependentVector) {
    levelsVectorX <- independentVector - mean(independentVector)
    levelsVectorY <- dependentVector - mean(dependentVector)
    
    numeratorB1 <- sum(levelsVectorX * levelsVectorY)
    denominatorB1 <- sum(levelsVectorX ** 2)  
    slope <- numeratorB1 / denominatorB1
    
    returnValue(slope)
  }
  
  # Returns SLR Intercept value
  getSLRIntercept <- function (independentVector, dependentVector, slope) {
    intercept <- mean(dependentVector) - (slope * mean(independentVector))
    returnValue(intercept)
  }
  
  # Utils ====
  getIndependentVarsAsMatrix <- function(matrixData, independentVarList) {
    independentMatrix <- matrix(0, nrow = nrow(matrixData), ncol = length(independentVarList))
    colnames(independentMatrix) <- independentVarList
    
    for (i in independentVarList) {
      name <- grep(i, colnames(matrixData))
      independentMatrix[,i] <- matrixData[,name]  
    }
    returnValue(independentMatrix)
  }
  
  getFileToMatrix <- function(filePath, delimiter) {
    rawDataSet <-  read.csv(filePath, sep = delimiter, header = TRUE)
    returnValue(data.matrix(rawDataSet))
  }
  
  ' Export charts as images.
   @p A chart object
   @filename Name of the file to be saved.
   @ext File extension
  '
  exportChartAsImg <- function(p, filename, ext) {
    export(p, paste(filename, ext))    
  }
  
  handleDataHeaders <- function(data, header) {
    names(data) <- modelHeader 
    returnValue(data)
  }
  
  
  # Charts ####
  # Add to a chart labels and title
  applyLayout <- function(p, xLabel, yLabel, chartTitle) {
    title <- NULL
    if (missing(chartTitle)) {
      title <- paste(yLabel, " by ", xLabel)
    } else {
      title <- chartTitle
    }
    
    p <- layout(p, title = title,
                xaxis = list(title = xLabel),
                yaxis = list (title = yLabel))
    returnValue(p)
    
  }
  
  'Export models as simple X vs Y line charts, from a given model.
  @ model A LinearRegressor object.
  @ dirToSave The directory where the plots will be exported
  '
  createChartsFromModel <- function(model, dirToSave = NULL) {
    for (x in model$XIndependent) {
      if (isTRUE(grepl('^[0-9]', x))) {
        x <- paste("X", x, sep = "")
      } 
      
      print(paste("Generating chart for => ", x))
      
      p <- lmYXLineChart(model$Dataset[,x], 
                         model$Dataset[,model$YDependent], 
                         model$Predictions[,"Predicted Y"],
                         model$Predictions[,"Residual E"],
                         x,
                         model$YDependent)
      
      exportChartAsImg(p, paste(dirToSave, model$YDependent, " by ", x), ".png")
    }
  }
  
  # Creates a line chart, by ploting y in the y axis and x in the x axis.  
  # lmYXLineChart(TipoMoradores, PrecoVenda, PrecoPrevisto, ResiduoModelo, "TipoMoradores", "PrecoVenda")
  lmYXLineChart <- function(xData, yData, yPredicted, eResid, xLabel, yLabel) {
    xName <- xLabel
    yName <- yLabel
    if (missing(xName)) {
      xName <- "X"
    }
    
    if (missing(yName)) {
      yName <- "Y"
    }
    
    chartFrame <- data.frame(xData, yData, yPredicted, eResid)    
    aggData <- aggregate(chartFrame, by = list(chartFrame$xData), FUN = mean)
    
    lmPlot<- plot_ly(aggData, x = ~aggData$xData, name = xName)
    
    lmPlot <- add_trace(lmPlot, y= ~aggData$yData, name = yName, type = 'scatter', mode = 'lines')
    lmPlot <- add_trace(lmPlot, y = ~aggData$yPredicted, name = paste(yName, "(Pred)"), type = 'scatter', mode = 'lines')
    lmPlot <- add_trace(lmPlot, y = ~aggData$eResid, name = 'Residual', type = 'scatter', mode = 'markers')

    lmPlot <- applyLayout(lmPlot, xName, yName)
    returnValue(lmPlot)
    
  }