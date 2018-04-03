  # About ============================
  # @author: Adail Carvalho
  # @since : 2018-03-24
  # __________________________________
  
  #Requires ====
  
  #install.packages("matlib")
  library(matlib)
  
  # Matrices / DFrames headers ====
  resultsHeader <- c("Coefficients", "Predictions", "RMSErrorRate", "Dataset")
  lmHeader <- c("Real_Y","Predict_Y", "Resid_E")
  
  #Error messages ====
  insuficientArgsException <- simpleError("Missing independent variables. Unable to proceed.")
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
   @ resultModel A List containing both Predictions and the Linear Model coefficients.
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
      stop(insuficientArgsException)    
    }
    
    matrixDataSet <- getFileToMatrix(filePath, delimiter)
    if (length(independentVarList) == 1) {
      resultModel <- simpleLinearModel(matrixDataSet, dependentVar, independentVarList[[1]])
    } else {
      resultModel <- multiLinearModel(matrixDataSet, dependentVar, independentVarList)    
    }
    
    returnValue(resultModel)
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
    
    resultModel <- createSimpleRegressionModel(intercept, slope, independentVector, dependentVector)
    
    coefficientsMatrix <- matrix(nrow = 1, ncol = 2, dimnames = list(c(1),c("Intercept", independentVar)))
    coefficientsMatrix[,1] <- intercept
    coefficientsMatrix[,2] <- slope
      
    modelErrorRate <- estimateModelError(resultModel[,2])
    
    linearModel <- cbind(dependentVector,resultModel)
    colnames(linearModel) <- lmHeader
    
    # Gather generetade values
    results <- list(coefficientsMatrix, linearModel, modelErrorRate, matrixDataSet)
    names(results) <- resultsHeader
    
    returnValue(results)
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
    
    #Identifies which column represents the dependent variable. Separates the X variables and Y
    #And store in different matrices.
    dependentColIndex <- grep(dependentVar, colnames(matrixDataSet))
    dependentMatrix <- matrixDataSet[,dependentColIndex]
    
    independentMatrix <- matrix(0, nrow = nrow(matrixDataSet), ncol = length(independentVarList))
    colnames(independentMatrix) <- independentVarList
    for (i in independentVarList) {
      name <- grep(i, colnames(matrixDataSet))
      independentMatrix[,i] <- matrixDataSet[,name]  
    }
    
    # Generates a matrix that holds the mean of each X variable in the dataset.
    # x - ~x ~> ~x = avg(x)
    levelsMatrix <- getRegressionLevelsMatrix(as.matrix(independentMatrix))
    
    xProductMatrix <- matrixTranposeAndMultiply(levelsMatrix, NULL)
    xProductMatrix <- treatInfValuesAsZeros(xProductMatrix)
    xProductMatrix <- getInverseMatrix(xProductMatrix)
    yProductMatrix <- matrixTranposeAndMultiply(levelsMatrix, as.matrix(dependentMatrix))
   
    # Calculates the coefficients
    coeficientsMatrix <- matrixMultiply(xProductMatrix,yProductMatrix)
    coeficientsMatrix <- as.matrix(coeficientsMatrix[,1])
    
    # Estimates Y values
    resultModel <- createRegressionModel(coeficientsMatrix, independentMatrix, dependentMatrix, levelsMatrix)
    
    # Calculates the Error rate using Root Main Square Error
    modelErrorRate <- estimateModelError(resultModel[,2])
    
    linearModel <- cbind(dependentMatrix,resultModel)
    colnames(linearModel) <- lmHeader
    
    # Gather generetade values
    results <- list(coeficientsMatrix, linearModel, modelErrorRate, matrixDataSet)
    names(results) <- resultsHeader
    
    returnValue(results)
  }
  
  # Linear Model Creation ====
  
  # Estimates the values for Y, using the calculated coefficients.
  # Apllies the following rule: ~Yi : B0 + (B1 * Xi)
  # Where ~Yi = estimated value for dependent value Y, B0 = Slope coefficient, B1 = Intercept coefficient, Xi = observed value for X, 
  # and i, the iterator value over the X independent vector
  # returns a Matrix with the estimated values for Y and the residuals E, where E = real value of Y - estimated value for Y
  createSimpleRegressionModel <- function(coefficientB0, coefficientB1, independentVector, dependentVector) {
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
  createRegressionModel <- function(coefficientsMatrix, independentMatrix, dependentMatrix, levelsMatrix) {
    predictedY <- matrix(0, nrow = nrow(independentMatrix), ncol = 1)
    residualMatrix <- matrix(0 , nrow = nrow(independentMatrix), ncol = 1)
    
    
    for (r in 1:nrow(independentMatrix)) {
      for (coe in 1:nrow(coefficientsMatrix)) {
        if (coe == 1) {
          predictedY[r,1] <- predictedY[r,1] + coefficientsMatrix[coe,]
        } else {
          predictedY[r,1] <- predictedY[r,1] + (coefficientsMatrix[coe,] * levelsMatrix[r,coe])
        }
      }
    }
    
    residualMatrix <- dependentMatrix - predictedY;
    
    returnValue(cbind(predictedY, residualMatrix))
  }
  
  # Matrices general operations ====
  # Returns the (matriz)-¹ of a given matrix.
  # inv(x) is a funtion provided by the package matlib*
  getInverseMatrix <- function(matriz) {
    returnValue(inv(matriz))
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
  
  # Substitutes Inf values for zeros
  treatInfValuesAsZeros <- function(matriz) {
    for (c in 1:ncol(matriz)) {
      for (r in 1:nrow(matriz)) {
        if (is.infinite(matriz[c,r])) {
          matriz[c,r] <- 0
        } 
      }
    }
    returnValue(matriz) 
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
  getFileToMatrix <- function(filePath, delimiter) {
    rawDataSet <-  read.csv(filePath, sep = delimiter, header = TRUE)
    returnValue(data.matrix(rawDataSet))
  }
  
  # Beta-Functions ====
  ' Returns a matrix represetation of the dataset, removing the values below and above the floor and roof (ceiling) params.
   @matrixDataset A matrix representing the dataset to be handled.
   @residualVector A vetor that contains the values of the ~Y differences between the real values for Y x predicted ones
   @floor Lines where the E values are less than the floor value will be removed from matrixDataset. 
   @roof Lines where the E  values are greater than the roof value will be removed from matrixDataset.
   e.g.:
  
   OriginalM         Predictions (~Y) , Residuals (E)
  
    x1   x2   y  | ~Y     E
  1|2   -1    4  |  3.8   0.2
  2|3   9.2  -8  |  -7.3  -0.7
  3|-1  0.9  0.9 |  3.6   2.6
  4|-2   4.5   1 |  -1    -2 
  
  NewDataset <- removeOutliersLines(OriginalM, E, -1, 2.5)

   NewDataset
    x1   x2   y
  1|2   -1    4
  2|3  9.2   -8
  
  '
  removeOutliersLines <- function(matrixDataset, residualVector, floor, roof) {
    if (floor == roof) {
      stop(paste(illegalArgsException, "floor must be different from roof."))
    }
    nonOutfitted <- which(residualVector > floor & residualVector < roof)
    tmpMatrix <- matrix(nrow = length(nonOutfitted), ncol = ncol(matrixDataset), dimnames = list(c(1:length(nonOutfitted)), colnames(matrixDataset)))
    if (length(nonOutfitted) > 0) {
      for(r in 1:nrow(tmpMatrix)) {
        for (c in 1:ncol(tmpMatrix)) {
          tmpMatrix[r,c] <- matrixDataset[nonOutfitted[r],c]
        }
      }
      matrixDataset <- tmpMatrix
    } 

    returnValue(matrixDataset)
  }