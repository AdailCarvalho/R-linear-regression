# ==========================================================
# @author: Adail Carvalho
# @since : 2018-03-26
# ==========================================================

#Requirements

#Used for matrix inversion
#install.packages("matlib")
library(matlib)

#Error messages
insuficientArgsException <- simpleError("Missing independent variables. Unable to proceed.")
illegalArgsException <- simpleError("Some values are not in the proper expected way.")
notSupportedException <- simpleError("Operation with the passed args are not supported yet.")


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
               $Coefficients :  
               $Predictions  : matrix that shows the predicted values, the real values for Y and the E residuals

 # usage

 beearModel <- linearModelMain("C:/opt/beer_delivery.csv","\t", "Tempo", "NumCaixas", "Distancia")
 houses <- linearModelMain("C:/opt/houses_set.csv", ",", "SalePrice", "OverallQual", "YearBuilt", "MSSubClass", "BedroomAbvGr")
 
 # checking values
 
 houses$Coefficients
 houses$Predictions
'
linearModelMain <- function (filePath, delimiter, dependentVar, ...) {
  independentVarList <- list(...)
    
  if (is.null(dependentVar) || length(independentVarList) == 0) {
    stop(insuficientArgsException)    
  }
  
  matrixDataSet <<- getFileToMatrix(filePath, delimiter)
  if (length(independentVarList) == 1) {
    resultModel <<- simpleLinearModel(matrixDataSet, dependentVar)
  } else {
    resultModel <<- multiLinearModel(matrixDataSet, dependentVar, independentVarList)    
  }
}

getFileToMatrix <- function(filePath, delimiter) {
  rawDataSet <-  read.csv(filePath, sep = delimiter, header = TRUE)
  returnValue(data.matrix(rawDataSet))
  
}

simpleLinearModel <- function (matrixDataSet, dependentVar) {
  if (!is.matrix(matrixDataSet)) {
    stop(illegalArgsException)
  }
 
  dependentIndex <- grep(dependentVar, colnames(matrixDataSet))
  dependentMatrix <<- matrixDataSet[,dependentIndex]
  independentMatrix <<- matrixDataSet[,-dependentIndex]
  
  # Generates a matrix that holds the mean of each X variable in the dataset.
  # x - ~x ~> ~x = avg(x)
  levelsMatrizX <- getRegressionLevelsMatrix(independentMatrix)
  # y - ~y ~> ~y = avg(y)
  levelsMatrixY <- getRegressionLevelsMatrix(dependentMatrix)
  print(levelsMatrizX)
  print(levelsMatrixY)
}

multiLinearModel <- function (matrixDataSet, dependentVar, independentVarList) {
  if (!is.matrix(matrixDataSet)) {
    stop(illegalArgsException)
  }
  
  #Identifies which column represents the dependent variable. Separates the X variables and Y
  #And store in different matrices
  dependentColIndex <- grep(dependentVar, colnames(matrixDataSet))
  dependentMatrix <<- matrixDataSet[,dependentColIndex]
  
  independentMatrix <- matrix(0, nrow = nrow(matrixDataSet), ncol = length(independentVarList))
  colnames(independentMatrix) <- independentVarList
  for (i in independentVarList) {
    name <- grep(i, colnames(matrixDataSet))
    independentMatrix[,i] <- matrixDataSet[,name]  
  }
  
  # Generates a matrix that holds the mean of each X variable in the dataset.
  # x - ~x ~> ~x = avg(x)
  levelsMatrix <- getRegressionLevelsMatrix(as.matrix(independentMatrix))
  
  xProductMatrix <<- matrixTranposeAndMultiply(levelsMatrix, NULL)
  xProductMatrix <- treatInfValuesAsZeros(xProductMatrix)
  xProductMatrix <- getInverseMatrix(xProductMatrix)
  yProductMatrix <- matrixTranposeAndMultiply(levelsMatrix, as.matrix(dependentMatrix))
 
  # Calculates the coefficients
  coeficientsMatrix <- matrixMultiply(xProductMatrix,yProductMatrix)
  coeficientsMatrix <- as.matrix(coeficientsMatrix[,1])
  
  # Estimates Y values
  resultModel <- createRegressionModel(coeficientsMatrix, independentMatrix, dependentMatrix, levelsMatrix)
  linearModel <- cbind(dependentMatrix,resultModel)
  colnames(linearModel) <- c("Real_Y","Predict_Y", "Resid_E")
  
  # Gather generetade values
  results <- list(coeficientsMatrix, linearModel)
  names(results) <-c("Coefficients", "Predictions")
  
  returnValue(results)
}

# Returns the mean of independent var.
getRegressionLevelsMatrix <- function(matriz) {
  avgIndependentsVarMatrix <<- t(as.matrix(colMeans(matriz)))
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

# Returns the product of matrices multiplication
matrixTranposeAndMultiply <- function(matriz, matrixB) {
  transpMatrix <- t(matriz)
  
  if (is.null(matrixB)) {
    matrixY <<- matriz
  } else {
    matrixY <<- matrixB
  } 
  
  returnValue(matrixMultiply(transpMatrix, matrixY))
}

# Multiplies a matrix by it's transposed version
matrixMultiply <- function(matrixA, matrixB) {
  resultMatrix <<- matrix(0, nrow = nrow(matrixA), ncol = ncol(matrixB))
  for (r in 1:nrow(matrixA)) {
    for (c in 1:ncol(matrixB)) {
      for (kol in 1:ncol(matrixA)) {
        resultMatrix[r,c] <- resultMatrix[r,c] + (matrixA[r,kol] * matrixB[kol, c])
      }
    }
  }
  
  returnValue(resultMatrix)
  
}

# Estimates the values for Y, using the calculated coefficients.
# Apllies the following rule: ~Y : B0 + B1(X1 - ~X1) + B2(X2 - ~X2) + ... + Bn(Xn - ~Xn)
# Where ~Y = estimated value for dependent value Y, Bi = Coeficient values, Xi = observed value for X, ~Xi = mean of X variable.
# returns a Matrix with the estimated values for Y and the residuals E, where E = real value of Y - estimated value for Y
createRegressionModel <- function(coefficientsMatrix, independentMatrix, dependentMatrix, levelsMatrix) {
  predictedY <- matrix(0, nrow = nrow(independentMatrix), ncol = 1)
  residuals <- matrix(0 , nrow = nrow(independentMatrix), ncol = 1)
  
  
  for (r in 1:nrow(independentMatrix)) {
    for (coe in 1:nrow(coefficientsMatrix)) {
      if (coe == 1) {
        predictedY[r,1] <- predictedY[r,1] + coefficientsMatrix[coe,]
      } else {
        predictedY[r,1] <- predictedY[r,1] + (coefficientsMatrix[coe,] * levelsMatrix[r,coe])
      }
    }
  }
  
  residuals <- dependentMatrix - predictedY;

  returnValue(cbind(predictedY, residuals))
}

# Returns the (matriz)-¹ of a given matrix.
# inv(x) is a funtion provided by the package matlib*
getInverseMatrix <- function(matriz) {
  returnValue(inv(matriz))
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