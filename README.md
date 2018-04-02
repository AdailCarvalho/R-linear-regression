# R-Linear-Model

A program to perform Linear Regression analysis.

## Features

*	Simple Linear Regression: generates a regression model based on a X independent column and a Y dependent column
*	Multivariant Linear Regression: generates a regression model based on multiple X independent columns and a Y dependent column

## Requirements

The following packages need to be installed before running the program:

*	**matlib** : package used to make matrix math

Installing matlib:

	install.packages("matlib")
	
## Usage

The entry point for the program is the method linearModel(). 

*	**@filePath**: CSV file that represents the dataset;
*	**@delimiter**: the CSV file delimiter (if it's a TSV, then use "\t");
*	**@dependentVar**: represents the Y column name in the dataset, where Y represents the dependent variable in the Linear Model.
    Colum Y must be numeric;
*	**@...** : represents one or more datasets X columns names, where X represents the independent variables in the Linear Model.
      Columns X must be numerics.

*Return:*
	  
*	**@resultModel** a List containing both Predictions and the Linear Model coefficients.

You can find some sample datasets on *src/sample/data*. 

To run a regression model:

	houses <- linearModel("src/main/datahouses_set.csv", ",", 
	"SalePrice", "OverallQual", "YearBuilt", "MSSubClass", "TotRmsAbvGrd", "YearRemodAdd", "GarageCars", "OverallCond", "1stFlrSF", "2ndFlrSF", "TotalBsmtSF")-

## Checking Results	

The returned List contains:

*	**$Coefficients** : list containing the values found for the linear model coefficients 
*	**$Predictions** : matrix that shows the predicted values, the real values for Y and the E residuals
*	**$RMSErrorRate** : rating used to know how good our model is. A good model is always between -1< R <=1.
*	**$Dataset**: a matrix representation of the original dataset
	
## Outliers helper

In order to remove lines where the predicted values were uncommonly greater or lesser than the real ones,
you can try out the *removeOutliersLines* function. Providing the dataset and determining a range of
acceptable residuals, the function will remove the lines out of the informed range. You can
try to recreate the predictions using the newer dataset. 

	OriginalM         Predictions (~Y) , Residuals (E)
  
	x1   x2   y  | ~Y     E
	1|2   -1    4  |  3.8   0.2
	2|3   9.2  -8  |  -7.3  -0.7
	3|-1  0.9  0.9 |  3.6   2.6
	4|-2   4.5   1 |  -1    -2 
  
	newDataset <- removeOutliersLines(OriginalM, E, -1, 2.5)

	newDataset
    
	x1   x2   y
	1|2   -1    4
	2|3  9.2   -8
	
## Author

LinkedIn - [Adail Carvalho](https://www.linkedin.com/in/adail-carvalho-a34343106)
