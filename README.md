# R-Linear-Model

A program to perform Linear Regression analysis over files.

## Features

*	Simple Linear Regression: generates a regression model based on a X independent column and a Y dependent column
*	Multivariant Linear Regression: generates a regression model based on multiple X independent columns and a Y dependent column

## Requirements

The following packages need to be installed before running the program:

*	**matlib** : package used to make matrix math

Installing matlib:

	install.packages("matlib")
	
## Usage

The entry point for the program is the method linearModelMain(). 

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

	houses <- linearModelMain("src/main/datahouses_set.csv", ",", 
	"SalePrice", "OverallQual", "YearBuilt", "MSSubClass", "TotRmsAbvGrd", "YearRemodAdd", "GarageCars", "OverallCond", "1stFlrSF", "2ndFlrSF", "TotalBsmtSF")-

## Cheking Results	

The returned List contains:

*	**$Coefficients** : list containing the values found for the linear model coefficients 
*	**$Predictions** : matrix that shows the predicted values, the real values for Y and the E residuals
*	**$RMSErrorRate** : rating used to know how good our model is. A good model is always between -1< R <=1.
*	**$PossibleOutliersNumLines**: show lines containing both min and max residuals values.
	
## Author

Contact me via [LinkedIn](https://www.linkedin.com/in/adail-carvalho-a34343106)
