# R-Linear-Model

A program to perform Linear Regression analysis over files.

## Features

*	Simple Linear Regression: generates a regression model based on a X independent column and a Y dependent column
*	Multivariant Linear Regression: generates a regression model based on multiple X independent columns and a Y dependent column

## Requirements

*	**matlib** : package used to make matrix math.
	install.packages("matlib")
	
## Usage

The entry point for the program is the method linearModelMain(). 

*	**@filePath**: CSV file that represents the dataset;
*	**@delimiter**: the CSV file delimiter (if its a TSV, then use "\t");
*	**@dependentVar**: represents the Y column name in the dataset, where Y represents the dependent variable in the Linear Model.
    Colum Y must be numeric;
*	**@...** : represents one or more datasets X columns names, where X represents the independent variables in the Linear Model.
      Columns X must be numerics.

*Return:*
	  
*	**@resultModel** a List containing both Predictions and the Linear Model coefficients.

You can find some sample datasets on *src/main/data*. 

To run a regression model:

	houses <- linearModelMain("src/main/datahouses_set.csv", ",", 
	"SalePrice", "OverallQual", "YearBuilt", "MSSubClass", "TotRmsAbvGrd", "YearRemodAdd", "GarageCars", "OverallCond", "1stFlrSF", "2ndFlrSF", "TotalBsmtSF")-

## Cheking Results	

The returned List contains:

*	**$Coefficients** : list containing the values founded for the linear model coeficients 
*	**$Predictions** : matrix that shows the predicted values, the real values for Y and the E residuals
*	**$RMSErrorRate** : rating used to know how good our model is. Value is always between -1< R <=1.
*	**$PossibleOutliersNumLines**: show lines containing both min anx max residuals values.
	
## Author

Contact me through [LinkedIn](https://www.linkedin.com/in/adail-carvalho-a34343106)