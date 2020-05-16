# UOPWaterPoloDataAnalytics
Data analytics of the comparison between men's and women's water polo teams' shots

## dataClean.R 
**1.** Contains a class that cleans a dataset it reads from a .csv file to fit the specific needs of the research


## dataExploration.R
**1.** Imports **dataClean.R**

**2.** Creates two gender-based objects of the *dataClean* class using *Men* and *Women* datasets taken .csv files (*default files: Men_Shots.csv and Women_Shots.csv*)

**3.** Combines both datasets into *dataset* variable distinguised by the *Gender* column (*"Male"*, *"Female"*)


## logisticModel.R
**1.** Imports **dataExploration.R**

**2.** Seperates the dataset into (0.6) training and and (0.4) testing datasets

**3.** Trains and tests logistic model using dataset


## dataVisualizations.R
**1.** Imports **logisticModel.R**

**2.** Produces heatMaps from the model
