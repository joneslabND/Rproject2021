###R Project, Biocomputing Fall 2021
###Loan Duong
###Second script: Uses the source function to load the functions defined in
###first script, compiles all data into a CSV file, process the data included in
###the entire data set to answer the questions, and provide graphical evidence.

##Load functions from supportingFunctions.R
source("supportingFunctions.R")

##Compiles allData.csv file
CSVFunction()
CompileCSVFunction("CountryX")
CompileCSVFunction("CountryY")
allDataFunction()

##Process the data
summary()

##Answer the questions.
Question1()
Question2()
