## Functions for R project

# Write a function that converts all files in a directory with space- or tab-delimited data (.txt) 
# into comma-separated value files.

commaConvert <- function(directory) {
  #set directory
  setwd(directory)
  #create a list of files from target directory
  file_list <- list.files(path = ".", pattern = ".txt", full.names = TRUE)
  #read files
  for (i in 1:length(file_list)) {
    input <- file_list[i]
    output <- paste0(gsub("\\.txt$", "", input), ".csv")
    data = read.table(input, header = TRUE)
    write.table(data, file = output, sep=",", col.names=TRUE, row.names=FALSE)
  }
}


# Installing packages
library(dplyr)
library(plyr)
library(readr)

# Combining all data from the .csv files in each country's directory to one .csv file. Add country and dayofYear columns.
# Offer options for how to handle the entries with NAs.

csvCombine = function(dir){
  setwd(dir)
  # NOTE: directory must be the country's directory
  fileNames=list.files(path=dir, pattern="*.csv", full.names=TRUE)
  directories=dirname(path=fileNames)
  allFiles=fileNames %>% 
    lapply(read_csv) %>% 
    bind_rows
  allFiles=data.frame(matrix(ncol = 12, nrow = 0))
  # Asks whether the user wants to be warned about NAs in the data
  A=as.character(readline(prompt="Be warned about NAs (Y or N): "))
  for (i in 1:length(fileNames)){
    if (A=="Y"){
      addFile=read_csv(fileNames[i])
      addFile$country=sub(".+([A-Z])", "\\1", directories[i])
      addFile$dayofYear=sub(".+([0-9]{3}).*","\\1",fileNames[i])
      allFiles=rbind(allFiles,addFile)
      if(TRUE %in% is.na(addFile)){
        # Warns user of an NA in the data
        print(paste0((fileNames[i])," contains NA"))
        # Asks whether the user wants to remove rows with NAs in the data
        B=as.character(readline(prompt="Remove rows with NAs (Y or N): "))
        if (B=="Y"){
          addFile=na.omit(addFile)
        }else{
          addFile=read_csv(fileNames[i])
          addFile$country=sub(".+([A-Z])", "\\1", directories[i])
          addFile$dayofYear=sub(".+([0-9]{3}).*","\\1",fileNames[i])
          allFiles=rbind(allFiles,addFile)
        }
      }else{
        addFile=read_csv(fileNames[i])
        addFile$country=sub(".+([A-Z])", "\\1", directories[i])
        addFile$dayofYear=sub(".+([0-9]{3}).*","\\1",fileNames[i])
        allFiles=rbind(allFiles,addFile)
      }
    }else{
      addFile=read_csv(fileNames[i])
      addFile$country=sub(".+([A-Z])", "\\1", directories[i])
      addFile$dayofYear=sub(".+([0-9]{3}).*","\\1",fileNames[i])
      allFiles=rbind(allFiles,addFile)
    }
  }
  write.csv(allFiles,file="csvCombine",row.names=FALSE)
}

# Write a function to summarize the compiled data set in terms of number of screens run, percent of
# patients screened that were infected, male vs. female patients, and the age distribution of patients.

summarizeData <- function(file) {
  input <- read.csv(file, header = TRUE)
  #Number of screens run
  cat(paste0("Number of screens run: ", nrow(input), "\n"))
  #Percent of patients infected
  data = input[-c(2,14)]
  sumPatients <- 0
  for (row in 1:nrow(data)) {
    patient <- grepl(pattern = "1", x = row)
    if (patient==TRUE) {
      sumPatients=sumPatients+1
    } 
  }
  percentage = (sumPatients/(nrow(data)))*100
  cat(paste0("Percentage of patients screened that were infected: ", percentage, " %\n"))
  #Female vs. Male patients
  sumFemale <- 0
  sumMale <- 0
  for (i in 1:nrow(input)) {
    if (input$gender[i]=="female") {
      sumFemale=sumFemale+1
    } else if (input$gender[i]=="male") {
      sumMale=sumMale+1
    }
  }
  cat(paste0(sumFemale, " females were tested and ", sumMale, " males were tested.\n"))
  #Age Distribution
  cat(paste0("Statistics concerning age are as follows:\n"))
  print(summary(input$age))
}
