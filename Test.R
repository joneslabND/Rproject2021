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

# Write a function to compile data from all .csv files in a directory into a single .csv file. The compiled
# data should have the original twelve columns from daily data sheets, but also country and dayofYear
# columns. The user should be able to choose whether they want to remove rows with NA’s in any
# columns, include NAs in the compiled data but be warned of their presence, or include NAs in the
# compiled data without a warning

## In other document! 

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
