# Ashton Bieri | github @ashtonbieri

# function that converts all files in a directory
# with space- or tab- delimited (.txt) to csv files
csvConvert <- function(directory="."){
   files <- list.files(path=directory,pattern="*.txt")
   for (fileIndex in 1:length(files)){
      temp <- read.table(file.path(directory,files[fileIndex]),sep="",header=TRUE)
      name <- tools::file_path_sans_ext(x=files[fileIndex])
      filename <- paste(name,".csv",sep='')
      write.csv(x=temp,file=file.path(directory,filename),row.names=FALSE)
   }
}

# compile data from all .csv files in a directory to one csv
# has 12 columns, plus country and dayOfYear
# user can choose to remove rows w/ NAs, get warning and include, or no warning
# handleNA allows 3 options for handling NA values: warnAndKeep, keepNA, and removeNA
# also allows to set directory, country and day of year if different from dir/filenames
catCsv <- function (directory = ".", handleNA = "warnAndKeep", country = NULL) {
   files <- list.files(path=directory, pattern="*.csv")
   # loop through files
   for (fileIndex in 1:length(files)){
      # in the first iteration: add column names, erase contents of csv(if any)
      if (fileIndex == 1){
         temp <- read.csv(file=file.path(directory,files[fileIndex]))
         temp$country <- NA
         temp$dayOfYear <- readr::parse_number(files[fileIndex])
         write.csv(x=temp,file=file.path(directory,"allData.csv"),append=FALSE,row.names=FALSE,col.names=TRUE)
      }else{ #append data to csv
         temp <- read.csv(file=file.path(directory,files[fileIndex]))
         temp$country <- NA
         temp$dayOfYear <- readr::parse_number(files[fileIndex])
         write.table(x=temp,file=file.path(directory,"allData.csv"),append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
      }
   }
   #if country isn't specified in parameters: use the folder name of the directory
   if(is.null(country)){
      if (directory=='.'){country=basename(getwd())}
      else{country=basename(directory)}}
   allData <- read.csv(file.path(directory,"allData.csv"),header=TRUE)
   allData$country <- country
   write.csv(allData,file.path(directory,"allData.csv"),row.names=FALSE)
}



# function summarizing compiled dataset
# for number of screens run, % of screened patients that were infected,
# male vs female, age distribution

dataSummary <- function (file="allData.csv") {
   data <- read.csv(file)
   writeLines(paste("There have been", nrow(data), "unique screens"))
   writeLines(paste("Out of the screens,",sum(rowSums(data[3:12])!=0),"were infected"))
   writeLines(paste("Total female patients:\t",sum(data$gender=="female")))
   writeLines(paste("Total male patients:\t",sum(data$gender=="male")))
   writeLines("Patients across age groups:\n0-17\t18-40\t41-65\t65+")
   writeLines(paste(sum(x$age %in% 0:17),sum(x$age %in% 18:40),sum(x$age %in% 41:65),sum(x$age %in% 65:120),sep="\t"))
}
