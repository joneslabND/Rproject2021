###STEP (2) - compile csv

##2a: Create function to organize all csv file data in country Y
country = "Y"
dir <- setwd("~/Downloads/Notre Dame/Biocomputing/Rproject2021/")
csvcompiling <- function(dir,country) {
  # paste asks you what to put together and what to separate by 
  files<-list.files(path=paste(dir,"/country",country, sep=""), full.names=TRUE)
  
  # read in first file
  alldata <- read.csv(files[1], header=TRUE, stringsAsFactors = FALSE)
  alldata$country <- numeric(length=nrow(alldata))
  for (i in 1:length(alldata$country)){
    alldata$country[i] = country
  }
  alldata$DOY <- numeric(length=nrow(alldata))
  for (i in 1:length(alldata$DOY)){
    alldata$DOY[i] = 120
  }
  year = 121
  for(i in 2:length(files)){
    
    # open every file and add columns for country and DOY
    csvdata <- read.table(file=files[i], sep=',', header=TRUE,
                          stringsAsFactors=FALSE)
    csvdata$country <- numeric(length=nrow(csvdata))
    for (i in 1:length(csvdata$country)){
      csvdata$country[i] = country
    }
    csvdata$DOY <- numeric(length=nrow(csvdata))
    for (i in 1:length(csvdata$DOY)){
      csvdata$DOY[i] = year
    }
    # combine all files and fill out year column
    alldata <-rbind(alldata,csvdata)
    year = year + 1 
  }
  # manage NA conditionals 
  if(anyNA(alldata) == TRUE){
    print("NA present.")
    yourresponse<-readline(prompt="Please select from following:
                1) Delete NA rows
                2) Include NA rows with warning
                3) Include NA rows, excluding warning")
    if(yourresponse == "1"){
      final_countries<-na.omit(alldata)
    }else if(useranswer == "2"){
      print("Warning: NA present!")
      return(yourresponse)
    }else if(yourresponse == "3"){
      return(final_countries)
    }else{
      print("Invalid selection. Please select a choice from 1-3.")
    }
    return(alldata)
  }
  return(alldata)
}
# call the function
dfy <- csvcompiling(dir,country)
write.csv(dfy,"~/Downloads/Notre Dame/Biocomputing/Rproject2021/dfy.csv",row.names=TRUE)

##2b: Repeat previous function with country X
dir <- setwd("~/Downloads/Notre Dame/Biocomputing/Rproject2021/")
country = "X"
# call the function
dfx <- csvcompiling(dir,country)
write.csv(dfx,"~/Downloads/Notre Dame/Biocomputing/Rproject2021/dfx.csv",row.names=TRUE)

##2c: Bring data from both countries into one single csv file 
mergecsv<-function(dir){
  write.csv(rbind(dfx, dfy),"finalcombineddata.csv")
  finalmerging<-read.csv("finalcombineddata.csv")
  return(finalmerging)
}
# Call the function 
finalcombineddata<-mergecsv(dir)
