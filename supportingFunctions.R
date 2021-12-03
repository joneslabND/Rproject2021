###R Project, Biocomputing Fall 2021
###Loan Duong
###First script: a number of custom functions for data handling or summary tasks

##Convert all files in a directory with space- or tab-delimited data into csv
#files. The function name is CSV function and will accept the name of a
#directory that contains .txt files. It defaults to the CountryY directory.
CSVFunction<-function(TXTDir="countryY"){
  cat("Loading...")
  cat("\n")
TXTfiles <- list.files(TXTDir, pattern = ".txt")
TXTfilesIn <- paste(TXTDir, TXTfiles, sep="/") 
CSVfilesOut <- paste(TXTDir, paste0(sub(".txt","", TXTfiles),".csv"), sep="/")
for (i in 1:length(TXTfilesIn)) {
  temp <- (read.table(TXTfilesIn[i], header = TRUE, sep=" "))
  write.csv(temp, file = CSVfilesOut[i], row.names = FALSE)
}
cat("All screening files have been converted to .csv and can be found in their source directory.")
}

##Compile data from all csv files in a directory into a single csv file,
##maintaining original 10 columns as well as country & day of year columns. This
##function accepts a directory name and will create a compilation of all the csv
##files within that directory, creating a new csv within the same directory.
CompileCSVFunction<-function(dirToCompile){
  cat("Loading...")
  cat("\n")
  file.list<-list.files(dirToCompile, pattern = "\\d.csv")
  initDataFrame<-data.frame()
  CSVtoRead <- paste(dirToCompile, file.list, sep="/")
  for (i in 1:length(file.list)){
    readIn<-read.csv(CSVtoRead[i],header=TRUE)
    readIn$country = substr(dirToCompile,8,8)
    readIn$day = substr(CSVtoRead[i],17,19)
    initDataFrame<-rbind(initDataFrame,readIn)
    write.csv(initDataFrame,
              file=paste(dirToCompile,
                         paste(dirToCompile,"compiled.csv"), sep="/"), row.names = FALSE)
  }
  cat("Compiled .csv been generated for selected directory and can be found in its original directory.")
}
#Code to compile the compiled data from country's x and y into a single file, allData.csv
allDataFunction<-function(){
  cat("Loading...")
  cat("\n")
  X<-read.csv("countryX/countryX compiled.csv")
  Y<-read.csv("countryY/countryY compiled.csv")
  allData<-rbind(X, Y)
  write.csv(allData, file="allData.csv", row.names = FALSE)
  allData[allData==0]<-NA
  cat("allData.csv has been created. This contains all screening data for both countries.")
  NAsChoice<-readline(prompt="Would you like a .csv with all negative screenings removed? Type Y or N. ")
  if(NAsChoice=="N"){
    cat(paste("Rows with NAs have been preserved. See allData.csv."))
  }else{
  if(NAsChoice=="n"){
      cat(paste("Rows with NAs have been preserved. See allData.csv."))
  }else{
    PosScreeningsOnly<-allData[rowSums(is.na(allData[,3:12]))!=10,]
    write.csv( PosScreeningsOnly, file="PositiveScreeningsOnly.csv", row.names=FALSE)
    cat(paste("Negative screenings have been omitted. See PositiveScreeningsOnly.csv."))
  }}}
##Summarize the compiled data set in terms of number of screens run, percent of
##patients screened that were infected, M vs F patients, and the age
##distribution of patients.
summary<-function(){
  allData<-read.csv("allData.csv")
  allData[allData==0]<-NA
  numScreens<-nrow(allData)
  positiveCasesOnly<-allData[rowSums(is.na(allData[,3:12]))!=10,]
  positiveCases<-nrow(positiveCasesOnly)
  cat("Here is a brief summary of the screening data.")
  cat("\n")
  cat(paste("Total number of screens:"), numScreens)
  cat("\n")
  InfectRatio<-positiveCases/numScreens
  InfectPercent<-round(InfectRatio*100, digits=2)
  cat(paste("Percent positive cases:", InfectPercent, "%", sep=" "))
  cat("\n")
  MalePts<-sum(allData$gender=="male")
  FemalePts<-sum(allData$gender=="female")
  cat(paste("Number of Male Patients Screened:"), MalePts)
  cat("\n")
  cat(paste("Number of Female Patients Screened:"), FemalePts)
  cat("\n")
  options(warn=-1)
  library(ggplot2)
  ageDistribution<-ggplot(allData,aes(x=age))+
    geom_histogram(bins=15, binwidth = 3, color="blue", fill="white")+
    ggtitle("Figure 1. Age Distribution of Screened Patients",)+
    ylab("Number of patients")+
    scale_x_continuous(breaks=seq(0,120,10), limits=c(0,120))+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))
  cat("The age distribution of patients is visualized in Figure 1.")
  ageDistribution
}

##Extra functions. XvYPlot plots the number of positive screens over time. For
##Q1.
XvYPlot<-function(){
  X<-read.csv("countryX/countryX compiled.csv")
  Y<-read.csv("countryY/countryY compiled.csv")
  X[X==0]<-NA
  XPos<-X[rowSums(is.na(X[,3:12]))!=10,]
  Y[Y==0]<-NA
  YPos<-Y[rowSums(is.na(Y[,3:12]))!=10,]
  XHist<-hist(XPos$day, breaks=20)
  XHist
  YHist<-hist(YPos$day, breaks=20)
  YHist
  plot(XHist, col=rgb(0,0,1, 1/4),
       main="Figure 2. Number of Positive Cases in Countries X and Y over Time",
       xlab="Screening Day", ylab="Positive Tests",
       breaks=20)
  plot(YHist, col=rgb(1,0,0, 1/4),add=T)
  legend(x="topleft", 
         legend=c("Country X", "Country Y"),
         fill=c(rgb(0,0,1, 1/4), rgb(1,0,0, 1/4)), border="black")
}
#For q2. Markers profile. 
MarkerProfile<-function(){
  X<-read.csv("countryX/countryX compiled.csv")
  Y<-read.csv("countryY/countryY compiled.csv")
  X[X==0]<-NA
  XPos<-X[rowSums(is.na(X[,3:12]))!=10,]
  Y[Y==0]<-NA
  YPos<-Y[rowSums(is.na(Y[,3:12]))!=10,]
  XMarkers<-colSums(XPos[3:12], na.rm=TRUE)
  YMarkers<-colSums(YPos[3:12], na.rm=TRUE)
  plot(XMarkers, main="Figure 3. Marker Profiles", col="blue", xlab="Marker", ylab="Count")
  axis(1, at=1:10, labels=c(1:10))
  lines(YMarkers, col="red", type = "p")
}

#Analysis text
Question1<-function(){
  cat("Question 1. In what country did the outbreak likely begin?")
  cat("\n")
  cat("The disease likely began in Country X. Plotting positive tests over time shows that it first appeared in Country X on day 120, and steadily grew while it didn't appear until day 139 in Country Y.")
  cat("\n")
  cat("This is visualized in Figure 2.")
  XvYPlot()
}
Question2<-function(){
  cat("Question 2. If Country Y develops a vaccine for the disease, is it likely to work for country X?")
  cat("\n")
  cat("It will likely NOT work. While Country Y's positive tests mostly resulted in positive hits for markers 6-10, Country X's positive tests were mostly hits on Markers 1-5.")
  cat("\n")
  cat("This is visualized in Figure 3.")
  MarkerProfile()
}