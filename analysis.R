# Question 1: In which country (X or Y) did the disease outbreak likely begin?
# Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?


#Assumptions:
# path= "~/Desktop/R Studio Projects/Rproject2021/"
#objects in Rproject2021:
  #countryX folder
  #countryY folder
  #allData.csv
  #convert.R
  #compile.R
  #summary.R
  #analysis.R

#Go to working directory
setwd(dirname(getwd()))
#convert .txt to .csv
source("convert.R")
#compile all .csv files
source("compile.R")
#summarize data
source("summary.R")

#From plots 'C' and 'D' found in the summary, we see ...
  # Country X: greater number of bacteria markers, concentrated in 1-5
  # Country Y: smaller number of markers present, but more distribution 
# More distribution means that the the bacteria mutated more in Country Y compared to X.
# As a result, a vaccine from Country Y would not work for Country X 
# because variability in the disease would make it difficult to provide immunity.


##Where did the disease originate
data <- read.csv("allData.csv", header=TRUE, sep= ",", stringsAsFactors=FALSE)

#Number of screens for each country

dfx #compiled data for Country X
dfy #compiled data for Country Y

# compile data for screens for Total, Country X, and Country Y

  X<-read.csv("dfx.csv")
  Y<-read.csv("dfy.csv")
  X[X==0]<-NA
  XPos<-X[rowSums(is.na(X[,4:13]))!=10,]
  Y[Y==0]<-NA
  YPos<-Y[rowSums(is.na(Y[,4:13]))!=10,]
  XHist<-hist(XPos$DOY, breaks=20)
  XHist
  YHist<-hist(YPos$DOY, breaks=20)
  YHist
  plot(XHist, col=rgb(0,0,1, 1/4),
       xlab="Day of the Year", ylab="Infected",
       breaks=20)
  plot(YHist, col=rgb(1,0,0, 1/4),add=T)
  legend(x="topleft", 
         legend=c("Country X", "Country Y"),
         fill=c(rgb(0,0,1, 1/4), rgb(1,0,0, 1/4)), border="black")
  
###What is the difference between cases in the two countries?
  
  newDataset = read.csv("allData.csv")
  countx = 0
  county = 0
  for (i in 1: nrow(newDataset)){
    for (j in 3:12)
      if (newDataset[i,j]==1 && newDataset$country[i] == "X"){
        countx = countx +1
        break
      }else if (newDataset[i,j]==1 && newDataset$country[i] == "Y"){
        county = county +1
        break
      }else{
      }
  }
print(paste0("Country X infected patients =  ",countx))
print(paste0("Country y infected patients =  ",county))
  difference = countx - county
print(paste0("Country X  has ", difference ," more infected patients than Country Y "))
print(paste0("As seen in this histogram, Country X began having cases earlier in the year than Country Y. Furthermore, Country X has more cases than Country Y." ))
  