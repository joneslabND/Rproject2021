setwd("C:/Users/megan/Desktop/RProject/RProject2021")
direct <- list.dirs(".")
library(reshape2)
alltxt <- c()
#finds all the text files in each directory
for (i in 1:length(direct)){
    alltxt <- c(alltxt, list.files(dirs[i], pattern = ".txt", full.names = TRUE))
}

#change files in the directory that are .txt files into .csv files
for (i in 1:length(alltxt)){
  input<-read.table(alltxt[i], sep = "", stringsAsFactors = FALSE, header=TRUE)
  out <- sub(".txt", ".csv", alltxt[i])
  write.table(input, file=out, sep =",", col.names = TRUE, row.names = FALSE)
}


#finds all the csv files in each directory
files <- function(){
  dirs <- list.files(( path = "."))
  allfiles <- c()
  for (k in 1:length(dirs)){
  allfiles <- c(allfiles, list.files(dirs[k], pattern = ".csv", full.names = TRUE))
  }
}
userNAresponse <- readline(prompt="How would you like to deal with null values in the data? Respond with 1 to remove NAs. Respond with 2 to keep NAs but with a warning. Respond with 3 to include NAs without a warning.")

#add all csv files into a single file with two added columns: country and DayofYear
    dirs <- list.dirs(("../RProject2021/"))
    library(dplyr)
    #establishes the first file to be compiled and df
    df <- read.csv(allfiles[1], header = TRUE)
    if(grepl("countryX", allfiles[1])==TRUE){
      df$country <- "X"
    }else{
      df$country <- "Y"
    }
    day <- regmatches(allfiles[1], regexpr("[0-9].*[0-9]", allfiles[1]))
    df$dayofYear <- day
    for (i in 2:length(allfiles)){
      d <- read.csv(allfiles[i], header = TRUE)
      if(userNAresponse==1){
        na.omit(d)
      }else if(userNAresponse==2){
        # code for keeping NAs with warning
        print("Null values will not be removed from this file")
      }else{
        # code for keeping NAs without warning
      }
      #adds X for country X
      if(grepl("countryX", allfiles[i])==TRUE){
        d$country <- "X"
      }
      #adds Y for country Y
      if (grepl("countryY", allfiles[i])==TRUE){
        d$country <- "Y"
      }
      #adds the day of the year
      day <- regmatches(allfiles[i], regexpr("[0-9].*[0-9]", allfiles[i]))
      d$dayofYear <- day
      df <- bind_rows(df, d)
    }
    allD <- as.data.frame(df)
    #writes a file with all compiled data
    write.csv(allD, file = "allOfData.csv", col.names = TRUE, row.names = FALSE)
    allDa <- read.csv("allOfData.csv", header = TRUE, stringsAsFactors = FALSE)
    print("Type in summary(allDa) to view the statisitcs of all the data")
#given compiled data
allDe <- read.csv("allData.csv", header = TRUE, stringsAsFactors = FALSE)

#analysis of compiled data
summary <- function(allD){
library(ggplot2)
  #number of screens run
  total <- nrow(allD)
  print(paste("The total number of screens run was", as.character(total)))
  #number of infected patients
  numOfInfected <- 0
for (j in 1:total){
  for (i in 1:ncol(allD)){
    if(allD[j,i] == 1){
      numOfInfected = numOfInfected + 1
      break
    } else{
      numOfInfected = numOfInfected
    }
  }
}
  percent <- (numOfInfected/total)*100
  print(paste("The number of infected patients is" , numOfInfected))
  print(paste("The percent of infected patients is" , percent, "%"))
  #male vs female patients
  fem <- 0
  male <- 0
  for (j in 1:total){
    if(allD[j,1] == "female"){
      fem = fem +1
    }else{
      male =male +1
    }
  }
  print(paste("The number of female vs male patients:" , fem, "vs", male))
#age distribution of patients
  dis <- sort(allD$age, decreasing = TRUE)
  over100 <- 0
  zeroto20 <- 0
  twentyto40 <- 0
  fourtyto60 <- 0
  sixtyto80 <- 0
  eightyto100 <- 0
  for (i in 1:length(dis)){
    if (dis[i]> 100){
      over100 <- over100 +1
    }
    if (dis[i] <=100 & dis[i] > 80){
      eightyto100 = eightyto100 +1
    }
    if (dis[i] <= 80 & dis[i] > 60){
      sixtyto80 = sixtyto80 +1
    }
    if (dis[i] <=60 & dis[i] > 40) {
      fourtyto60 = fourtyto60 +1
    }
    if (dis[i] <= 40 & dis[i] > 20){
      twentyto40 = twentyto40 + 1
    }
    if(dis[i] <= 20){
      zeroto20 = zeroto20 +1
    }
  }
  agedis=matrix(0,nrow=0,ncol = 6)
  colnames(agedis) <- c("under 20", "20-40", "40-60", "60-80", "80-100", "over 100")
  counts <- c(zeroto20,twentyto40,fourtyto60,sixtyto80,eightyto100,over100)
  agedistribution <- rbind(agedis,counts)
  library(dplyr)
  aged<-ggplot(allD,aes(age))+
    geom_histogram(stat = "count", fill = "black")+
    theme_classic()+
    xlim(0,125)+
    labs(title = "Age Distribution (Outliers of over 125 not shown)")
  ggsave("AgeDistribution.png", aged, width =5, height =5)

#number of people from each country with each marker 
      xcountry <- allD[allD$country == "X",]
      vectx <- c()
      for (i in 3:12){
      vectx <- c(vectx, sum(xcountry[,i]))
      #print(paste("The number of people from country X with marker" , i-2, "is", sum(xcountry[,i])))
        
      }
      ycountry <- allD[allD$country == "Y",]
      vecty <- c()
      for (i in 3:12){
        vecty <- c(vecty, sum(ycountry[,i]))
       # print(paste("The number of people from country Y with marker" , i-2, "is", sum(ycountry[,i])))
      }
#the number of infected people of each country
      numOfInfectedX <- 0
      for (j in 1:nrow(xcountry)){
        for (i in 1:ncol(xcountry)){
          if(xcountry[j,i] == 1){
            numOfInfectedX = numOfInfectedX + 1
            break
          } else{
            numOfInfectedX = numOfInfectedX
          }
        }
      }
      print(paste("The number of infected patients in country X is" , numOfInfectedX))
      numOfInfectedY <- 0
      for (j in 1:nrow(ycountry)){
        for (i in 1:ncol(ycountry)){
          if(ycountry[j,i] == 1){
            numOfInfectedY = numOfInfectedY + 1
            break
          } else{
            numOfInfectedY = numOfInfectedY
          }
        }
      }
      print(paste("The number of infected patients in country Y is" , numOfInfectedY))
      
      y <- c("country", "marker 1", "marker 2","marker 3","marker 4","marker 5","marker 6","marker 7",
             "marker 8","marker 9","marker 10")
      A=matrix(0,nrow=0,ncol=11)
      colnames(A) = y
      rowX <- c("X", as.numeric(vectx))
      rowY <- c("Y", as.numeric(vecty))
      B <- rbind(A, rowX)
      C <- rbind(B, rowY)
      summ <- as.data.frame(C)
      #makes the data readable for ggplot
      summs <- melt(summ, id.vars = "country")
      summs$value <- as.numeric(summs$value)
      PatientAges<- as.data.frame(agedistribution)
      print(summs)
      print(PatientAges)
      #type of marker vs amount of each marker for each country
      library(ggplot2)
      MarkerGraph <-ggplot(summs, aes(variable, value, color = country))+
        geom_point()+
        theme_classic()+
        xlab("Maker Types for the Virus") +
        ylab("Number of Infections with each marker type") +
        theme(axis.text.x = element_text(angle=65, vjust=0.6))+
        scale_y_continuous()
      MarkerGraph
      ggsave("markerGraph.png", plot = MarkerGraph, width =5, height = 5)
      
#total number of infections per day in each country
      days <- c(120:175)
      vectoX <- c()
      vectoY <- c()
      for (b in 120:175) {
        beginning <- allD[allD$dayofYear == b,]
        beginningY <- beginning[beginning$country == "Y",]
        numOfInfectedYinB <- 0
        for (j in 1:nrow(beginningY)){
          for (i in 1:ncol(beginningY)){
            if(ycountry[j,i] == 1){
              numOfInfectedYinB = numOfInfectedYinB + 1
              break
            } else{
              numOfInfectedYinB = numOfInfectedYinB
            }
          }
        }
        
        beginningX <- beginning[beginning$country == "X",]
        numOfInfectedXinB <- 0
        for (j in 1:nrow(beginningX)){
          for (i in 1:ncol(beginningX)){
            if(xcountry[j,i] == 1){
              numOfInfectedXinB = numOfInfectedXinB + 1
              break
            } else{
              numOfInfectedXinB = numOfInfectedXinB
            }
          }
        }
        vectoX <- c(vectoX, numOfInfectedXinB)
        vectoY <- c(vectoY, numOfInfectedYinB)
      }
      library(reshape2)
      InfectedPerDay <- data.frame(days, vectoX, vectoY)
      colnames(InfectedPerDay) <- c("days", "# of infected in X", "# of infected in Y")
    #makes it readable for ggplot
    InfectedPerDayymelt <- melt(InfectedPerDay, id.vars = "days")
    print(InfectedPerDay)
#dayOfYear vs infected count with different colors for each country
InfectionvsDay<- ggplot(InfectedPerDayymelt, aes(days,value, color = as.factor(variable)))+
  geom_line()+
  xlab("Day of Year") +
  ylab("count of infection") +
  theme(legend.title=element_blank())+
  theme_classic()
InfectionvsDay
ggsave("InfectionVsDay.png",InfectionvsDay, width = 5, height = 5)
#ggplot(InfectedPerDay, aes(days))+
 # geom_col()

#the total number of infected people over the first 5 days in each country
      beginning <- allD[allD$dayofYear < 125,]
      beginningY <- beginning[beginning$country == "Y",]
      numOfInfectedYinB <- 0
      for (j in 1:nrow(beginningY)){
        for (i in 1:ncol(beginningY)){
          if(ycountry[j,i] == 1){
            numOfInfectedYinB = numOfInfectedYinB + 1
            break
          } else{
            numOfInfectedYinB = numOfInfectedYinB
          }
        }
      }
      
      beginningX <- beginning[beginning$country == "X",]
      numOfInfectedXinB <- 0
      for (j in 1:nrow(beginningX)){
        for (i in 1:ncol(beginningX)){
          if(xcountry[j,i] == 1){
            numOfInfectedXinB = numOfInfectedXinB + 1
            break
          } else{
            numOfInfectedXinB = numOfInfectedXinB
          }
        }
      }
      print(paste("The total number of people infected in country Y on day 125 is", numOfInfectedYinB))
      print(paste("The total number of people infected in country X on day 125 is", numOfInfectedXinB))
      
}
