#### Supporting Functions ####

#convert txt to csv (only in countryY) 
convert_to_csv <- function(directory){
setwd(directory)
for (i in 1:length(FILES)){
  FILES <- list.files(pattern = ".txt")
  FILE=read.table(file=FILES[i],header=T)
  write.table(FILE,file=paste0("directory3",sub(".txt","",FILES[i]),".csv"),row.names=F,quote=F,sep=",")
} 
}

#compile all into one file called alldata
compileX <- function(directory){
  #define a set of files of all the files 
  setwd(directory)
  files <- list.files(path=directory, pattern=".csv", full.names=FALSE, recursive=FALSE)
  alldata<- data.frame()
  
  #for loop over the rest of the files 
  for(i in 1:length(files)){
    if(i==1){
      alldata <- rbind(alldata, (read.csv(files[i], header=TRUE, sep=",")))
      alldata$country <- "X"
      alldata$dayofYear <- substr(files[i], 8,10)
    }else{
      output <- read.csv(file=files[i], header=TRUE, sep=",")
      #getting the text into the two new columns 
      output$country<-"X"
      #have to create some variable that labels the day of the year 
      #basically telling it which position you want to cut out in the file name 
      output$dayofYear <- substr(files[i],8,10) 
      alldata <- rbind(alldata, output)
    }
  }
      #handle NA's
  for(i in 1:length(alldata)){
    if(sum(alldata[,]=="NA")>0){
      input <- readline(prompt=paste("Warning: File", i, "includes NA's. Type Y to override. Type Remove to remove rows with NA's"))
      if(input!="Y"){
      }else if(input!="Remove"){
        alldata <- na.omit(alldata)
      }
    }
  }
  return(alldata)
}
  
alldata <- compileX(directory)

#adding country Y files to alldata 
compileY <- function(directory){
  #define a set of files of all the files 
  setwd(directory)
  files <- list.files(path=directory, pattern=".csv", full.names=FALSE, recursive=FALSE)
  alldata<- data.frame()
  
  #for loop over the rest of the files 
  for(i in 1:length(files)){
    if(i==1){
      alldata <- rbind(alldata, (read.csv(files[i], header=TRUE, sep=",")))
      alldata$country <- "Y"
      alldata$dayofYear <- substr(files[i], 8,10)
    }else{
      output <- read.csv(file=files[i], header=TRUE, sep=",")
      #getting the text into the two new columns 
      output$country<-"Y"
      #have to create some variable that labels the day of the year 
      #basically telling it which position you want to cut out in the file name 
      output$dayofYear <- substr(files[i],8,10) 
      alldata <- rbind(alldata, output)
    }
  }
  #handle NA's
  for(i in 1:length(alldata)){
    if(sum(alldata[,]=="NA")>0){
      input <- readline(prompt=paste("Warning: File", i, "includes NA's. Type Y to override. Type Remove to remove rows with NA's"))
      if(input!="Y"){
      }else if(input!="Remove"){
        alldata <- na.omit(alldata)
      }
    }
  }
  return(alldata)
}

alldata <- rbind(alldata, compileY(directory)


#summarize the data once all compiled
#1 summary = number of test screens ran 
num_screens <- function(directory){
num_rows <- nrow(alldata)
return(num_rows)
}
#This is the number of screens ran 
num_screens(directory)

#2 summary-percent of patients that were infected 
percent <- function(directory){
  infected <- 0
  num_rows <- nrow(alldata)
for(row in 1:length(alldata$marker01)){
  if((alldata$marker01[row]=="1")){
    infected <- infected + 1
  }else if(alldata$marker02[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker03[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker04[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker05[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker06[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker07[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker08[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker09[row]=="1"){
    infected <- infected + 1
  }else if(alldata$marker10[row]=="1"){
    infected <- infected + 1
  }
}
  decimal_infected <- infected/num_rows
  percent_infected <- decimal_infected*100
  return(percent_infected)
}

#This the percent of patients infected 
percent(directory) 

#3 summary-number of male versus female patients 
#This graph offers another visualization of the males versus females 

library(ggplot2)
male_versus_female <- function(directory){
ggplot(data=alldata, aes(x=gender)) + geom_bar() + theme_classic()
}
male_versus_female(directory)

number_of_males <- nrow(alldata[alldata$gender=="male",])
number_of_females <- nrow(alldata[alldata$gender=="female",])
#This is the number of males who were patients 
number_of_males
#This is the number of females who were patients 
number_of_females

#4 summary - age distribution of patients 
#This graph shows the density of ages among the patients 
library(ggplot2) 
ages <- function(directory){
ggplot(data=alldata, aes(x=age)) + geom_density() + theme_classic() 
}

ages(directory)

       