#### Supporting Functions R Script - Biocomp R Project
### Dianna Perez and Abi Batkoff


library(ggplot2)

## tab/space delimited file converter to comma delimited

txt2csv <- function(directory) {
  #define set of files and loop
  files <- list.files(path=directory, full.names = TRUE)
  
  for(i in 1:length(files)){
    input <- files[i]
    output <- paste0(gsub("\\.txt$", "", input),".csv")
    data.csv <- read.table(file=input, header=TRUE, stringsAsFactors = FALSE)
    write.csv(data.csv, file=output, col.names=TRUE, row.names=FALSE)
    file.remove(input)
  }
}
#txt2csv("/Users/abigaylebatkoff/Downloads/MBA Year 1/Biocomputing/RProject2021/countryY")



compileY <- function(directory){
  #list files
  setwd(directory)
  files <- list.files(path=directory, pattern=".csv", full.names=FALSE, recursive=FALSE)
  alldata<- data.frame()
  
  #for loop to add filesafter the first one
  for(i in 1:length(files)){
    if(i==1){
      alldata <- rbind(alldata, (read.csv(files[i], header=TRUE, sep=",")))
      alldata$country <- "X"
      alldata$dayofYear <- substr(files[i], 8,10)
    }else{
      output <- read.csv(file=files[i], header=TRUE, sep=",")
      #fill in the two new columns
      output$country<-"X"
      #extract day of Year from file name 
      output$dayofYear <- substr(files[i],8,10) 
      alldata <- rbind(alldata, output)
    }
  }
  answer <- readline(prompt="Do you want to remove rows with NA values? (hit 1 if yes) Do you want to include NA values
                     but be warned of their presence? (hit 2 if yes) Do you want to include NA values without a warning (hit 3 if yes)")
  as.numeric(answer)
  if( answer == 1){
    #remove all rows with NAs
    alldata <- na.omit(alldata)
  }else if(answer == 2 && sum(is.na(alldata)) > 0){
    #remove all rows with NAs with a warning
    print("Warning: this compiled data contains NA values")
  }else{
    #keep all NAs without a warning
    return(alldata)
  }
  return(alldata)
}

#dataY <- csvcompilerY("/Users/abigaylebatkoff/Downloads/MBA Year 1/Biocomputing/RProject2021/countryY")


compileX <- function(directory){
  #list files
  setwd(directory)
  files <- list.files(path=directory, pattern=".csv", full.names=FALSE, recursive=FALSE)
  alldata<- data.frame()
  
  #for loop to add filesafter the first one 
  for(i in 1:length(files)){
    if(i==1){
      alldata <- rbind(alldata, (read.csv(files[i], header=TRUE, sep=",")))
      alldata$country <- "X"
      alldata$dayofYear <- substr(files[i], 8,10)
    }else{
      output <- read.csv(file=files[i], header=TRUE, sep=",")
      #fill in the two new columns 
      output$country<-"X"
      #extract day of Year from file name  
      output$dayofYear <- substr(files[i],8,10) 
      alldata <- rbind(alldata, output)
    }
  }
 
  
  answer <- readline(prompt="Do you want to remove rows with NA values? (hit 1 if yes) Do you want to include NA values
                     but be warned of their presence? (hit 2 if yes) Do you want to include NA values without a warning (hit 3 if yes)")
  as.numeric(answer)
  if( answer == 1){
    #remove all rows with NAs
    alldata <- na.omit(alldata)
  }else if(answer == 2 && sum(is.na(alldata)) > 0){
    #remove all rows with NAs with a warning
    print("Warning: this compiled data contains NA values")
  }else{
    #keep all NAs without a warning
    return(alldata)
  }

  return(alldata)
}

#dataX<- csvcompilerX("/Users/abigaylebatkoff/Downloads/MBA Year 1/Biocomputing/RProject2021/countryX")

#forrealalldata <- rbind(dataX,dataY)

## summary plots 

summaryfunction <- function(file=forrealalldata){
  numofscreens <- nrow(forrealalldata)
  numinfected <- nrow(forrealalldata[which(rowSums(forrealalldata[,3:12]) >=1),])
  percentinfected <- numinfected / numofscreens
  male <- nrow(forrealalldata[which(forrealalldata$gender == "male"),])
  female <- nrow(forrealalldata[which(forrealalldata$gender == "female"),])
  plot <- ggplot(data=forrealalldata,aes(x=age))+
    geom_histogram(binwidth = 2, fill = "blue")+
    scale_x_continuous(limits=c(0,100))+
    xlab("Age")+
    ylab("Number of Patients")+
    ggtitle("Age Distribution of Patients")+
  theme_classic()
  
  print("Here is the total number of screens:")
  print(numofscreens)
  print("Here is the percent of patients that were infected:")
  print(percentinfected*100)
  print("Here is the number of male and female patients, respectively:")
  print(male)
  print(female)
  print("Lastly, the plot shows the age distribution")
  return(plot)
}
  


