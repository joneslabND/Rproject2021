#####supportingFunctions.R
###This Function works to convert .txt files to .csv files 
part1<-function(dir){
  setwd(dir)
  FILE<-list.files(pattern=".txt")
  #the function starts by addressing the argument as a directory which can be changed by the user.
  #It continues by creating a variable called FILE which takes all the .txt files in a given directory
  for(i in 1:length(FILE)){
    data<-read.table(FILE[i], header=TRUE, sep=" ", stringsAsFactors=F)
    fname=paste("screen_", i, ".csv", sep="")
    write.table(data, file=fname, row.names=FALSE, col.names=TRUE, sep=",")
  }
}
#the For Loop reads-in all the .txt files and rewrites them as .csv files

#to run the function "Part1", the working directory is specified ^. 


#END
#This function works to combine data files from each country into one data set
function2 <- function(dir, country){
  #setting working directory using argument dir
  setwd(dir)
  #creating object of list of files
  FILES <- list.files(pattern=".csv")
  #looping through files 
  for(i in 1:length(FILES)){
    if(i==1){
      #adding first files to dataframe allfile
      allfile <- read.csv(FILES[i], sep=",", header=TRUE)
      #adding column with country
      allfile <- allfile %>% mutate(country=country)
      #adding column with day
      allfile <- allfile %>% mutate(Day=substr(FILES[i], 8,10))
    }else{
      #adding the following files to dataframe
      #reading in file
      file <- read.csv(FILES[i], sep=",", header=TRUE)
      #adding column with country
      file <- file %>% mutate(country=country)
      #adding column with day
      file <- file %>% mutate(Day=substr(FILES[i], 8,10))
      #adding file to dataframe
      allfile <- rbind(allfile, file)
    }
  }
  #asking user what to do with NA's
  answer <- readline(prompt="What would you like to do with row's that have NA's?
           1: Remove rows with NA's
           2: Include NA's but with a warning
           3: Include NA's without warning")
  if(answer==1){
    #remove rows with NA's
    allfile <- na.omit(allfile)
  }else if(answer==2){
    #warning user if there are NA's in files
    if(any(is.na(allfile)==TRUE)){
      print("There are NA's in this file")
    }else{
      print("There are no NA's in this file") 
      break
    }
  }else if(answer==3){
    #not warning user if they chose not to receive notice
    break
  }
  
  return(allfile)
}

###Part 3###

part3 <- function(file){
  #load in summarized data and save as object
  data <- read.csv(file)
  
  #create new column showing whether the patient is actually infected 
  #(1 in any of the markers) and save this as new object
  newData <- data %>%
    mutate(
      Status = if_else(marker01+marker02+marker03+marker04+marker05+marker06+
                         marker07+marker08+marker09+marker10 > 0, "infected", "not infected")
    )
  
  #calculate sum of infected people for each country 
  xInfected <- nrow(newData[(newData$Status=="infected")&(newData$country=="X"),])
  yInfected <- nrow(newData[(newData$Status=="infected")&(newData$country=="Y"),])
  
  
  #plot progression with infected for each country (day of year vs. infected)
  ##this shows that the infected people originated in country X
  plot1DaybyDayProgression <- ggplot(newData, aes(x=dayofYear, y=country))+
    geom_jitter()+ 
    facet_wrap(vars(newData$Status))
  
  #another way to do this 
  plot2DaybyDayProgression <- ggplot(newData, aes(x=dayofYear, y=country, color=Status))+
    geom_jitter()
  
  #calculate percent of screens/tests that were actually infected 
  #infected/total *100
  percent_infected <- ((nrow(newData[newData$Status=="infected",]))/(39888))*100
  
  #plot infection for each gender
  plotGenderInfection <- ggplot(newData, aes(x = gender, fill=country)) +
    geom_histogram(stat='count')+
    facet_wrap(vars(newData$Status))
  
  #number of males and females 
  numberFemale <- nrow(newData[newData$gender=="female",])
  numberMale <- nrow(newData[newData$gender=="male",])
  
  
  #plot age distribution
  plotAgeDist <- ggplot(newData, aes(x=age, color=gender))+
    geom_histogram()+
    facet_wrap(vars(newData$Status))
  
  #some sort of plot or calculation to determine the vaccine question
  #based on the different markers? or is it something with the mutation or something
  #if markers could do a count of all of the individuals in each country with each of the markers
  
  Xmarker01 <- nrow(newData[(newData$country=="X")&(newData$marker01=="1"),])
  Ymarker01 <- nrow(newData[(newData$country=="Y")&(newData$marker01=="1"),])
  
  Xmarker1 <- (nrow(newData[(newData$country=="X")&(newData$marker01=="1"),]))/(xInfected)
  Ymarker1 <- (nrow(newData[(newData$country=="Y")&(newData$marker01=="1"),]))/(yInfected)
  
  Xmarker2 <- (nrow(newData[(newData$country=="X")&(newData$marker02=="1"),]))/(xInfected)
  Ymarker2 <- (nrow(newData[(newData$country=="Y")&(newData$marker02=="1"),]))/(yInfected)
  
  Xmarker3 <- (nrow(newData[(newData$country=="X")&(newData$marker03=="1"),]))/(xInfected)
  Ymarker3 <- (nrow(newData[(newData$country=="Y")&(newData$marker03=="1"),]))/(yInfected)
  
  Xmarker4 <- (nrow(newData[(newData$country=="X")&(newData$marker04=="1"),]))/(xInfected)
  Ymarker4 <- (nrow(newData[(newData$country=="Y")&(newData$marker04=="1"),]))/(yInfected)
  
  Xmarker5 <- (nrow(newData[(newData$country=="X")&(newData$marker05=="1"),]))/(xInfected)
  Ymarker5 <- (nrow(newData[(newData$country=="Y")&(newData$marker05=="1"),]))/(yInfected)
  
  Xmarker6 <- (nrow(newData[(newData$country=="X")&(newData$marker06=="1"),]))/(xInfected)
  Ymarker6 <- (nrow(newData[(newData$country=="Y")&(newData$marker06=="1"),]))/(yInfected)
  
  Xmarker7 <- (nrow(newData[(newData$country=="X")&(newData$marker07=="1"),]))/(xInfected)
  Ymarker7 <- (nrow(newData[(newData$country=="Y")&(newData$marker07=="1"),]))/(yInfected)
  
  Xmarker8 <- (nrow(newData[(newData$country=="X")&(newData$marker08=="1"),]))/(xInfected)
  Ymarker8 <- (nrow(newData[(newData$country=="Y")&(newData$marker08=="1"),]))/(yInfected)
  
  Xmarker9 <- (nrow(newData[(newData$country=="X")&(newData$marker09=="1"),]))/(xInfected)
  Ymarker9 <- (nrow(newData[(newData$country=="Y")&(newData$marker09=="1"),]))/(yInfected)
  
  Xmarker10 <- (nrow(newData[(newData$country=="X")&(newData$marker10=="1"),]))/(xInfected)
  Ymarker10 <- (nrow(newData[(newData$country=="Y")&(newData$marker10=="1"),]))/(yInfected)
  
  
  #matrix for all markers
  Xmarkers <- c(Xmarker1, Xmarker2, Xmarker3, Xmarker4, Xmarker5, Xmarker6, Xmarker7, Xmarker8, Xmarker9, Xmarker10)
  Ymarkers <- c(Ymarker1, Ymarker2, Ymarker3, Ymarker4, Ymarker5, Ymarker6, Ymarker7, Ymarker8, Ymarker9, Ymarker10)
  
  markers <- data.frame(CountryX=Xmarkers, CountryY=Ymarkers, difference = (Xmarkers-Ymarkers))
  
  
  #make object with everything I want to return
  returnValues <- print(xInfected) + print(yInfected) + print(plot1DaybyDayProgression) +
    print(plot2DaybyDayProgression) + print(percent_infected) + print(numberFemale) +
    print(numberMale) + print(plotGenderInfection) + print(plotAgeDist) + print(markers)
  
  return(returnValues)
}
