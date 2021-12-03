#### Analysis File ####
#source all the functions from supportingFunctions.
source(file="supportingFunctions.R") 

#convert_to_csv Converts all the txt files in the Country Y directory to csv files, use the country Y directory path for directory 
#compile X compiles all csv files from country X into one files, use the country X directory path for directory 
#compile Y adds all csv files from country Y into the file with country X files, use the country Y directory path for directory 
#num_screens gives you the number of screens ran, use the Rproject2021 directory path as directory
#percent_infected gives you the percent of patients infected, use the Rproject2021 directory path as directory
#male_versus_female contrasts how many males and females were screened, xRproject2021 directory path as directory
#ages shows the distribution of ages of the patients screened, Rproject2021 directory path as directory

### 1.In which country (X or Y) did the disease outbreak likely begin?
############ whichever has more infections on the first day 
infectedX <- 0

for(row in 1:length(alldata)){
  if((alldata$marker01[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX <- infectedX  + 1
  }else if((alldata$marker02[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX <- infectedX  + 1
  }else if((alldata$marker03[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX  <- infectedX  + 1
  }else if((alldata$marker04[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX  <- infectedX  + 1
  }else if((alldata$marker05[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX  <- infectedX  + 1
  }else if((alldata$marker06[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX  <- infectedX  + 1
  }else if((alldata$marker07[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX <- infectedX + 1
  }else if((alldata$marker08[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX <- infectedX + 1
  }else if((alldata$marker09[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX <- infectedX + 1
  }else if((alldata$marker10[row]=="1") && (alldata$country=="X") && (alldata$dayofYear=="120")){
    infectedX <- infectedX + 1
  } 
}

infectedY <- 0

for(row in 1:length(alldata)){
  if((alldata$marker01[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY <- infectedY  + 1
  }else if((alldata$marker02[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY <- infectedY  + 1
  }else if((alldata$marker03[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY  <- infectedY  + 1
  }else if((alldata$marker04[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY  <- infectedY  + 1
  }else if((alldata$marker05[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY  <- infectedY  + 1
  }else if((alldata$marker06[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY  <- infectedY  + 1
  }else if((alldata$marker07[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY <- infectedY + 1
  }else if((alldata$marker08[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY <- infectedY + 1
  }else if((alldata$marker09[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY <- infectedY + 1
  }else if((alldata$marker10[row]=="1") && (alldata$country=="Y") && (alldata$dayofYear=="120")){
    infectedY <- infectedY + 1
  } 
}

#This the number of infected people on the first day in country X
infectedX
#This the number of infected people on the first day in country Y
infectedY
#Because more people were infected on the first day in country X, the outbreak most likely originated there

### 2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?

dataX175 <- subset(alldata, dayofYear=="175" & country=="X")
dataY175 <- subset(alldata, dayofYear=="175" & country=="Y")
dfX <- data.frame (marker = 1:10, markerSum = c(sum(dataX175$marker01),sum(dataX175$marker02),sum(dataX175$marker03),sum(dataX175$marker04),sum(dataX175$marker05),sum(dataX175$marker06),sum(dataX175$marker07),sum(dataX175$marker08),sum(dataX175$marker09),sum(dataX175$marker10)))
dfY <- data.frame (marker = 1:10, markerSum = c(sum(dataY175$marker01),sum(dataY175$marker02),sum(dataY175$marker03),sum(dataY175$marker04),sum(dataY175$marker05),sum(dataY175$marker06),sum(dataY175$marker07),sum(dataY175$marker08),sum(dataY175$marker09),sum(dataY175$marker10)))


library(ggplot2)

ggplot(data = dfY, aes(x=marker, y=markerSum)) +
  geom_bar(stat="identity")

ggplot(data = dfX, aes(x=marker, y=markerSum)) +
  geom_bar(stat="identity")

#Based on the two bar graphs on day 175, the last day of sampling, infected people in country Y displayed markers 6-10 at an extemely high rate and markers 1-5 at a very low rate. Country X was the opposite. If Country Y developed a vaccine, it is very unlikely to work for Country X since infected people in each of the two countries display different markers for the disease. 
