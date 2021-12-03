## Analysis for R project

source("supportingFunctions.R")

# Installing packages
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(dplyr)

# Compile all data into a .csv file
<<<<<<< HEAD
countryXdf=read.csv("~/Desktop/Rproject2021/countryX/csvCombine",header=TRUE)
countryYdf=read.csv("~/Desktop/Rproject2021/countryY/csvCombine",header=TRUE)
allData=rbind(countryXdf,countryYdf)
=======
# dir X is to the csvCombine for country X
# dir Y is to the csvCombine for country Y
# desiredDir is where the combined file for both countries should go

combineCountries = function(dirX,dirY,desiredDir){
  countryXdf=read.csv(dirX)
  countryYdf=read.csv(dirY)
  countryData=rbind(countryXdf,countryYdf)
  setwd(desiredDir)
  write.csv(countryData,file="countryData.csv")
}
>>>>>>> 2a37b036ebba4790be879a255173fe0eb0a5dd81

# Process data to answer questions and provide graphical evidence

## In which country (X or Y) did the outbreak likely begin?
# Cases over time in country X
countryXcases=data.frame(matrix(nrow=,ncol=2))
colnames(countryXcases)=c("dayofYear","dailyCases")

for (d in 120:175){
  dailyCases=sum(rowSums(countryXdf[countryXdf$dayofYear %in% d, 3:12]))
  dailyCases=data.frame(d,dailyCases)
  colnames(dailyCases)=c("dayofYear","dailyCases")
  countryXcases=rbind(countryXcases,dailyCases)
}
countryXcases=na.omit(countryXcases)

# Cases over time in country Y
countryYcases=data.frame(matrix(nrow=,ncol=2))
colnames(countryYcases)=c("dayofYear","dailyCases")

for (d in 120:175){
  dailyCases=sum(rowSums(countryYdf[countryYdf$dayofYear %in% d, 3:12]))
  dailyCases=data.frame(d,dailyCases)
  colnames(dailyCases)=c("dayofYear","dailyCases")
  countryYcases=rbind(countryYcases,dailyCases)
}
countryYcases=na.omit(countryYcases)

# Plot infections over time in each country (compare on a plot)
outbreak=ggplot()+
  geom_line(data=countryXcases, aes(x=dayofYear,y=dailyCases,color="X"))+
  geom_line(data=countryYcases, aes(x=dayofYear,y=dailyCases,color="Y"))+
  labs(color="Country")+
  xlab("Day of Year")+
  ylab("Number of Cases")+
  ggtitle("Disease Outbreak in Countries X and Y")

outbreakFigure=plot_grid(outbreak)
outbreakFigure

# Based on the line graph (titled "outbreakFigure"), it is clear that the disease outbreak likely began in Country X.
# This is because the number of cases was higher initially and increased more rapidly at the beginning in Country X.
# In Country Y, however, there was a lag to the increase and cases were low or nonexistent at the beginning.

# If Country Y develops a vaccine for the disease, is it likely to work for Country X?

# Focus on end of data, looking at the markers prevalent in bacteria now
Xmarkers = countryXdf[countryXdf$dayofYear==175,c(3:12)]
Ymarkers = countryYdf[countryYdf$dayofYear==175,c(3:12)]

# Get sum of individual markers 
sumMarkersX = mapply(sum, Xmarkers)
sumMarkersY = mapply(sum, Ymarkers)

# Create two pie charts:
# Prepare a color palette
myPalette <- brewer.pal(10, "Set3") 

# Create Pie Chart
pie(sumMarkersX, border="white", col=myPalette, main = "Country X Markers") 
pie(sumMarkersY, border = "white", col=myPalette, main = "Country Y Markers")

# As the pie charts show, the markers in bacteria in Country X are different from those present in the bacteria
# in Country Y by day 175. Thus, it is likely that any vaccine that Country Y develops will be ineffective against 
# the different strains of the bacteria present in Country X. 
