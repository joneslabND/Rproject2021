###Analysis script
#load in the functions from supportingFunctions.R

source("supportingFunctions.R")

#choose any directory with .txt files not already in .csv format and create .csv
#please put the subdirectory that needs to converted as the argument for function
#If more than one subdirectory has to be converted, please put them in separate "" separated by a comma
#countryY is used in our script due to the name of the subdirectory, feel free to replace
cmdlfiles("countryY")
#Compile all csv files from all of the country subdirectories
#Write in the country directories that need analyzed. Write directories in quotations, separated by comma(s).
#This is shown using countryX and countryY, please feel free to replace using your sub directory names
z<-mergecsv("countryX", "countryY")
#install graphing utility and open it up using the library function
install.packages("ggplot2")
library(ggplot2)
#use this funtion to summarize the data from compiled country list. The input has been done for you
summarize.R(z)
#Create list of just infected screening from the compiled data
infectedlist<-z[z$marker01==1|z$marker02==1|z$marker03==1|z$marker04==1| 
                      z$marker05==1|z$marker06==1|z$marker07==1|z$marker08==1|z$marker09==1|
                      z$marker10==1,]
#create a graph showing the positive results as a function of days of the year
ans1 <-ggplot(infectedlist, aes(x=dayofyear, y=country))+ 
  geom_jitter(stat="identity", show.legend=TRUE)+
  ggtitle("Total infected")+
  xlab("day of year")+ 
  ylab("Country")
ans1
#We can see on this graph that countryX showed positive cases of the sickness around day 120. However, the first positive screenings of the sickness in country Y were not until day 140, thus country X must be where the disease originated.


#create new data table using the infected list that seperates the two countries screenings
Xinflist<-infectedlist[infectedlist$country == "countryX",]
Yinflist<-infectedlist[infectedlist$country == "countryY",]
#Create new data frame where the columns are:
h=c("Marker", "CountryX", "CountryY")
#and the rows are:
p=c("Marker1", "Marker2", "Marker3","Marker4","Marker5", "Marker6","Marker7","Marker8","Marker9","Marker10")
markers<-data.frame(matrix(NA, ncol = 3, nrow = 10))
markers[,1]=p
colnames(markers)=h

#assign each country the sum of that bio marker infection, creating a comprehensive table that displays the overall number of screenings with mutations to that specific biomarker, while differentiating the countries of the screening
markers[1,2]=sum(Xinflist$marker01)
markers[2,2]=sum(Xinflist$marker02)
markers[3,2]=sum(Xinflist$marker03)
markers[4,2]=sum(Xinflist$marker04)
markers[5,2]=sum(Xinflist$marker05)
markers[6,2]=sum(Xinflist$marker06)
markers[7,2]=sum(Xinflist$marker07)
markers[8,2]=sum(Xinflist$marker08)
markers[9,2]=sum(Xinflist$marker09)
markers[10,2]=sum(Xinflist$marker10)
markers[1,3]=sum(Yinflist$marker01)
markers[2,3]=sum(Yinflist$marker02)
markers[3,3]=sum(Yinflist$marker03)
markers[4,3]=sum(Yinflist$marker04)
markers[5,3]=sum(Yinflist$marker05)
markers[6,3]=sum(Yinflist$marker06)
markers[7,3]=sum(Yinflist$marker07)
markers[8,3]=sum(Yinflist$marker08)
markers[9,3]=sum(Yinflist$marker09)
markers[10,3]=sum(Yinflist$marker10)
markers
library(ggplot2)
#graph this data
Vaccine<-ggplot(markers, aes(x=CountryX, y=CountryY, fill=Marker))+
  geom_point()
  Vaccine         
  #Due to the fact that the points are located near the axis of the graph, there was not 
  # much overlap in the number of mutations for any marker between country x and y, if there had been points located in the center of the graph, it would have displayed similarity in the microsatallites the disease targeted.
  #Because of this, the same vaccine would most likely not work for both countries because the disease has been targeting different microsatallites.



 
 
 
 
 
 