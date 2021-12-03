#second script (analysis.R) will use the source() function to load the functions defined in supportingFunctions.
#R, compile all data into a single comma-separated value (.csv) file, process the data included
#in the entire data set in order to answer the two questions above and provide graphical evidence for your
#answers
setwd("/Users/eselland/Documents/ND/IntroBiocomputing/R_project/Rproject2021/")

source("supportingFunctions.R") 


#In order to analyze this data we need to add a column to tell us whether or not a patient has the disease
alldata$infect<-alldata$marker01+alldata$marker02+alldata$marker03+alldata$marker04+
  alldata$marker05+alldata$marker06+alldata$marker07+alldata$marker08+alldata$marker09+
  alldata$marker10
#the new column "infect" tells us how many markers each patient has for the disease
#if 0, then that patient does not have the disease
alldata$case[alldata$infect>0]<-1
#now there is a column named cases where if the patient has the disease there is a 1, if they dont have the disease then it is NA

#now, we want to look at the number of cases each country has each day
library(ggplot2)

#this shows us over time the number of cases that both country x and country y have
(CasesPerDay<-ggplot(data = alldata, aes(x=dayofYear,y=case,color=as.factor(country)))+
    geom_col()+
    theme_classic())
#we analyzed this graph and interpret it to mean that the disease outbreak started in Country X
##and then spread to country Y (around day 140)


##to determine if a vaccine will work for both countries, we need to see which disease markers are most prevalent 
##in each respective country
#sum of each marker column for each country
Xm1<-sum(alldata[alldata$country=="X",3])
Xm2<-sum(alldata[alldata$country=="X",4])
Xm3<-sum(alldata[alldata$country=="X",5])
Xm4<-sum(alldata[alldata$country=="X",6])
Xm5<-sum(alldata[alldata$country=="X",7])
Xm6<-sum(alldata[alldata$country=="X",8])
Xm7<-sum(alldata[alldata$country=="X",9])
Xm8<-sum(alldata[alldata$country=="X",10])
Xm9<-sum(alldata[alldata$country=="X",11])
Xm10<-sum(alldata[alldata$country=="X",12])

Ym1<-sum(alldata[alldata$country=="Y",3])
Ym2<-sum(alldata[alldata$country=="Y",4])
Ym3<-sum(alldata[alldata$country=="Y",5])
Ym4<-sum(alldata[alldata$country=="Y",6])
Ym5<-sum(alldata[alldata$country=="Y",7])
Ym6<-sum(alldata[alldata$country=="Y",8])
Ym7<-sum(alldata[alldata$country=="Y",9])
Ym8<-sum(alldata[alldata$country=="Y",10])
Ym9<-sum(alldata[alldata$country=="Y",11])
Ym10<-sum(alldata[alldata$country=="Y",12])

markers<-data.frame(marker=1:10,X=c(Xm1,Xm2,Xm3,Xm4,Xm5,Xm6,Xm7,Xm8,Xm9,Xm10),Y=c(Ym1,Ym2,Ym3,Ym4,Ym5,Ym6,Ym7,Ym8,Ym9,Ym10))
#this table shows us the number of patients in each country that tested positive at each microsatellite
#from this graph, it looks to be that the disease has mutated significantly between countries, where
#country X has cases that mostly are diseased by a version of the bacteria with the first 5 markers, 
#whereas country Y has cases that mostly are diseased by a version of the bacteria with the last 5 markers
#so, if a vaccine is made in country Y that only targets microsatellites 6-10, it will be effective
#throughout their country (Y), but less effective throughout country X because of altered binding sites


