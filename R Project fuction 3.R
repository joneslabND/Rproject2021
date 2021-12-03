#Group Members: Patrick Brown, Samir El Idrissi, Zack Tabellione
install.packages("ggplot2")
library(ggplot2)
summarize.R<-function(findata){
  
  numscreens<-findata

#Counts of screens conducted
  screens<-nrow(numscreens)
  cat("Total number of screens: ", screens, "\n")

#Percent of patients screened that were infected
  totinfected<-nrow(numscreens[numscreens$marker01==1|numscreens$marker02==1|numscreens$marker03==1|
  numscreens$marker04==1|numscreens$marker05==1|numscreens$marker06==1|numscreens$marker07==1| 
  numscreens$marker08==1|numscreens$marker09==1|numscreens$marker10==1, ])

#Calculate percent of patients infected
  percinfected=totinfected/screens*100
  cat("Percent of patients infected=", percinfected,"%\n")

#Male vs Female screen
  male=nrow(numscreens[numscreens$gender=="male",])
  female=nrow(numscreens[numscreens$gender=="female",])
  cat("Number of male patients screened=", male, "\n")
  cat("Number of female patients screened=", female, "\n")

library(ggplot2)
#Age Distribution of Patients
  numscreens$group<-cut(numscreens$age, breaks=c(0,10,20,30,40,50,60,70,80,400),
  labels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80+"), right=T)
  ggplot(numscreens, aes(x=group, fill=gender))+
    geom_bar()+
    xlab("Age of Patients")+
    ylab("Number of Screens")
  
}
