##Analysis

#vaccine question: look at how different the markers are, does it change a lot in country y

#analysis R
#use functions you defined
#answer 2 questions w/ explanation in comments
#figure to support BOTH answers

source("/Users/diannaperez/Desktop/BiocompR/Rproject2021/supportingFunctions.R")

library(ggplot2)

txt2csv("/Users/diannaperez/Desktop/BiocompR/Rproject2021/countryY")

dataX <- compileX("/Users/diannaperez/Desktop/BiocompR/Rproject2021/countryX")
dataY <- compileY(directory = "/Users/diannaperez/Desktop/BiocompR/Rproject2021/countryY") 
                   
forrealalldata <- rbind(dataX,dataY)

summaryfunction(file=forrealalldata)

#In which country (X or Y) did the disease outbreak likely begin?
#see which country had the most cases in the first screening days
#create a cumulative sum vector based on each country's data
#A: There were more infections on the first recorded day (day of year 120) in country X, with 57, than 
# in country Y where there was zero. It is likely that the disease outbreak began in Country X.

Xday1infection <- nrow(dataX[which(rowSums(dataX[dataX$dayofYear =="120",3:12]) >=1),])
Yday1infection <- nrow(dataY[which(rowSums(dataY[dataY$dayofYear =="120",3:12]) >=1),])
cat(c("This is the number of infections on day of year 120 in country X: ", Xday1infection))
cat(c("This is the number of infections on day of year 120 in country Y: ", Yday1infection))

#### Q2
##If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#A. No, it is unlikely that a vaccine developed for citizens of Country Y would work for citizens of Country X.
#The markers prevalent in each country differ, Country X is affected mostly by markers 1-5 whereas country Y
# has markers 6-10. The disease is caused by different strains of a virus likely and thus a vaccine 
# developed for markers 1-5 would differ from a vaccine for markers 6-10.

  Xmarkers <- colSums(dataX[,3:12]) #sum of marker incidence in country X
  Ymarkers <- colSums(dataY[,3:12]) #sum of marker incidence in country Y

Xmark <- data.frame(marker=c(1:10),sum =c(6566,6686,6574,6789,6629,56,79,78,66,63))
Ymark <- data.frame(marker=c(1:10),sum=c(198,207,210,814,795,1884,1914,1428,1471,1445))

library(ggplot2)
ggplot(data=Xmark, aes(x=as.factor(marker), y=sum))+
  geom_bar(stat="identity")+
  xlab("Marker") +
  ylab("Prevalence") +
  ggtitle("Country X")+
  theme_classic()

ggplot(data=Ymark, aes(x=as.factor(marker), y=sum))+
  geom_bar(stat="identity")+
  xlab("Marker") +
  ylab("Prevalence") +
  ggtitle("Country Y")+
  theme_classic()



