# Ashton Bieri | github @ashtonbieri

source("./supportingFunctions.R")

# convert txt to csv for countryY files
csvConvert("./countryY")

catCsv(directory="./countryX",handleNA="keepNA")

catCsv(directory="./countryY",handleNA="keepNA")

x<-read.csv("./countryX/allData.csv")
y<-read.csv("./countryY/allData.csv")
combined <- write.table(x,file="bothCountries.csv",append=FALSE,row.names=FALSE,col.names=TRUE,sep=",")
combined <- write.table(y,file="bothCountries.csv",append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")

dataSummary("bothCountries.csv")
#basic summary of data

xInfected<-x[rowSums(x[3:12])!=0,]
yInfected<-y[rowSums(y[3:12])!=0,]
hist(xInfected$dayOfYear,xlim=range(120,180),ylim=range(0,2000))
hist(yInfected$dayOfYear,xlim=range(120,180),ylim=range(0,2000))
# in the histograms, we see country X has infected sooner, and at a much higher frequency

hist(x$dayOfYear)
hist(y$dayOfYear)
# we see that from day 120 on, both countries are testing adequately to determine outbreaks
# and at roughly equal rates
print("Standardized frequencies of markers for country X:")
print(round(colMeans(x[3:12])*2.95858,2))
print("Standardized frequencies of markers for country Y:")
print(round(colMeans(y[3:12])*10.35841,2))
# we see that country X infected are mostly markers 01-05; country Y infected are mostly 06-10
# although there is overlap with 04 and 05
# it is likely that a vaccine targeted specfically for Country Y would not be effective for country X