# Directory paths will change based on user
# Carson Hasbrouck

# Use source() function to load functions from supportingfunctions.R, then run those functions to obtain a compiled
# csv files with all screens from a given country

source("supportingfunctions.R")
fileconvertcsv("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021/countryY")
#Country X starts with all csv files, so we do not need to convert them with the fileconvertcsv function

allonecsv("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021/countryY", "countryY")
allonecsv("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021/countryX", "countryX")

sumdir("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021/countryY", "countryY")
sumdir("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021/countryX", "countryX")

# Compile all data into a single csv file

setwd("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021")

csvfiles<-dir(pattern = '*all.csv$', recursive = TRUE)

compiledcsv<-data.frame()

for(i in 1:length(csvfiles)){
  if(i==1)
    compiledcsv<-read.csv(csvfiles[i])
  else
    compiledcsv<-rbind(compiledcsv, read.csv(csvfiles[i]))
} #allows for many country's data (more than 2) to be compiled into one csv when the functions in supportingfunctions.R
# have been run before it

write.csv(compiledcsv, 'compileddata.csv')

### Question 1: In which country (X or Y) did the disease outbreak likely begin?

##Read in data
alldata <- read.csv("compileddata.csv", header = TRUE)
head(alldata)

onlyinfected<-alldata[alldata$marker01 == 1 | alldata$marker02 == 1 | alldata$marker03 == 1 | alldata$marker04 == 1 | alldata$marker05 == 1 | alldata$marker06 == 1 | alldata$marker07 == 1 | alldata$marker08 == 1 | alldata$marker09 == 1 | alldata$marker10 == 1, ]

for (i in 3:12){ # This Graph answers Question 1
  assign(paste0("plot_marker", i-2),
         print(ggplot(onlyinfected, aes(x = day, y = onlyinfected[,i], color = country)) +
                 geom_point() +
                 ylab(colnames(onlyinfected)[i])))
}

library(cowplot)

Question1Figure<-plot_grid(plot_marker1,plot_marker2,plot_marker3,plot_marker4,plot_marker5,plot_marker6,plot_marker7,plot_marker8,plot_marker9,plot_marker10, rel_widths = c(1, 0.85), ncol = 2, nrow = 5)

setwd("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021")

ggsave(filename = "Question1Figure.pdf", Question1Figure, width = 8, height = 5, dpi = 300)

#The figure produced is 10 different smaller graphs. Each graph produced shows when the first presence of each marker was registered in each country.
#In Country X, the first presence of each marker was measured to be around the 120 day of the year mark.
#In Country Y, the first presence of each marker was measurd to be around the 140 day of the year mark.
#Due to this measurement and the fact that each marker's numbers are consistent (Country X showed first signs)
#the conclusion that can be made from this data is that Country X was the country in which the disease outbreak likely began.

### Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?

# Graphical analysis of infections between country X and country Y based on marker identified

onlyinfected<-compiledcsv[compiledcsv$marker01 == 1 | compiledcsv$marker02 == 1 | compiledcsv$marker03 == 1 | compiledcsv$marker04 == 1 | compiledcsv$marker05 == 1 | compiledcsv$marker06 == 1 | compiledcsv$marker07 == 1 | compiledcsv$marker08 == 1 | compiledcsv$marker09 == 1 | compiledcsv$marker10 == 1, ]

marker1<-compiledcsv[compiledcsv$marker01 == 1, ] #number of infected people with marker 1 (repeats for markers 2-10 in following lines)
marker1countries<-marker1[, "country"] # vector of countries for each individual in marker1 dataset (repeats for markers 2-10 in following lines)
marker1markers<-c(rep("marker01", nrow(marker1))) # vector of marker name based on number of individuals (repeats for markers 2-10 in following lines)
longmarker1<-data.frame(marker1countries, marker1markers) # dataframe of previous two vectors (repeats for markers 2-10 in following lines)
colnames(longmarker1)<-c("country","marker") # changes column name for above dataframe (repeats for markers 2-10 in following lines)

marker2<-compiledcsv[compiledcsv$marker02 == 1, ]
marker2countries<-marker2[, "country"]
marker2markers<-c(rep("marker02", nrow(marker2)))
longmarker2<-data.frame(marker2countries, marker2markers)
colnames(longmarker2)<-c("country","marker")

marker3<-compiledcsv[compiledcsv$marker03 == 1, ]
marker3countries<-marker3[, "country"]
marker3markers<-c(rep("marker03", nrow(marker3)))
longmarker3<-data.frame(marker3countries, marker3markers)
colnames(longmarker3)<-c("country","marker")

marker4<-compiledcsv[compiledcsv$marker04 == 1, ]
marker4countries<-marker4[, "country"]
marker4markers<-c(rep("marker04", nrow(marker4)))
longmarker4<-data.frame(marker4countries, marker4markers)
colnames(longmarker4)<-c("country","marker")

marker5<-compiledcsv[compiledcsv$marker05 == 1, ]
marker5countries<-marker5[, "country"]
marker5markers<-c(rep("marker05", nrow(marker5)))
longmarker5<-data.frame(marker5countries, marker5markers)
colnames(longmarker5)<-c("country","marker")

marker6<-compiledcsv[compiledcsv$marker06 == 1, ]
marker6countries<-marker6[, "country"]
marker6markers<-c(rep("marker06", nrow(marker6)))
longmarker6<-data.frame(marker6countries, marker6markers)
colnames(longmarker6)<-c("country","marker")

marker7<-compiledcsv[compiledcsv$marker07 == 1, ]
marker7countries<-marker7[, "country"]
marker7markers<-c(rep("marker07", nrow(marker7)))
longmarker7<-data.frame(marker7countries, marker7markers)
colnames(longmarker7)<-c("country","marker")

marker8<-compiledcsv[compiledcsv$marker08 == 1, ]
marker8countries<-marker8[, "country"]
marker8markers<-c(rep("marker08", nrow(marker8)))
longmarker8<-data.frame(marker8countries, marker8markers)
colnames(longmarker8)<-c("country","marker")

marker9<-compiledcsv[compiledcsv$marker09 == 1, ]
marker9countries<-marker9[, "country"]
marker9markers<-c(rep("marker09", nrow(marker9)))
longmarker9<-data.frame(marker9countries, marker9markers)
colnames(longmarker9)<-c("country","marker")

marker10<-compiledcsv[compiledcsv$marker10 == 1, ]
marker10countries<-marker10[, "country"]
marker10markers<-c(rep("marker10", nrow(marker10)))
longmarker10<-data.frame(marker10countries, marker10markers)
colnames(longmarker10)<-c("country","marker")

markerdataset<-rbind(longmarker1, longmarker2, longmarker3, longmarker4, longmarker5, longmarker6, longmarker7, longmarker8, longmarker9, longmarker10)

Question2Figure<-ggplot(markerdataset, aes(x=marker, y=nrow(markerdataset), fill=country)) + geom_bar(position = "fill", stat = "identity") + xlab("Marker") + ylab("Proportion of Markers") + ggtitle("Proportion of Markers 01-10 Subdivided by Country") + theme_classic()
# This Graph supports the answer to Question 2

setwd("D:/Dropbox/Notre Dame/Junior Year/Intro to Biocomputing/Rproject2021")

ggsave(filename = "Question2Figure.pdf", plot = Question2Figure, width = 8, height = 5, dpi = 300)

### Discussion of question 2
# Based on the results of the above percent stacked bar charts, we can see that the markers that affect individuals
# in countries X and Y differ significantly, as markers 01-05 are predominantly present in country X whereas markers 06-10
# are predominantly present in country Y. We were told that the differences in markers that are present within an
# infected individual indicate differences in the protein in the disease, as well as potentially different responses
# by a patient's immune system. Due to this information we can conclude that because of the stark contrast in marker
# composition between country X and country Y, a vaccine developed for infected individuals in country Y is not likely
# to work for individuals of country X. We can conclude this because the proteins in the disease that a vaccine developed 
# in country Y would target for infected individuals are different from the proteins in the disease that a desired vaccine 
# for infected individuals in country X would target. The data gathered shows us that a vaccine developed in country Y
# would likely be ineffective at preventing the disease for individuals in country X.
