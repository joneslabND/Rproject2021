#####Analysis.R
setwd("/Users/edeeg/Documents/practice/tutorial/Rproject2021/")
rm(list=ls())
#make the source of the data our supporting functions script
source("supportingFunctions.R")

#jack's function (part 1)
part1(dir="/Users/edeeg/Documents/practice/tutorial/Rproject2021/countryY")

#eva's function (part 2)
for(i in 1:2){
  if(i==1){
    alldata <- function2(dir="C:/Users/edeeg/Documents/practice/tutorial/Rproject2021/countryX", country="X")
  }else if(i==2){
    alldata1 <- function2(dir="C:/Users/edeeg/Documents/practice/tutorial/Rproject2021/countryY", country="Y")
  }
}
rbind(alldata, alldata1)





#anna's function (part 3 - using Jones' compiled data)
part3(file='allData.csv')



###Analysis questions:
##Question 1 - In which country (X or Y) did the disease likely originate?
#Based on the graphs in part 3 that show the day by day progression (plot1DaybyDayProgression and plot2DaybyDayProgression) we believe that 
#disease likely originated in country X. This is due to the fact that early in the testing time frame, country X had nearly all of the infected 
#patients while country Y had nearly no infected patients until about halfway through the testing time periods. 

##Question 2 - If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#We don't think that the vaccine developed by County Y would work for Country X. This is due to the values that can be found in the "markers" 
#dataframe in which the percent of infected patients with each marker for each country is given as well as the difference between these percentages.
#Based on these values it can be seen that the percentage of infected patients based on each marker is very different between the two countries 
#which causes us to believe that each country has strains of the virus that presents itself differently. Because of this, it is likely the vaccine
#created in Country Y would only work for the strains with the markers that are most prevalent in that country, which are different from Country X. 
