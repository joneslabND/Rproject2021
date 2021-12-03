setwd("C:/Users/megan/Desktop/RProject/RProject2021")
source("support.R")
summary(allDa)
#Question 1
#Based on this data, the outbreak likely broke out in country X as there are 60 infections on day 120 (day 1) in country X
#compared to 16 infections on day 120 in country Y. Additionally, over the first five days, there are 55 total infections 
#in country Y compared to 540 infections in country X
#see graph InfectionVsDay.png
#Question 2
#If country Y develops a vaccine, it is unlikely to work in country X. The majority of infected people in country X have the
#markers 1,2,3,4 and 5; whereas the majority of infected people in country Y have the markers 6,7,8,9, and 10. A vaccine in
#country Y will most likely target these markers 6-10 as a majority of the population displays these markers. Therefore, a vaccine 
#that targets markers 6-10 will not be effective for country X, as only a minority of the population displays these markers. A
#vaccine that targets markers 1-5 would be effective in country X. 
#see graph markerGraph.png
