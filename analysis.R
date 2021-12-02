##R Project
#Introduction to Biocomputing 

#Group Members: 
#Andrei Badilla
#Mayrangeli Torres
#Marlee Shaffer

##This script analyzes the data for the disease for Country X and Country Y
##This script will use source to use the custom functions, compile data into one .csv
##and process the data. 

library(ggplot2)
library(cowplot)

##Set the working directory. This directory must contain the folders countryX and countryY and all of their associated files. 
dir<-setwd("~/Desktop/Rproject2021")

##Use functions from supportingFunctions.R
CSV.converter(dir)
CSV.merger(dir)
summary("combined.csv")
heterogeneity("combined.csv")

##Overall Summary of Outbreak
#The function summary outputs multiple plots. Plot A looks at the number of screens that were run 
#in Country X and Country Y. The total number of screens run was 39,888 which is broken down into 
#20,062 done in Country X and 19,826 done in Country Y. 

#Output from Screendf, used to create Plot A.
#Location Screen
#1     Total  39888
#2 Country X  20062
#3 Country Y  19826

#Plot B shows the number of infected and uninfeceted patients in total, Country X, and Country Y. In total, 
#there were 22,557 infected patients and 17,331 uninfected patients. Country X had 15,687 infected and 4,375
#uninfected patients and Country Y has 6,870 infected and 12,956 uninfected patients. 

#Output from SUMdf, used to create Plot B
#      Group   Subgroup Patients
#1     Total   Infected    22557
#2     Total Uninfected    17331
#3 Country X   Infected    15687
#4 Country X Uninfected     4375
#5 Country Y   Infected     6870
#6 Country Y Uninfected    12956

#Plot C uses the data from Plot B to determine the percent of the patients that were infected. Country X
#had 78.19% of its patients test positive and Country Y had 34.65% of its patients test positive. The total 
#percent of infected individuals was 56.55% between the two countries. 

#Output from Percentdf, used to create Plot C
#   Labelper  Percent
#1     Total 56.55084
#2 Country X 78.19260
#3 Country Y 34.65147

#Plot D looks at the comparison between female and male patients who were screened for the virus. Overall, 
#the number of females and males screened were similar. Country X had 9,977 female patients and 1,0085 male
#patients and Country Y had 9,919 female patients and 9,907 male patients. The total number of female patients
#was 19,896 and the total number of male patients was 19,992.

#Output from fvsmdf, used to create Plot D
#      Group Subgroup Patient
#1     Total   Female   19896
#2     Total     Male   19992
#3 Country X   Female    9977
#4 Country X     Male   10085
#5 Country Y   Female    9919
#6 Country Y     Male    9907

#Plots E, F, and G look at the age distribution of patients. The distribution shows that the patients were typically 
#under the age of 24, with a large portion of the patients falling in the 0-4 and 5-14 age groups. This was seen in both 
#Country X and Country Y. 

#Output from agedata, used to create Plot E, F, and G 
#     Group Patients Patients_X Patients_Y
#1     0-4    10823       5424       5399
#2    5-14    18651       9367       9284
#3   15-24     5628       2818       2810
#4   25-34     2231       1165       1066
#5   35-44     1040        512        528
#6   45-54      548        290        258
#7   55-64      329        156        173
#8   65-74      206        102        104
#9   75-84      125         63         62
#10  85-94       89         44         45
#11 95-104       57         30         27
#12   105+      161         91         70

#The heterogeneity function looks at the changes in the markers present in infections over the days of the year
#that screens were run. Country X had a prevalence of infections with Markers 1 through 5, with only a handful of
#infections with Markers 6 through 10. Contrarily, Country Y had most of its cases with Markers 6 through 10, with a 
#a spike of Markers 1 through 5 on Day 139, which is when the first cases were detected in Country Y. Towards the end
#of the screening period, there was a slight increase in the number of detects for Marker 4

##In which country (X or Y) did the disease outbreak likely begin?

##Based on the output of the heterogeneity function, it is likely that the disease outbreak started in Country X. 
##From A in the figure, it is seen that infections were detected in Country X starting on Day 120, where as 
##B shows that infections were not detected in Country Y until close to Day 140. The lag that occurs shows 
##that the infection started in Country X and was then transmitted to Country Y. 

##If Country Y develops a vaccine for the disease, is it likely to work for the 
##citizens of Country X?

#If Country Y developed a vaccine for the disease, it would not likely work for the 
#citizens of Country X because the different markers that are regularly present in infections. Again, 
#the heterogeneity function shows the occurrence of the markers over time. Country X had a wide occurrence of 
#infections with Markers 1 through 5, where Country Y has infections with Markers 6 through 10. The difference 
#between the Markers could have an impact on the vaccine efficacy for Country X, however it would help to prevent
#the spread of the virus with Markers 6 through 10. A vaccine would have to be made that can either be effective 
#against all of the variants, which would be difficult depending on where in the sequence the mutation is, or Country 
#X would have to develop their own vaccine. 




