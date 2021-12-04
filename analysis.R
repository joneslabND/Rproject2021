##Question 1  
   #Answer: The disease outbreak likely begin with the country X.
   #Reason: By running the function of patient_day_figure(), we could get the daily patient number and percent in X and Y country, 
#the blue line shows the result of X country, and the red line shows the result of Y country. We could see from the picture that 
#from the first monitoring day, the patients have already exist in country X while the number in Y country is zero, and the disease 
#took place in Y country is almost 20 days after the monitoring began. And the patient number and percentage in X country are higher
#than the Y country during all the monitoring period. So the disease outbreak likely begin with the country X. 

##Question 2
   #Answer: If Country Y develops a vaccine for the disease, it may not work for citizens of Country X.
   #Reason: By running the function of marker_figure(), we could get each marker number in all patients, same color present same 
#marker in X and Y country. We could see from the picture that X and Y country has different distribution of markers, the patients 
#in Y country mostly present with last five markers, marker 4 and 5 are less present, and marker 1 to 3 are more less present. While
#the patients in X country mostly present with first five markers, marker 6 to 10 are very less present, almost near zero.
#Since different markers would indicate different immunological active protein, and cause potentially different responses in 
#a patient??s immune system, the vaccine in Y country may mostly target to patient with marker 6 to 10, less target to patient with 
#other markers. So the vaccine in Y country may not be working in X country.


##running source() function to load the functions defined in supportingFunctions.R
source("supportingFunctions.R")


##the function txt2csv() could convert all .txt monitoring files in Y country into comma-separated .csv files and save with the name dataY
txt2csv('countryY')


##the function all2one()function could upload and combine all data of Y and X country into a new table and add two columns of 
##country_name and monitoring date of each people and its corresponding content into new generate countryX.csv and countryY.csv
##this function also convenient for users by changing the parameter of warningNA and removeNA to choose give warning and remove 
##NA files or not if there is any NA present in the table
all2one('countryY','Y')
all2one('countryX','X')
##the function combine2csv() could combine all X and Y country data into a new table, and save as alldata.csv
combine2csv('countryX.csv','countryY.csv','alldata.csv')

 
##the function patient_day_figure() could get daily patient number and percent in X country, Y country, both two countries and present the 
##result in three line graphs, these line graph use blue line shows the result of X country, and the red line shows the result of Y country
#we could see X country first outbreak the disease and patient number during all monitoring period are still higher than Y country
patient_day_figure()


##the function gender_day_figure() could 1)get daily female patient number and percent in X country, Y country and both two countries, 
##2) present these female patient percent in line and bar chart
#we could see female percent in X,Y country and both two countries are always around 50%, maybe mean there maybe no much different 
#regarding the gender
gender_day_figure()


##the function age_figure() could get 1)the number of of all age patients and present every age distribution to the disease in country X, 
##country Y, both the two countries in line chart; 2）the number of every ten years age patients and present the age period distribution 
##to the disease in country X, country Y, the two countries in line and chart graph
#wen could see that 0-9 years old take place the 1st position in all years parts, 10-19 years old the 2rd, 20-29 years old the third, people
#age below 30 years old take up almost 90% of all patients,and the age is older the number of patients is less.So the age may contribute 
#to the disease, and younger person are more easy to get the disease.
age_figure()


##the function marker_figure() could get the number of patients labeled with every marker of the ten disease related markers in X and Y country
#we could see X and Y country has different distribution of markers, the patients in Y country mostly present with last five markers, marker 
#first five markers are less present. While the patients in X country mostly present with first five markers, marker 6 to 10 are very less
#present, almost near zero.
marker_figure()
