##This script has the functions needed to accomplish various data handling or summary tasks
##Set working directory
dir<-setwd("~/Desktop/Rproject2021")

##Function 1 
#Country X and Y have different traditions for the delimiter in their data files. 
#This function converts all files in a directory with a space or tab-delimited data
#into .csv files. The path input must be directly to the files that are being converted.

CSV.converter <-function(dir){
  setwd(dir)
  filelist = list.files(path = dir, pattern = ".txt", recursive = TRUE)
  for (i in 1:length(filelist)){
    input<-filelist[i]
    output<-paste0(gsub("\\.txt$", "", input), ".csv")
    data = read.table(input, sep = "", header = TRUE)   
    write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
    file.remove(input)
  }
}

##Function 2 
#CSV.merger
##This function compiles data from all .csv extension files within a main directory into 
##a single .csv file. Also, it adds the columns country and dayofYear, which entries (rows) are filled out based upon
##the sub directory of origin and the number of the file, respectively. 
##CSV.merger receives a main directory input (absolute or relative path) from the user. 
##The function displays a prompt where the user is able to choose to remove rows 
##with NA’s in any columns, include NAs in the compiled
##data but be warned of their presence, or include NAs in the compiled data without a warning

CSV.merger <-function(dir){
  library(data.table)
  all_paths <- list.files(path = dir, recursive = TRUE,
                          pattern = "*.csv", full.names = TRUE)
  all_dir <-dirname(path= all_paths)
  days <-'dayofYear'
  country<-'country'
  header<-read.csv(all_paths[1],header = TRUE, stringsAsFactors = FALSE) #take a first file from list.files and use only the first row (header) 
  header<-data.frame(header[1,]) 
  ##prompt to ask for an user entry and continue with the analysis process 
  response<-readline(prompt = 
                       "Enter 1 if you want to remove rows with NA’s 
Enter 2 if you want to compile data but be warned of NA'S 
Enter 3 if you want to include NAs in the compiled data without any warning")
  
  response<-as.character(response)
  for (i in seq_along(all_paths)){
    ##if user says don't remove NAs then do this:
    if (response=="1"){
      data<-read.csv(all_paths[i],header = TRUE, stringsAsFactors = FALSE)
      no.nas <-na.omit(data)
      no.nas[,days]<- gsub(".+([0-9]{3}).*", "\\1", all_paths[i])
      no.nas[,country]<-gsub(".+([A-Z]{1}).*", "\\1", all_dir[i])
      header<-rbindlist(list(header,no.nas),fill = TRUE)  #adding content to the header in each iteration
    }
    ##include NAs in the compiled data but be warned of their presence
    if (response=="2") {
      data<-read.csv(all_paths[i],header = TRUE, stringsAsFactors = FALSE)
      data[,days]<- gsub(".+([0-9]{3}).*", "\\1", all_paths[i])
      data[,country]<-gsub(".+([A-Z]{1}).*", "\\1", all_dir[i])
      header<-rbindlist(list(header,data),fill = TRUE)
      print("Warning, the data generated contains NA value (s) that weren't removed during the merging process")
    }
    ##compile data even with NAs
    if (response=="3"){
      data<-read.csv(all_paths[i],header = TRUE, stringsAsFactors = FALSE)
      data[,days]<- gsub(".+([0-9]{3}).*", "\\1", all_paths[i])
      data[,country]<-gsub(".+([A-Z]{1}).*", "\\1", all_dir[i])
      header<-rbindlist(list(header,data),fill = TRUE)
    }
  }
  header<-(header[-1,]) #after loop is finished, the extra row is removed 
  write.csv(header,file = "combined.csv")
}

##Function 3
#This function summarizes the number of screens run, percent of patients screened that were 
#infected, male vs. female patients, and the age distribution

summary<-function(file){
##First load the data from the combined file
files<-read.csv(file, header = TRUE, sep = ",")

library(ggplot2)
library(cowplot)

#Determine the number of screens, which is equal to the number of rows
Total_Screens<-nrow(files)
#Determine the total number for each country
fileX<-subset(files, country == "X")
Country_X_Screens<-nrow(fileX)
fileY<-subset(files, country == "Y")
Country_Y_Screens<-nrow(fileY)
#Make a data frame of Total, Country X, and Country Y
Location<-c("Total", "Country X", "Country Y")
Screen<-c(Total_Screens, Country_X_Screens, Country_Y_Screens)
Screendf<-data.frame(Location, Screen)
#Plot Data
plota<-ggplot(data = Screendf, aes(x = Location, y = Screen))+
  geom_col(aes(fill = Location)) + ggtitle("Number of Screens")+
  theme(legend.position = "none") + theme(text = element_text(hjust = 0.5)) +
  theme(text = element_text(size=8)) + theme(axis.text.x = element_text(angle = 45, vjust = .9, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

#Overall Percentage of infected patients 
##This section works by looking at each row individually and determining if any of the markers are a 1, meaning
#the patient was infected. The outcomes are stored in vectors for infected and uninfected, which are summed to 
#determine the total infected patients and total overall patients to calculate the percent. 
infected<-c()
uninfected<-c()
for (i in 1:nrow(files)){
  if (files$marker01[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker02[i]==1){
    infected<-c(infected,1)
    uninfected<-c(uninfected,0)}
  else if (files$marker03[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker04[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker05[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker06[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker07[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker08[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker09[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else if (files$marker10[i]==1){
    infected<-c(infected, 1)
    uninfected<-c(uninfected,0)}
  else{
    infected<-c(infected,0)
    uninfected<-c(uninfected,1)
  }
}

#Find the number of infected individuals to uninfected
sum_inf<-sum(infected)
sum_non<-sum(uninfected)
total<-sum_inf + sum_non
per_inf<-(sum_inf/total)*100

#Percent infected in Country X
Xinfected<-c()
Xuninfected<-c()
fileX<-subset(files, country == "X")
for (i in 1:nrow(fileX)){
  if (fileX$marker01[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker02[i]==1 ){
    Xinfected<-c(Xinfected,1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker03[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker04[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker05[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker06[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker07[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker08[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker09[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else if (fileX$marker10[i]==1 ){
    Xinfected<-c(Xinfected, 1)
    Xuninfected<-c(Xuninfected,0)}
  else{
    Xinfected<-c(Xinfected,0)
    Xuninfected<-c(Xuninfected,1)
  }
}

#Find the number of infected individuals to uninfected
Xsum_inf<-sum(Xinfected)
Xsum_non<-sum(Xuninfected)
Xtotal<-Xsum_inf + Xsum_non
Xper_inf<-(Xsum_inf/Xtotal)*100

#Percent in Country Y
Yinfected<-c()
Yuninfected<-c()
fileY<-subset(files, country == "Y")
for (i in 1:nrow(fileY)){
  if (fileY$marker01[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker02[i]==1 ){
    Yinfected<-c(Yinfected,1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker03[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker04[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker05[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker06[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker07[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker08[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker09[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else if (fileY$marker10[i]==1 ){
    Yinfected<-c(Yinfected, 1)
    Yuninfected<-c(Yuninfected,0)}
  else{
    Yinfected<-c(Yinfected,0)
    Yuninfected<-c(Yuninfected,1)
  }
}

#Find the number of infected individuals to uninfected
Ysum_inf<-sum(Yinfected)
Ysum_non<-sum(Yuninfected)
Ytotal<-Ysum_inf + Ysum_non
Yper_inf<-(Ysum_inf/Ytotal)*100

#Make summary data frame for infected vs non infected and percent
TValues<-c(sum_inf,sum_non)
XValues<-c(Xsum_inf, Xsum_non)
YValues<-c(Ysum_inf, Ysum_non)
Label<-c("Infected", "Uninfected")

SUMdf<-data.frame(Group = rep(c("Total", "Country X", "Country Y"), each = 2), 
                  Subgroup = c("Infected", "Uninfected"), 
                  Patients = c(sum_inf, sum_non, Xsum_inf, Xsum_non, Ysum_inf, Ysum_non))

Percent<-c(per_inf, Xper_inf, Yper_inf)
Labelper<-c("Total", "Country X", "Country Y")

Percentdf<-data.frame(Labelper, Percent)

#Plot Summarizing Data
plotb<-ggplot(data = SUMdf, aes(x = Group, y = Patients, fill = Subgroup))+
  geom_bar(stat = "identity", position = "dodge") + xlab("Country") + ylab("Number of \n Patients") +
  ggtitle("Infected and Uninfected \n Patients for each Country") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, vjust = .9, hjust=1))


plotc<-ggplot(data = Percentdf, aes(x = Labelper, y = Percent, fill = Labelper)) + 
  geom_col()+ xlab("Country") + ylab("Percent") + ggtitle("Percent of Patients Infected") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, vjust = .9, hjust=1)) + theme(legend.position = "none")

#Determine the number of males to females 
#This section uses a for loop to determine which rows have the gender column as female. If the gender = female, 
#then the vector female gets a 1 added to it and if the gender does not equal female, a 1 is added to the male vector.
#The total number of males and females is determined by taking the length of the vector. 
female<-c()
male<-c()
for (i in 1:nrow(files)){
  if (files$gender[i]=="female"){
    female<-c(female,1)
    }else{
      male<-c(male,1)
  }
}
tot_fem<-length(female)
tot_mal<-length(male)

#Males to Females for Country X
Xfemale<-c()
Xmale<-c()
for (i in 1:nrow(fileX)){
  if (fileX$gender[i]=="female"){
    Xfemale<-c(Xfemale,1)
  }else{
    Xmale<-c(Xmale,1)
  }
}
Xtot_fem<-length(Xfemale)
Xtot_mal<-length(Xmale)

#Males to Females for Country Y
Yfemale<-c()
Ymale<-c()
for (i in 1:nrow(fileY)){
  if (fileY$gender[i]=="female"){
    Yfemale<-c(Yfemale,1)
  }else{
    Ymale<-c(Ymale,1)
  }
}
Ytot_fem<-length(Yfemale)
Ytot_mal<-length(Ymale)

#Output as data frame 
fvsmT<-c(tot_fem, tot_mal)
fvsmX<-c(Xtot_fem, Xtot_mal)
fvsmY<-c(Ytot_fem, Ytot_mal)
labs<-c("Female Patients", "Male Patients") 
                
fvsmdf<-data.frame(Group = rep(c("Total", "Country X", "Country Y"), each = 2), 
                   Subgroup = c("Female", "Male"), 
                   Patient = c(tot_fem, tot_mal, Xtot_fem, Xtot_mal, Ytot_fem, Ytot_mal))

#Plot
plotd<-ggplot(data = fvsmdf, aes(x = Group, y = Patient, fill = Subgroup))+
  geom_bar(stat = "identity", position = "dodge") + xlab("Country") + ylab("Number of \n Patients") +
  ggtitle("Female vs Male \n Patients for each Country") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text= element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, vjust = .9, hjust=1))

#Determine the age distribution 
#The age distribution is determined by creating 12 groups of ages, creating an empty vector for each group. The age column 
#is looked at and if the logic statement is true, a 1 is added to that specific empty vector. The vectors are summed at the 
#end to get a complete distribution table.
lst<-sort(files$age, decreasing = FALSE)

g1<-c()
g2<-c()
g3<-c()
g4<-c()
g5<-c()
g6<-c()
g7<-c()
g8<-c()
g9<-c()
g10<-c()
g11<-c()
g12<-c()
for (i in 1:length(lst)){
  if (lst[i] <= 4){
    g1<-c(g1,1)}
  else if (lst[i]<=14 && lst[i]>=5){
    g2<-c(g2,1)}
  else if (lst[i]<=24 && lst[i]>=15){
    g3<-c(g3,1)}
  else if (lst[i]<=34 && lst[i]>=25){
    g4<-c(g4,1)}
  else if (lst[i]<=44 && lst[i]>=35){
    g5<-c(g5,1)}
  else if (lst[i]<=54 && lst[i]>=45){
    g6<-c(g6,1)}
  else if (lst[i]<=64 && lst[i]>=55){
    g7<-c(g7,1)}
  else if (lst[i]<=74 && lst[i]>=65){
    g8<-c(g8,1)}
  else if (lst[i]<=84 && lst[i]>=75){
    g9<-c(g9,1)}
  else if (lst[i]<=94 && lst[i]>=85){
    g10<-c(g10,1)}
  else if (lst[i]<=104 && lst[i]>=95){
    g11<-c(g11,1)}
  else{
    g12<-c(g12,1)
  }
}
g1a<-sum(g1)
g2a<-sum(g2)
g3a<-sum(g3)
g4a<-sum(g4)
g5a<-sum(g5)
g6a<-sum(g6)
g7a<-sum(g7)
g8a<-sum(g8)
g9a<-sum(g9)
g10a<-sum(g10)
g11a<-sum(g11)
g12a<-sum(g12)
Group<-c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85-94", "95-104", "105+")
Patients<-c(g1a,g2a,g3a,g4a,g5a,g6a,g7a,g8a,g9a,g10a,g11a,g12a)
agedata<-data.frame(Group, Patients)

##For Country X
lstX<-sort(fileX$age, decreasing = FALSE)

g1X<-c()
g2X<-c()
g3X<-c()
g4X<-c()
g5X<-c()
g6X<-c()
g7X<-c()
g8X<-c()
g9X<-c()
g10X<-c()
g11X<-c()
g12X<-c()
for (i in 1:length(lstX)){
  if (lstX[i] <= 4){
    g1X<-c(g1X,1)}
  else if (lstX[i]<=14 && lstX[i]>=5){
    g2X<-c(g2X,1)}
  else if (lstX[i]<=24 && lstX[i]>=15){
    g3X<-c(g3X,1)}
  else if (lstX[i]<=34 && lstX[i]>=25){
    g4X<-c(g4X,1)}
  else if (lstX[i]<=44 && lstX[i]>=35){
    g5X<-c(g5X,1)}
  else if (lstX[i]<=54 && lstX[i]>=45){
    g6X<-c(g6X,1)}
  else if (lstX[i]<=64 && lstX[i]>=55){
    g7X<-c(g7X,1)}
  else if (lstX[i]<=74 && lstX[i]>=65){
    g8X<-c(g8X,1)}
  else if (lstX[i]<=84 && lstX[i]>=75){
    g9X<-c(g9X,1)}
  else if (lstX[i]<=94 && lstX[i]>=85){
    g10X<-c(g10X,1)}
  else if (lstX[i]<=104 && lstX[i]>=95){
    g11X<-c(g11X,1)}
  else{
    g12X<-c(g12X,1)
  }
}
g1aX<-sum(g1X)
g2aX<-sum(g2X)
g3aX<-sum(g3X)
g4aX<-sum(g4X)
g5aX<-sum(g5X)
g6aX<-sum(g6X)
g7aX<-sum(g7X)
g8aX<-sum(g8X)
g9aX<-sum(g9X)
g10aX<-sum(g10X)
g11aX<-sum(g11X)
g12aX<-sum(g12X)
Patients_X<-c(g1aX,g2aX,g3aX,g4aX,g5aX,g6aX,g7aX,g8aX,g9aX,g10aX,g11aX,g12aX)


##For Country Y 
lstY<-sort(fileY$age, decreasing = FALSE)

g1Y<-c()
g2Y<-c()
g3Y<-c()
g4Y<-c()
g5Y<-c()
g6Y<-c()
g7Y<-c()
g8Y<-c()
g9Y<-c()
g10Y<-c()
g11Y<-c()
g12Y<-c()
for (i in 1:length(lstY)){
  if (lstY[i] <= 4){
    g1Y<-c(g1Y,1)}
  else if (lstY[i]<=14 && lstY[i]>=5){
    g2Y<-c(g2Y,1)}
  else if (lstY[i]<=24 && lstY[i]>=15){
    g3Y<-c(g3Y,1)}
  else if (lstY[i]<=34 && lstY[i]>=25){
    g4Y<-c(g4Y,1)}
  else if (lstY[i]<=44 && lstY[i]>=35){
    g5Y<-c(g5Y,1)}
  else if (lstY[i]<=54 && lstY[i]>=45){
    g6Y<-c(g6Y,1)}
  else if (lstY[i]<=64 && lstY[i]>=55){
    g7Y<-c(g7Y,1)}
  else if (lstY[i]<=74 && lstY[i]>=65){
    g8Y<-c(g8Y,1)}
  else if (lstY[i]<=84 && lstY[i]>=75){
    g9Y<-c(g9Y,1)}
  else if (lstY[i]<=94 && lstY[i]>=85){
    g10Y<-c(g10Y,1)}
  else if (lstY[i]<=104 && lstY[i]>=95){
    g11Y<-c(g11Y,1)}
  else{
    g12Y<-c(g12Y,1)
  }
}
g1aY<-sum(g1Y)
g2aY<-sum(g2Y)
g3aY<-sum(g3Y)
g4aY<-sum(g4Y)
g5aY<-sum(g5Y)
g6aY<-sum(g6Y)
g7aY<-sum(g7Y)
g8aY<-sum(g8Y)
g9aY<-sum(g9Y)
g10aY<-sum(g10Y)
g11aY<-sum(g11Y)
g12aY<-sum(g12Y)
Patients_Y<-c(g1aY,g2aY,g3aY,g4aY,g5aY,g6aY,g7aY,g8aY,g9aY,g10aY,g11aY,g12aY)
#Create Data Frame
agedata<-data.frame(Group, Patients, Patients_X, Patients_Y)
#Create plots

plote<-ggplot(data = agedata, aes(x = factor(Group, level = Group), y = Patients))+ geom_col(color = "#F8766D", fill = "#F8766D") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Age Group")+ylab("Number of Patients")+
  ggtitle("Age Distribution of \n Total Patients")+ theme(text = element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.5, hjust=0.25))

plotf<-ggplot(data = agedata, aes(x = factor(Group, level = Group), y = Patients_X))+ geom_col(color = "#619CFF", fill = "#619CFF") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Age Group")+ylab("Number of Patients")+
  ggtitle("Age Distribution of \n Patients in Country X") + theme(text = element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.5, hjust=0.25))

plotg<-ggplot(data = agedata, aes(x = factor(Group, level = Group), y = Patients_Y))+ geom_col(color = "#00BA38", fill = "#00BA38") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Age Group")+ylab("Number of Patients")+
  ggtitle("Age Distribution of \n Patients in Country Y") + theme(text = element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.5, hjust=0.25))
  

fig<-plot_grid(plota, plotb, plotc, plotd, plote, plotf, plotg, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))
print("Data for Screenings")
print(Screendf)
print("Summary of infected vs uninfected patients")
print(SUMdf)
print("Percent of patients infected")
print(Percentdf)
print("Female vs Male Patient Data")
print(fvsmdf)
print("Age Distribution Data")
print(agedata)
return(fig)
}

##Function 4
#This function will determine the markers that are present in each country for the infections
files<-"combined.csv"

heterogeneity<-function(files){
 
  ##First load the data from the combined file
  file<-read.csv(files, header = TRUE, sep = ",")
  #Subset files by country
  fileX<-subset(file, country == "X")
  fileY<-subset(file, country == "Y")
  
  m1x<-sum(fileX$marker01)
  m2x<-sum(fileX$marker02)
  m3x<-sum(fileX$marker03)
  m4x<-sum(fileX$marker04)
  m5x<-sum(fileX$marker05)
  m6x<-sum(fileX$marker06)
  m7x<-sum(fileX$marker07)
  m8x<-sum(fileX$marker08)
  m9x<-sum(fileX$marker09)
  m10x<-sum(fileX$marker10)
  markerX<-c(m1x, m2x, m3x, m4x, m5x, m6x, m7x, m8x, m9x, m10x)
  
  m1y<-sum(fileY$marker01)
  m2y<-sum(fileY$marker02)
  m3y<-sum(fileY$marker03)
  m4y<-sum(fileY$marker04)
  m5y<-sum(fileY$marker05)
  m6y<-sum(fileY$marker06)
  m7y<-sum(fileY$marker07)
  m8y<-sum(fileY$marker08)
  m9y<-sum(fileY$marker09)
  m10y<-sum(fileY$marker10)
  markerY<-c(m1y, m2y, m3y, m4y, m5y, m6y, m7y, m8y, m9y, m10y)
  
  Xdays<-fileX$dayofYear
  Ydays<-fileY$dayofYear
  
  markerdf<-data.frame(markerX, markerY)
  
  #Function for looking at marker changes over days for each country 
  
  file<-read.csv(files, header = TRUE, sep = ",")
  #Subset files by country
  fileX<-subset(file, country == "X")
  fileY<-subset(file, country == "Y")
  
  Xdf<-fileX[, c(4,5,6,7,8,9,10,11,12,13,14)]
  Xmark<-aggregate(x = Xdf, by = list(Xdf$dayofYear), FUN = sum)
  Xmarkdata<-data.frame(Xmark[,c(1:11)])
  
  Ydf<-fileY[, c(4,5,6,7,8,9,10,11,12,13,14)]
  Ymark<-aggregate(x = Ydf, by = list(Ydf$dayofYear), FUN = sum)
  Ymarkdata<-data.frame(Ymark[,c(1:11)])
  
  #Plot X marker data and Y marker data 
  library(ggplot2)
  library(cowplot)
    
  plot1<-ggplot(Xmarkdata, aes(Group.1)) + 
    geom_line(aes(y = marker01, colour = "marker01")) + 
    geom_line(aes(y = marker02, colour = "marker02")) +
    geom_line(aes(y = marker03, colour = "marker03")) +
    geom_line(aes(y = marker04, colour = "marker04")) +
    geom_line(aes(y = marker05, colour = "marker05")) +
    geom_line(aes(y = marker06, colour = "marker06")) +
    geom_line(aes(y = marker07, colour = "marker07")) +
    geom_line(aes(y = marker08, colour = "marker08")) +
    geom_line(aes(y = marker09, colour = "marker09")) +
    geom_line(aes(y = marker10, colour = "marker10")) +
    xlab("Day of Year") + ylab("Occurrence")+ ggtitle("Changes in Markers over \n Time in Country X")+
    scale_color_discrete("Marker")+theme(plot.title = element_text(hjust = 0.5))
  
  plot2<-ggplot(Ymarkdata, aes(Group.1)) + 
    geom_line(aes(y = marker01, colour = "marker01")) + 
    geom_line(aes(y = marker02, colour = "marker02")) +
    geom_line(aes(y = marker03, colour = "marker03")) +
    geom_line(aes(y = marker04, colour = "marker04")) +
    geom_line(aes(y = marker05, colour = "marker05")) +
    geom_line(aes(y = marker06, colour = "marker06")) +
    geom_line(aes(y = marker07, colour = "marker07")) +
    geom_line(aes(y = marker08, colour = "marker08")) +
    geom_line(aes(y = marker09, colour = "marker09")) +
    geom_line(aes(y = marker10, colour = "marker10")) +
    xlab("Day of Year") + ylab("Occurrence")+ ggtitle("Changes in Markers over \n Time in Country Y")+
    scale_color_discrete("Marker")+theme(plot.title = element_text(hjust = 0.5))
  
  Figure_1<-plot_grid(plot1, plot2, labels = c('A', 'B'))
    
  print("Marker Data for Country X")
  print(Xmarkdata)
  print("Marker Data for Country Y")
  print(Ymarkdata)
  return(Figure_1)
}



    
