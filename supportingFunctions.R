##supporting functions
##Emily Selland and John Kane

setwd("/Users/eselland/Documents/ND/IntroBiocomputing/R_project/Rproject2021/")


#########Convert files to .csv#########
#converts all files in a directory with space- or tab-delimited data (.txt) into comma-separated value files
#user must tell the function what sep is separating these files (tab or space or comma)

ConvertToCSV<-function(dir){
  filelist <- list.files(pattern = ".txt")
  CSVfiles <- lapply(filelist, function(x) { 
    textfile <- read.table(x)
    write.csv(textfile, 
              file = sub(pattern = "\\.txt$", replacement = ".csv", x = x))
  })
}
ConvertToCSV("/Users/eselland/Documents/ND/IntroBiocomputing/R_project/Rproject2021/countryY")


## I didn't touch any of this -EKS

######First we need to add the Day of Year column to each file in each directory

DayofYear<-function(dir){#this function will add a column with the day of year to any files in dir
  filelist<-list.files(path = dir, pattern = ".csv")
  for (i in 1:length(filelist)) {
    temp<-as.name(filelist[i])
    temp<-as.character(temp)
    temp<-unlist(strsplit(temp,split="_"))
    temp<-temp[2]
    temp<-unlist(strsplit(temp,split = ".txt"))
    #now temp only holds 120
    filelist[i]$dayofYear<-c(temp)
    #now we have put that day of year into a column for every file, respective of what DOY it is
  }
}
#these files are now ready to be combined into one file

############Compile data into one file###########
#Write a function to compile data from all .csv files in a directory into a single .csv file. The compiled
#data should have the original twelve columns from daily data sheets, but also country and dayofYear
#columns. The user should be able to choose whether they want to remove rows with NA’s in any
#columns, include NAs in the compiled data but be warned of their presence, or include NAs in the
#compiled data without a warning
#user must input what country directory they are working in and this must be performed for each country separately


?read.csv
OneFile<-function(dir,Cntry){
  setwd(dir)
  filelist<-list.files(path = dir, pattern = ".csv")
  input1<-readline(prompt = "would you like to remove NAs(RM), include NAs with warning(warn), or include without warning(include)?")
  if (input1=="RM") { #if user selects to remove NAs
    for (i in 1:length(filelist)) {
      if (i==1) {
        alldata<-read.csv(filelist[i],header = TRUE)
      }else if (i>1) {
        alldata<-rbind(alldata,read.table(filelist[i],header = TRUE,sep=","))
      }
    }
    input2<-any(is.na(alldata))
    if (input2==TRUE) {#if there are NAs in the dataset then input2 will equal TRUE
      alldata<-alldata[complete.cases(alldata),]
      readline(prompt = "NAs have been removed")
    }else{
      readline(prompt = "no NAs detected")
    }
    alldata$country<-c("Cntry") #this adds a column for the country
    write.csv(alldata,file="alldata.csv")
  }else if (input1=="warn") { #if user selects to keep NAs but be warned
    for (i in 1:length(filelist)) {
      if (i==1) {
        alldata<-read.csv(filelist[i],header = TRUE)
      }else if (i>1) {
        alldata<-rbind(alldata,read.table(filelist[i],header = TRUE,sep=","))
      }
    }
    input2<-any(is.na(alldata))
    if (input2==TRUE) {
      readline(prompt = "WARNING: NAs have been detected, but not removed")
    }else{
      readline(prompt = "no NAs detected")
    }
    alldata$country<-c("Cntry") #this adds a column for the country
    write.csv(alldata,file="alldata.csv")
  }else if (input1=="include") {#if user selects to include NAs but not be warned
    for (i in 1:length(filelist)) {
      if (i==1) {
        alldata<-read.csv(filelist[i],header = TRUE)
      }else if (i>1) {
        alldata<-rbind(alldata,read.table(filelist[i],header = TRUE,sep=","))
      }
    }
    alldata$country<-c("Cntry") #this adds a column for the country
    write.csv(alldata,file="alldata.csv")
  }
}

OneFile("/Users/eselland/Documents/ND/IntroBiocomputing/R_project/Rproject2021/countryX")

OneFile("/Users/eselland/Documents/ND/IntroBiocomputing/R_project/Rproject2021/countryY")

#Now we have a file called alldata.csv which is a compiled list of all data from 
#that country

#Finally we want a function that will take these two files called alldata.csv
#and merge them into one file called alldata (probably shouldn't have been so redundant with variables)
#We want this file to be written in our working directory RProject2021 so that it is not 
#within one specific county's directory

CompiledData<-function(dir){
  setwd(dir)
  countryxcompiled<-read.csv("countryX/alldata.csv")
  countryycompiled<-read.csv("countryY/alldata.csv")
  alldata<-merge(countryxcompiled,countryycompiled)
  write.csv(alldata,file="alldata.csv")
}
  
CompiledData("/Users/eselland/Documents/ND/IntroBiocomputing/R_project/Rproject2021/")

##########Summarize compiled data###########
#summarize the compiled data set in terms of number of screens run, percent of
#patients screened that were infected, male vs. female patients, and the age distribution of patients.
summarizeData<-function(alldata){
  library(ggplot2)
  a<-nrow(alldata) #total number of screens run
  b<-(sum(na.omit(alldata$case))/nrow(alldata))*100 #percent patients infected
  c<-nrow(alldata[alldata$gender=="female",]) #number of female patients
  d<-nrow(alldata[alldata$gender=="male",]) #number of male patients
  e<-ggplot(data=alldata,aes(x=age))+
    geom_density()+
    scale_x_continuous(limits = c(0,80))+
    theme_classic() #density plot displaying age distribution
  #To eliminate some of the larger age values, we limited the ages to be 0-80
  print(e)
  return(c("Total Number Screens",a,"Percent Patients Infected",b,"Number Female Patients",c,"Number Male Patients",d,"Density Plot shows Age Distribution, with impossible outliers removed"))
}

summarizeData(alldata)
