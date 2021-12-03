setwd("~/Desktop/RProject2021")
##Assisting Functions for Project Code##

##Convert all files into csv files from space/tab delimintated ones##
Convert_csv<-function(dir){
  setwd(dir) 
  path<-list.files(path=dir, pattern=".txt", recursive = TRUE)
  for (i in 1:length(path)){
    file<-path[i]
    out<-paste0(gsub("\\.txt$","",file),".csv")
    data<-read.table(file, header= TRUE)
    write.csv(data, file=out)
  }
}
  

##Compile all Data into a single csv file##
#Only running for one file? also getting NA for column data 
compile<-function(dir, name, y){
  setwd(dir)
  csv_files<-list.files(path=dir,pattern='*(\\d+).csv', recursive=TRUE, full.names = FALSE)
  for(i in 1:length(csv_files)){
    x<-read.table(csv_files[i], header = TRUE,sep = ",", stringsAsFactors = FALSE)
    dayofYear<-substr(csv_files[i],start=17, stop=19)
    country<-substr(csv_files[i], start=1, stop=8)
    z<-cbind(x,dayofYear,country)
    result1<-z
    results<-rbind(z, x)
  } #To deal with NAs#
      if(y==1){
        results<-na.omit(results)
      }else if(y==2){
        return(print("Warning: File contains NAs"))
      }else if(y==3){
      }
  write.csv(results, file=name)
}


# Summarize function that takes complied data set and returns summary of number of screens run
# percent of patients screened that were infected, male vs female patients, and the age distribution of patients. 
# Supporting function that takes complied data set and returns summary of number of screens run
# percent of patients screened that were infected, male vs female patients, and the age distribution of patients. 
summarize<-function(i){
  data<-read.table(file=i, header=TRUE, sep=",")
  n<-count(data[,])
  print("number of screen runs", n)
  #number of rows with at least 1 present 
  #count male vs. female
  print("male", "female")
  #age
  summary(data$age)
}


# Summarize function (number 3 in supportingFunctions.R)

# Set working directory
# Function is designed to be used in the same directory as the compiled data set 
setwd("~/Desktop/Fall-2021/Biocomputing/")

# Usage: summarize("filename.csv")

summarize <- function(filename){
  filename_table <- read.table(filename, sep = ",", stringsAsFactors = TRUE, header = TRUE)
  
  # Summary of number of screens run 
  total_screens <- nrow(filename_table)
  print(paste0("Number of screens run: ",total_screens))
  
  # Percent of patients screened that were infected 
  infected<-0
  for(i in 1:nrow(filename_table)){
    if(sum(filename_table[i,3:12])>=1){
      infected<-infected+1
    }else{
    }
  }
  percent <- (infected/total_screens)*100
  print(paste0("Percent of patients screened that were infected: ",round(percent, digits = 2),"%"))
  
  # Male vs female patients
  gender<- data.frame(table(filename_table$gender))
  colnames(gender)<-c('Gender','Freq')
  
  male<-gender[2,2]
  print(paste0("Number of male patients: ",male))
  female<-gender[1,2]
  print(paste0("Number of female patients: ",female))
  
  # Load plot libraries
  library(ggplot2)
  library(cowplot)
  plot1 <- ggplot(data=gender, aes(x=Gender,y=Freq,fill=Gender))+
    geom_bar(stat="identity")+
    ylab("Number of Patients")+
    ggtitle("Male vs Female")+
    scale_fill_manual(values = c('lightblue','darkblue'))+
    theme_classic()
  
  # Age distribution
  plot2 <- ggplot(data=filename_table,aes(x=age))+
    geom_histogram(binwidth = 10, fill="lightblue", color="darkblue")+ 
    ylab("Number of Patients")+
    xlab("Age")+
    ggtitle("Age Distribution")+
    theme_classic() 
  
  # Display both summary graphs in console
  plot_grid(plot1, plot2, ncol=2, nrow =1, width = 8, height = 5)
}

# Call function
summarize("allData.csv")
