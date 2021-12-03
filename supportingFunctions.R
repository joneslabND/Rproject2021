##This function takes a space or tab delimited file with a .txt extension and
##creates a .csv file of the same name, replacing the .txt with .csv 

#Usage: call the function cmdlfiles with the directory of files as the input

cmdlfiles<-function(...){
  #creates list for infinite amount of files that need conversion
  datalist <- list(...)
  #Goes sub directory by sub directory
  for(directory in datalist){
    setwd(directory)
    x<-list.files(pattern = "*.txt")
    #converts and adds files with same name, replacing .txt extension with .csv
    for(i in x){
      
      
      a<-read.table(i, header = TRUE)
      b<-gsub(pattern = "\\.txt$","",i)
      c<-assign(b,paste(b,".csv",sep = ""))
      write.csv(a, file = c, quote = FALSE, row.names = FALSE)
    }
    #resets wd to previous one with other subdirectories
    setwd("..")
  }
  
}
##This Function is used to merge the country csv together. And generate feedback if NA
##is detected. 
#Usage: Function is mergecsv with country directory as inputs. 
mergecsv<-function(...){
  country.final=NULL
  countries <- list(...)
  #Reads the csv in the list of each file directory
  for (b in countries){
    setwd(b)
    filename=c(list.files(pattern = "*.csv"))
    #Binds the country csv files together in "country.final)
    for (i in filename) {
      Xdf<-read.csv(i)
      Xdf[,(ncol(Xdf)+1)]=b
      dayofyear<-gsub(pattern = "[^0-9]", "", i)
      Xdf[,(ncol(Xdf)+1)] = dayofyear
      country.final<-rbind(country.final,Xdf)
    }
    setwd("..")
  }
#Rename given columns
  names(country.final)[13]<-"country"
  names(country.final)[14]<-"dayofyear"
  #Warning system based on if-else system with user responses
  if(anyNA(country.final) == TRUE){
    print("NA detected.")
    useranswer<-readline(prompt = "Would you like to:
          1) delete rows with NA 
          2) Include and be warned
          3) Include without warning")
    if(useranswer == "1"){
      country.final<-na.omit(country.final)
    }else if(useranswer == "2"){
      print("NA in data!")
      return(country.final)
    }else if(useranswer == "3"){
      return(country.final)
    }else{
      print("invalid entry")
    }
  }
  #Return the final bound csv file after warning system
  return(country.final)
}
##This function summarizes key data relevant for the analysis of the countries. 
#Usage: With the final csv from the above function, use as input to generate summary.
summarize.R<-function(findata){
  
  numscreens<-findata
  
  #Counts of screens conducted
  screens<-nrow(numscreens)
  cat("Total number of screens: ", screens, "\n")
  
  #Percent of patients screened that were infected
  totinfected<-nrow(numscreens[numscreens$marker01==1|numscreens$marker02==1|numscreens$marker03==1|
                                 numscreens$marker04==1|numscreens$marker05==1|numscreens$marker06==1|numscreens$marker07==1| 
                                 numscreens$marker08==1|numscreens$marker09==1|numscreens$marker10==1, ])
  
  #Calculate percent of patients infected
  percinfected=totinfected/screens*100
  cat("Percent of patients infected=", percinfected,"%\n")
  
  #Male vs Female screen
  male=nrow(numscreens[numscreens$gender=="male",])
  female=nrow(numscreens[numscreens$gender=="female",])
  cat("Number of male patients screened=", male, "\n")
  cat("Number of female patients screened=", female, "\n")
  
  library(ggplot2)
  #Age Distribution of Patients
  numscreens$group<-cut(numscreens$age, breaks=c(0,10,20,30,40,50,60,70,80,400),
                        labels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80+"), right=T)
  ggplot(numscreens, aes(x=group, fill=gender))+
    geom_bar(position = "dodge")+
    xlab("Age of Patients")+
    ylab("Number of Screens")
}
