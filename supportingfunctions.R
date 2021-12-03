#setwd("/Users/Quintonhayre/Desktop/R_Biocomp/Rproject2021/") #Will change based on user
#Quinton Hayre 


fileconvertcsv = function(directory){
  setwd(directory)
  filelist = list.files(pattern = ".txt")
  if (length(filelist) > 0){
    for (i in 1: length(filelist)){
      output = paste0(gsub("\\.txt$", "", filelist[i]), ".csv")
      data = read.table(filelist[i], sep = " ", header = TRUE)
      write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
    }
  }else{
    print(paste0("Your directory ", directory, " does not have any .txt files to convert"))
  }
}

#Examples for above function: 
  #countryY
    #fileconvertcsv("/Users/Quintonhayre/Desktop/R_Biocomp/Rproject2021/countryY/")
  #countryX
    #fileconvertcsv("/Users/Quintonhayre/Desktop/R_Biocomp/Rproject2021/countryX/")

allonecsv = function(directory, countryname){
  library(readr)
  setwd(directory)
  filelist = list.files(pattern = ".csv")
  filelist = filelist[grepl("screen", filelist)] #incase rerunning with allcsv included so does get added 
  dataframe = data.frame(matrix(ncol = 12, nrow = 0))
  ls = list()
  yy = as.character(readline(prompt = "Would you like to be warned about NAs (yes or no): "))
  for (i in 1:length(filelist)){
    if (yy == "yes"){
      datatobind = read_csv(filelist[i])
      datatobind$day = substr(filelist[i], 8, 10)#assuming that the pattern of filenames will remain "screen_###.csv"
      if (TRUE %in% is.na(datatobind)){
        print(paste0((filelist[i]), " contains an NA"))
        ls = list(append(ls, filelist[i]))
        x = as.character(readline(prompt = "Do you want to remove rows with NA’s in any columns (yes or no): "))
        if (x == "yes"){
          datatobind = na.omit(datatobind) #removes entire row if a single NA value 
        }
      }
      dataframe = rbind(dataframe, datatobind)
    }else{
      datatobind = read_csv(filelist[i])
      datatobind$day = substr(filelist[i], 8, 10)#assuming that the pattern of filenames will remain "screen_###.csv"
      dataframe = rbind(dataframe, datatobind)
    }
  }
  dataframe$country = countryname
  if (yy == "yes"){
    print(paste0("The following days contained NA/s and were removed: ", ls))
  }
  write.csv(dataframe, file = paste0(countryname, "all.csv"), row.names = F)
}
#Examples for above function:
  #Ydirectory
    #allonecsv("/Users/Quintonhayre/Desktop/R_Biocomp/Rproject2021/countryY/", "countryY")
  #Xdirectory
    #allonecsv("/Users/Quintonhayre/Desktop/R_Biocomp/Rproject2021/countryX/", "countryX")



sumdir = function(directory, countryname){
  library(ggplot2)
  setwd(directory)
  data = read.csv(file = paste0(countryname, "all.csv"), header = TRUE) #makes it adaptable
  #age distribution 
  x = ggplot(data, aes(x = age)) + xlim(0,100) + geom_histogram(binwidth = 10)  + 
    xlab("Age") + labs(title = paste0("Age Distribution of ", countryname), subtitle = "Domain limited to 100 due to strong right skew") + 
    theme_classic()
  print(x)
  #number of screens 
  total_screen_num = nrow(data) - 1 #minus one to get rid of column values
  fulltable = data.frame(total_screen_num)# initating the new table
  # % infected
  relevant_data_infected = data[,c("marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10")]
  num_infected = length(which(rowSums(relevant_data_infected) > 0))
  percentinfected = num_infected / total_screen_num
  fulltable$total_percent_infected = percentinfected
  #gender
  fulltable$total_num_female = length(which(data$gender == "female")) 
  fulltable$total_num_male = length(which(data$gender == "male"))
  #Table
  return(fulltable)
}

##Examples for above function:
  #Ydirectory
    #sumdir("/Users/Quintonhayre/Desktop/R_Biocomp/Rproject2021/countryY/", "countryY")
  #Xdirectory
  #sumdir("/Users/Quintonhayre/Desktop/R_Biocomp/Rproject2021/countryX/", "countryX")




