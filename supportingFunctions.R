################### TXT to CSV coverter
txt_to_csv = function(diry){
  setwd(diry)
  fileList = list.files(pattern = ".txt")
  
  for (i in 1:length(fileList)){
    input = fileList[i]
    output <- paste0(gsub("\\.txt$", "", input), ".csv")
    newData = read.table(input, header = TRUE)
    write.csv(newData, file = output, row.names = F)
  }
  
  return(newData)
}



#################### merge all csv files into 1 file

#setwd("~/Documents/R/Rproject2021/countryY/")

Mergefile= function(diry,dirx,maindir)
{
  txt_to_csv(diry)

  i = 1
  file_list <- list.files(pattern = "*.csv")
  for (file in file_list){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read.csv(file, header=TRUE)
      num_years <- as.numeric(gsub("\\D+", "", file_list[i])) ## get the numeric value from file name
      
      ### adding columns to a csv file
      dataset =cbind(dataset, Country = "Y",DayofYear = num_years )
      i = i+1
    } else {
      temp_dataset <-read.csv(file, header=TRUE)
      num_years <- as.numeric(gsub("\\D+", "", file_list[i])) ## get the numeric value from file name
      
      ### adding columns to a csv file
      data2 =cbind(temp_dataset, Country = "Y",DayofYear = num_years )
      dataset <-rbind(dataset, data2)
      rm(temp_dataset)
      i = i+1
    }
  }
  
  setwd(dirx)
  j = 1
  xList <- list.files(pattern = "*.csv")
  for (file in xList){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("datasetX")){
      datasetX <- read.csv(file, header=TRUE)
      num_yearsX <- as.numeric(gsub("\\D+", "", xList[j])) ## get the numeric value from file name
      
      ### adding columns to a csv file
      datasetX =cbind(datasetX, Country = "X",DayofYear = num_yearsX )
      j = j+1
    } else {
      temp_dataset <-read.csv(file, header=TRUE)
      num_yearsX <- as.numeric(gsub("\\D+", "", xList[j])) ## get the numeric value from file name
      
      ### adding columns to a csv file
      dataX =cbind(temp_dataset, Country = "X",DayofYear = num_yearsX )
      datasetX <-rbind(datasetX, dataX)
      rm(temp_dataset)
      j = j+1
    }
  }
  
  setwd(maindir)
  oneDataset = rbind(datasetX, dataset) ## dataset = dataset Y
  
  ######
  print("If you want to remove any rows which include NA press 1")
  print("If you want to keep any rows which include NA, but recieve a warning press 2")
  print("If you want to keep any rows which include NA and run without warning press 3")
  user_input <- readline()
  
  if (user_input==1){
    for (k in 1: nrow(oneDataset)){
      for (l in 1:ncol(oneDataset))
        if (oneDataset[k,l]=="NA"){
          oneDataset= oneDataset[-c(k),]
          
        }else{
          
        }
    }
    
  }else if (user_input==2){
    for (k in 1: nrow(oneDataset)){
      for (l in 1:ncol(oneDataset))
        if (oneDataset[k,l]=="NA"){
          print(paste0("Warning: This row",k, "has NA in it"))
          
        }else{
          
        }
      
      
    }
  }else{
    
    
  }
  
  write.csv(oneDataset, file = paste0(maindir,"/theData.csv"), row.names = FALSE)
  return(oneDataset)
}



############################# Summary Function


Summary = function(maindir)
{

  setwd(maindir)
  oneDataset1 = read.csv(paste0(maindir,"/theData.csv"))
  num_of_screens=nrow(oneDataset1) ### number of screens run
  print(paste0("The number of screenning are ",num_of_screens))
  
  ### numbers of patients infected
  
  count = 0
  for (i in 1: nrow(oneDataset1)){
    for (j in 3:12)
      if (oneDataset1[i,j]==1){
        count = count +1
        break
      }else{
        
      }
  }
  perct = (count/num_of_screens)*100
  perct = round(perct, digits=2)
  print(paste0("The percentage of patients screened that were infected is ",perct, " %"))
  
  print("The following plots represent the age distribution of patient screenings and the number of male and female patients screened in each country")
  
  ########### plot the age distribution
  plot1 = ggplot(data = oneDataset1,
                 aes(x = age, fill = gender))+
    geom_bar()+
    facet_wrap(~Country)+
    xlab("Age (years)") +
    ylab("Number of Patients") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  plot2 = ggplot(data = oneDataset1,
                 aes(x = gender))+
    geom_bar(aes(fill = gender))+
    facet_wrap(~Country)+
    xlab("Gender") +
    ylab("Number of Patients") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(plot1)
    print(plot2)
    
}    
