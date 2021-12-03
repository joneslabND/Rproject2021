# function to convert files into comma-separated files

file2comma <- function(dir) {
  setwd(dir)
  list_files = list.files(path = dir, pattern = "*.txt")
  for (f in list_files) {
    data <- read.table(f, header = TRUE, sep = "", stringsAsFactors = FALSE)
    outputfile = sub(".txt", ".csv", f)
    write.table(data, file = outputfile, sep = ",", col.names = T, row.names = F)
  }
}





# function to compile data from all csv files in a directory into single csv file
# original 12 columns should be conserved and added country and dayofYear
# user can choose if they want to remove rows with NA, include NA but be warned or include NA without warning

compile <- function(dirIn1, dirIn2, dirFileOut, NA_opt){
  setwd(dirIn1)
  list_files_X = list.files(path = dirIn1, pattern = "*.csv")
  all_data_X = NULL
  for (file_X in list_files_X) {
    data_X = read.table(file_X, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    day_X = as.numeric(gsub(".*?([0-9]+).*", "\\1", file_X))
    data_X$country <- "X"
    data_X$dayofYear <- day_X
    all_data_X = rbind(all_data_X, data_X)
  }
  
  setwd(dirIn2)
  list_files_Y = list.files(path = dirIn2, pattern = "*.csv")
  all_data_Y = NULL
  for (file_Y in list_files_Y) {
    data_Y = read.table(file_Y, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    day_Y = as.numeric(gsub(".*?([0-9]+).*", "\\1", file_Y))
    data_Y$country <- "Y"
    data_Y$dayofYear <- day_Y
    all_data_Y = rbind(all_data_Y, data_Y)
  }
  
  all_data = rbind(all_data_X, all_data_Y)
  
  NA_opt = 1
  if (NA_opt == 0) {
    all_data = na.omit(all_data)
  } else if (NA_opt == 1) {
    if (sum(is.na(all_data)) > 0) {
      print("Warning! NA are included.")
    }
    else{
    }
  } else if (NA_opt == 2) {
  }
  
  write.table(all_data, file = dirFileOut, sep = ",", col.names = T, row.names = F)
}





# function to summarize compiled data

summarizeData <- function(dir, file) {
  library(ggplot2)
  
  setwd(dir)
  data = read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # x and y country female and male distribution
  country_x = data[data$country == "X",]
  country_y = data[data$country == "Y",]
  
  
  # male vs female patients
  FvsMdist <- function(datafile, title) {
    female = datafile[datafile$gender == "female",]
    male = datafile[datafile$gender == "male",]
    
    female_count = nrow(female)
    male_count = nrow(male)
    
    female_perc = signif((female_count * 100) / nrow(datafile), digits = 4)
    male_perc = signif((male_count * 100) / nrow(datafile), digits = 4)
    
    new_data = data.frame(gender = c("female", "male"), count = c(female_count, male_count),
                          perc = c(female_perc, male_perc))
    
    graph = ggplot(new_data, aes(x = "", y = count, fill = gender)) +
      geom_bar(stat = "identity", color = "white") + 
      coord_polar("y") +
      geom_text(aes(label = paste0(perc,"%")), position = position_stack(vjust = 0.5, reverse = FALSE)) +
      ggtitle(title) +
      theme_void()
    
    return(graph)
  }
  
  # plots of female and male distribution
  FvsM1 = FvsMdist(data, "Female vs male distribution (overall)")
  FvsM2 = FvsMdist(country_x, "Female vs male distribution of country X")
  FvsM3 = FvsMdist(country_y, "Female vs male distribution of country Y")
  
  
  # age distribution
  
  ageDist <- function(datafile, title, color) {
    hist(datafile$age, main = title, xlab = "Age", border = "black",
         col = color, xlim = c(0,430), las = 1, breaks = 20, prob = TRUE)
    lines(density(datafile$age))
  }
  
  age1 = ageDist(data, "Age distribution of patients (overall)", "red")
  age2 = ageDist(country_x, "Age distribution of patients in country X", "blue")
  age3 = ageDist(country_y, "Age distribution of patients in country Y", "green")
  
  # In case we want to exclude the outliers, we would use the code that is commented out below:
  
  #clean_data = data[data$age < 130, ]
  #clean_country_x = clean_data[clean_data$country == "X",]
  #clean_country_y = clean_data[clean_data$country == "Y",]
  
  #age1 = ageDist(clean_data, "Age distribution of patients (overall)", "red")
  #age2 = ageDist(clean_country_x, "Age distribution of patients in country X", "blue")
  #age3 = ageDist(clean_country_y, "Age distribution of patients in country Y", "green")
  
  #ageDist <- function(datafile, title, color) {
  #  hist(datafile$age, main = title, xlab = "Age", border = "black",
  #       col = color, xlim = c(0,430), las = 1, breaks = 20, prob = TRUE)
  #  lines(density(datafile$age))
  #}
  
  
  # number of screens run
  print(nrow(data))
  
  # percent of patients screened that were infected
  library(dplyr)
  
  markers = data[, 3:12]
  
  infectionPerc <- function(marker, title) {
    length_markers = dim(marker)[1]
    infected = marker %>% filter_all(any_vars(. %in% c(1)))
    
    total_patients = nrow(data)
    infected_patients = nrow(infected)
    infected_percent = signif(((infected_patients * 100) / total_patients), digits = 4)
    healthy_percent = signif((((total_patients - infected_patients) * 100) / total_patients), digits = 4)
    
    new_data = data.frame(status = c("healthy", "infected"),
                          count = c((total_patients-infected_patients),
                                    infected_patients), perc = c(healthy_percent, infected_percent))
    
    graph = ggplot(new_data, aes(x = "", y = count, fill = status)) +
      geom_bar(stat = "identity", color = "white") + 
      coord_polar("y") +
      geom_text(aes(label = paste0(perc,"%")), position = position_stack(vjust = 0.5, reverse = FALSE)) +
      ggtitle(title) +
      scale_fill_manual(values=c("#7CAE00", "#F8766D")) +
      theme_void()
    
    return(graph)
  }
  
  inf1 = infectionPerc(markers, "Percentage of patients screened infected (overall)")
  inf2 = infectionPerc(country_x, "Percentage of patients screened infected in country X")
  inf3 = infectionPerc(country_y, "Percentage of patients screened infected in country Y")
  
  return(list(FvsM1, FvsM2, FvsM3, age1, age2, age3, inf1, inf2, inf3))
}




