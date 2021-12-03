# custom functions to handle data or summary tasks

# 1. Convert all files in a directory with space- or tab-delimited data (.txt) 
  #into csv files


# write function name with input dir
toCSV <- function(dir){
  # set up directory for new csv files 
  dir.create(path = "/Users/alexdosch/Desktop/Rproject2021/countryY_csv")
  # inputs files from a specified path (set by user)
  directory <- "/Users/alexdosch/Desktop/Rproject2021/countryY"
  ndirectory <- "/Users/alexdosch/Desktop/Rproject2021/countryY_csv"
  
  file_name = list.files(directory, pattern = ".txt")
  
  filestoRead <- paste(directory, file_name, sep="/")
  filestoWrite <- paste(ndirectory, paste0(sub(".txt", "", file_name), ".csv"), sep="/")
  
  for(i in 1:length(filestoRead)){
    temp = read.csv(filestoRead[i], header = TRUE, fill = TRUE)
    write.csv(temp, file = filestoWrite[i])
  }
}

# 2. compile data from all .csv files in a directory into a single .csv file
  # file should have original 12 columns, adding "country" and "dayofYear"
  # user should choose to remove/keep rows with NA's (+/- warning)

oneCSV <- function(dir1, dir2){
  #define directories -- this will be different for each input of data
  directory1 <- "/Users/alexdosch/Desktop/countryX"
  directory2 <- "/Users/alexdosch/Desktop/countryY_csv"
  
  # define set of files
  filesX <- list.files(directory1, pattern = ".csv")
  # open each file
  for (i in filesX){
  read.csv(i, header = TRUE, stringsAsFactors = FALSE) 
  # add columns "country" and "dayofYear"
  with_columnsX <- filesX[i]$country <- c("X")
  with_columnsX <- filesX[i]$dayofYear <- c(120:175)
  }
  
  #same thing but for directory 2
  filesY <- list.files(directory2, pattern = ".csv")
  #open each file
  for (i in filesY){
    read.csv(i, header = TRUE, stringsAsFactors = FALSE)
    #add columns "country" and "dayofYear"
    with_columnsY <- filesY[i]$country <- c("Y")
    with_columnsY <- filesY[i]$dayofYear <- c(120:175)
  }
  
  # append/combine the new files
  allDataX <- lapply(with_columnsX, function (x) read.csv(file=x, header=TRUE))
  Reduce(function(x,y) merge(x,y), allDataX)
  
  allDataY <- lapply(with_columnsY, function (x) read.csv(file=x, header=TRUE))
  Reduce(function(x,y) merge(x,y), allDataY)
  
  allData <- append(allDataX, allDataY, after = length(allDataX))
  
  # handle NA's [argument]
  print("Data may contain NA's")
  answer <-readline("If you would like to remove NA's reply REMOVE. If you would like to keep NA's reply KEEP")
  if (answer!="REMOVE") { 
    na.omit(allData)
  } else if (answer!="KEEP") {
    answer <- readline("Print warning if NA's are found? Y or N")
    if (answer!="Y"){
      (is.na(allData)=TRUE)
      print("Warning: missing values")
    }
  } else {
    break
  }
}        



# summarize compiled data in terms of number of screens run, percent of infected 
  # patients screened, male vs. female patients, and age distribution

seeData <- function(allData) {
  #number of screens run: line count on allData
  allData <-read.csv("allData.csv", header = TRUE, stringsAsFactors = TRUE)
  num <- nrow(allData)
  mes <- "Number of screens run:"
  paste0(mes, num, sep = " ")
  
  # percent of infected patients screened 
  matches <- grep(pattern = "1", allData)
  numMatches <- length(matches)
  percent <- (numMatches/num)*100
  paste("Percent of infected patients screened:", percent, sep = " ")
  
  # female vs male data
  fem <- grep(pattern = "female", allData)
  numfem <- length(fem)
  male <- grep(pattern = "male", allData)
  nummal <- length(male)
  paste("Female to Male test subjects", numfem, nummal, sep = " : ")
  
  # scatter plot for age distribution
  library(ggplot2)
  ggplot(data = allData, aes(x = age)) +
    geom_histogram(binwidth = 5) +
    xlim(0,100) +
    theme_classic()
}


# Plots for answers to the questions in the analysis
infectedX <- length(grep(pattern = "1", allDataX))
infectedY <-length(grep(pattern = "1", allDataX))
subData <- data.frame(country=c("X", "Y"),
                      infected=c(infectedX, infectedY))


Q1 <- ggplot(data = subData, aes( x = country, y=infected)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("People infected")


plot1 <- ggplot(data = allData, aes( x = country, y=marker01)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot2 <- ggplot(data = allData, aes( x = country, y=marker02)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot3 <- ggplot(data = allData, aes( x = country, y=marker03)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot4 <- ggplot(data = allData, aes( x = country, y=marker04)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot5 <- ggplot(data = allData, aes( x = country, y=marker05)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot6 <- ggplot(data = allData, aes( x = country, y=marker06)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot7 <- ggplot(data = allData, aes( x = country, y=marker07)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot8 <- ggplot(data = allData, aes( x = country, y=marker08)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot9 <- ggplot(data = allData, aes( x = country, y=marker09)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")
plot10 <- ggplot(data = allData, aes( x = country, y=marker10)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("Marker Present")

Q2 <- plot_grid(plot1, plot2, plot3, plot4, plot5,
                labels = c("01", "02", "03", "04","05","06","07","08","09","10"),
                rel_widths = c(1, 1),
                ncol = 5,
                nrow = 2)

