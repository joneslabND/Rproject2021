###Step (3) - Write a function to summarize the compiled data set in terms of number of screens run, 
#percent of patients screened that were infected, male vs. female patients, and the age distribution of patients
setwd("~/Desktop/R Studio Projects/Rproject2021/")
#establish function and load data from combined file
library(ggplot2)
library(cowplot)

summary <- function(file){
  data <- read.csv("allData.csv", header=TRUE, sep= ",", stringsAsFactors=FALSE)
  
  ##Number of Screens
  #Number of Screens Total = number of rows (i.e. number of people tested)
  total_screens <- nrow(data)
  cat("The number of total screens run: ", total_screens, "\n")
  #Number of screens for each country
  Xfiles <- subset(data, country == "X")
  countryX_screens <- nrow(Xfiles)
  cat("The number of screens in Country X: ", countryX_screens, "\n")
  Yfiles <- subset(data, country == "Y")
  countryY_screens <- nrow(Yfiles)
  cat("The number of screens in Country Y: ", countryY_screens, "\n")
  # compile data for screens for Total, Country X, and Country Y
  Location <- c("Total Screens", "Country X", "Country Y")
  Screens <- c(total_screens, countryX_screens, countryY_screens)
  Screens_df <- data.frame(Location, Screens)
  
  ##Percentage of Infected Patients
  #Look at each row individually. If infected, individual will have one or more marker as a 1.
  #Percent of patients screened that were infected and not infected
  infected <- nrow(data[data$marker01 == 1 | data$marker02==1 | data$marker03==1 |
                          data$marker04==1 | data$marker05==1 | data$marker06==1 |
                          data$marker07==1 | data$marker08==1 | data$marker09==1 |
                          data$marker10==1, ])
  percent_infected <- (infected/total_screens)*100
  percent_infected <- round(percent_infected, digits=2)
  cat("The percent infected: ", percent_infected, "%\n")
  percent_not_infected <- (1-(infected/total_screens))*100
  percent_not_infected <- round(percent_not_infected, digits=2)
  cat("The percent not infected: ", percent_not_infected, "%\n")
  
  ##Male v. Female Patients
  total_patients <- nrow(data)
  male <- nrow(data[data$gender == "male",])
  percent_male <- (male/total_patients)*100
  percent_male <- round(percent_male, digits=2)
  cat("Percentage of male patients: ", percent_male, "%\n")
  female <- nrow(data[data$gender == "female",])
  percent_female <- (female/total_patients)*100
  percent_female <- round(percent_female, digits=2)
  cat("Percentage of female patients: ", percent_female, "%\n")
  
  ##Age Distribution
  age_data <- data
  age_data$group <- cut(age_data$age, breaks = c(0,10,20,30,40,50,60,70,80,400),
                        labels = c("0+","10+","20+","30+","40+","50+","60+","70+","80+"),
                        right=T)
  ##Determine Marker Count
  countryX_data <- data[data$country == "X", ]
  countryY_data <- data[data$country == "Y", ]
  # Count each marker per country
  markers <- data.frame(marker = seq(1,10), 
                        totalX = colSums(countryX_data[, 3:12] != 0),
                        totalY = colSums(countryY_data[, 3:12] != 0))
  markers$marker <- as.factor(markers$marker)
  
  ##Plot 1 : Number of Screens
  plot_1 <- ggplot(data = Screens_df, aes(x=Location, y=Screens))+
    geom_col(aes(fill=Location))+
    ggtitle("Number of Screens Run per Country")+
    theme(legend.position = "none")
  
  ##Plot 2: Infected
  slices <- c(percent_infected, percent_not_infected)
  lbls <- c("Infected", "Not Infected")
  pct <- round(slices)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ads % to labels
  pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Infected v. Not Infected")
  
  ##Plot 3: Gender
  slices <- c(percent_male, percent_female)
  lbls <- c("Male", "Female")
  pct <- round(slices)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ads % to labels
  pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Male v. Female Patients")
  
  ##Plot 4: Age Distribution
  plot_4 <- ggplot(age_data, aes(x=group, fill = gender)) +
    geom_bar(position = "dodge") +
    ggtitle("Age Distribution") +
    xlab("Age Group") +
    ylab("Number of People")
  
  ##Plot 5: Country X Marker
  plot_5 <- ggplot(markers, aes(x = marker, y = totalX, fill = marker)) + 
    geom_bar(stat = "identity") +
    ggtitle("Country X Marker Count") +
    theme(legend.position = "none")+
    geom_col(color = "black")+
    xlab("Bacteria Marker") + 
    ylab("Total Number")
  
  ##Plot 6: Country Y Marker
  plot_6 <- ggplot(markers, aes(x = marker, y = totalY, fill = marker)) + 
    geom_bar(stat = "identity") +
    ggtitle("Country Y Marker Count") +
    theme(legend.position = "none")+
    geom_col(color = "black")+
    ylim(c(0,7000))+
    xlab("Bacteria Marker") + 
    ylab("Total Number")

  
  # print results
  #fig<-plot_grid(x_marker_graph, y_marker_graph, labels = c('A', 'B'))
  # print(fig)
  
  fig<-plot_grid(plot_1, plot_4, plot_5, plot_6, labels = c('A', 'B', 'C', 'D'))
  print(fig)
}
summary("allData.csv")