# Analysis Script 

# Set working directory and load data and ggplot library
setwd("~/Desktop/Fall-2021/Biocomputing/Rproject2021/")
allData <- read.table("allData.csv", stringsAsFactors = TRUE, header = TRUE, sep = ",")
source()
library(ggplot2)


# Question 1

# For loop to determine positive infection cases 
positive <- NA 
for(i in 1:nrow(allData)){
  if(sum(allData[i,3:12])>=1){
    positive[(i+1)]<-1
  }else{
    positive[(i+1)]<-0
  }
}

# Vector with value of 1 for infection, value of 0 indicated not infected
# Deletes initial NA value so that length of vector matches number of screens
positive <- positive[2:length(positive)]
  
# Add new column to dataframe with the infection data
allData <- cbind(allData, new_col = positive)
#allData <- allData[ -c(15:17) ]

# Graphs number of positive cases by day of year, splits by country
plot1 <- ggplot(data = allData, aes(x = dayofYear, y = new_col, fill=country))+
  geom_bar(stat='identity')+
  facet_grid(.~country)+
  ylab("Positive Cases")+
  xlab("Day of Year")+
  theme_classic()

print(plot1)
print("Based on this figure, it is likely that the disease outbreak began in country X, and then spread to country Y. During the earliest days of screening, there were a greater number of positive cases for country X than country Y. Positive cases do not show up in country Y until about 20 days after country X. Therefore, the disease started in country X.")


# Question 2

# Install packages and load library
install.packages("reshape")
library(reshape)

# Convert subset of data from wide format to long format so that it can be graphed
data <- allData[,3:13]
data_long <- melt(data, id.vars = c('country'))

# Graphs positive infections for each marker and splits by country
plot2 <- ggplot(data = data_long, aes(x=variable, y=value, fill = variable))+
  geom_bar(stat='identity')+
  facet_grid(.~country)+
  theme_classic()+
  ylab("Number of Patients")+
  xlab("Marker")+
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.position = "none")

print(plot2)
print("If country Y develops a vaccine, it is unlikely to work for county X because the two countries show very different markers. Each marker can cause a patient to be positive for infection, but the difference in marker prevalence indicates mutation within the disease. If country Y developed an effective vaccine, it would most likely target markers 6 and 7 because they are found in the greatest number of patients, which would not be effective for infected patients in country X where markers 1-5 are most prevalent.") 


