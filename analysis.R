# load functions in supportingFunctions.R, compile and process data, and answer
#questions with graphical evidence (Answers are at the bottom!)

# source() to access supportingFunctions.R
source("supportingFunctions.R", local = TRUE)
# use functions to compile and process data
library(ggplot2)
library(cowplot)

toCSV(Rproject2021)
oneCSV(countryX, countryY_csv)
seeData(allData.csv)

# plots: 
Q1
Q2

# Answers:
 # 1. The country with more markers present and thus more people infected most
      # likely had the disease first because it has had more time to progress 
      # and develop than the other country. This is visualized in the plot of 
      # people infected per country (saved as Q1), showing that country X most
      # likely had the disease first.
  # 2. Since the markers present vary widely between the two countries' data, it 
      # can be inferred that the disease has mutated too much for a vaccine in 
      # country Y to be effective in country X. This is visualized by Q2, which 
      # shows how many of each marker are present in each country. 