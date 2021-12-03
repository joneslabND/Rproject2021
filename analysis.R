
source("/Users/niko/Downloads/shell-lesson-data/Rproject2021/supportingFunctions.R")

# Convert txt files to csv files
file2comma(dir = "/Users/niko/Downloads/shell-lesson-data/Rproject2021/countryY")

# Compile all country X and country Y screening files to one all_data file
compile("/Users/niko/Downloads/shell-lesson-data/Rproject2021/countryX",
        "/Users/niko/Downloads/shell-lesson-data/Rproject2021/countryY",
        "/Users/niko/Downloads/shell-lesson-data/Rproject2021/all_data.csv", 1)

#summarize the all_data file with figures and print out the number of all screens
summarizeData("/Users/niko/Downloads/shell-lesson-data/Rproject2021", "all_data.csv")



# 1. In which country (X or Y) did the disease outbreak likely begin?
Country X due to higher number of infected people.

# 2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
Although it is almost impossible to answer this question correctly one would assume that since virus strains would get mutated from country X to Y , vaccine developed at Y might be less effective towards the original variant from X