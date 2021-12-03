#Write a function to summarize the compiled data set in terms of number of screens run, 
#percent of patients screened that were infected, male vs. female patients, and the age distribution of patients.
#Alldata.csv is the compiled file

install.packages("ggplot2")

library(ggplot2)

setwd("~/Desktop/Rproject2021-master")

numscreens<-read.table("allData.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

colnames(numscreens)

# bar plot showing counts of screens conducted
ggplot(numscreens, aes(x = gender)) +
  geom_bar() +
  theme_bw() +
  xlab("numberofscreens") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))