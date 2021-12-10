setwd("C:/Users/Jack Shelley/Desktop/Rproject2021-master/countryY")

countryY <- list.files(pattern = ".txt")

screen_120 <- read.csv2("C:/Users/Jack Shelley/Desktop/Rproject2021-master/countryY/screen_120.txt", header=FALSE, sep="")
screen_120 <- screen_120[2:498,2:13]
colnames(screen_120) = c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10")

screen_120$gender <- gsub("^.{0,2}", "", screen_120$gender)
screen_120$marker10 <- gsub('"', "", screen_120$marker10)
screen_120$marker10 <- as.numeric(screen_120$marker10)


for (i in 2:length(countryY)){
  FILE=read.csv2(file=countryY[i],header=FALSE,sep="")
  FILE <- FILE[2:nrow(FILE),]
  colnames(FILE) = c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10")
  screen_120 <- rbind(screen_120, FILE)
}

write.csv(screen_120, file="C:/Users/Jack Shelley/Desktop/Rproject2021-master/countryY/countryY.csv", sep = ",")

setwd("C:/Users/Jack Shelley/Desktop/Rproject2021-master/countryX")
countryX <- list.files(pattern = ".csv")

countryY_Data = read.csv(file=countryX[1])
countryY_Data = countryY_Data[,2:13]

for (i in 2:length(countryX)){
  FILE2=read.csv(file=countryX[i])
  #FILE2 <- FILE[1:nrow(FILE2),]
  countryY_Data <- rbind(countryY_Data,FILE2)
}
write.csv(countryY_Data,file="countries.csv")
