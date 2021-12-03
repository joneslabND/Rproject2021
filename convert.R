setwd("~/Downloads/Notre Dame/Biocomputing/Rproject2021")

###STEP (1) - convert .txt to .csv
csv_converter <- function(dir){
  setwd(dir)
  data <- list.files(pattern = ".txt")
  for (i in 1:length(data)){
    DATA=read.table(file=data[i],header=T)
    write.table(DATA,file=sub(".txt",".csv",data[i]),row.names=F,quote=F,sep=",")
  }
  file.remove(data)
}

csv_converter("countryY")
dir <- setwd("~/Downloads/Notre Dame/Biocomputing/Rproject2021/")

