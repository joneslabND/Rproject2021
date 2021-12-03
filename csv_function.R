
##This function takes a space or tab delimited file with a .txt extension and
##creates a .csv file of the same name, replacing the .txt with .csv 

#Usage: call the function cmdlfiles with the directory of files as the input

cmdlfiles<-function(...){
  datalist <- list(...)
  for(directory in datalist){
    setwd(directory)
    x<-list.files(pattern = "*.txt")
 
      for(i in x){
   

        a<-read.table(i, header = TRUE)
        b<-gsub(pattern = "\\.txt$","",i)
        c<-assign(b,paste(b,".csv",sep = ""))
       write.csv(a, file = c, quote = FALSE, row.names = FALSE)
      }
   setwd("..")
  }

}
  

  
  
  
  