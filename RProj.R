mergecsv<-function(...){
  country.final=NULL
  countries <- list(...)
  for (b in countries){
    setwd(b)
    filename=c(list.files(pattern = "*.csv"))
    
      for (i in filename) {
        Xdf<-read.csv(i)
        Xdf[,(ncol(Xdf)+1)]=b
        dayofyear<-gsub(pattern = "[^0-9]", "", i)
        Xdf[,(ncol(Xdf)+1)] = dayofyear
        country.final<-rbind(country.final,Xdf)
      }
      setwd("..")
  }
  names(country.final)[13]<-"country"
  names(country.final)[14]<-"dayofyear"
  if(anyNA(country.final) == TRUE){
    print("NA detected.")
    useranswer<-readline(prompt = "Would you like to:
          1) delete rows with NA 
          2) Include and be warned
          3) Include without warning")
    if(useranswer == "1"){
      country.final<-na.omit(country.final)
    }else if(useranswer == "2"){
      print("NA in data!")
      return(country.final)
    }else if(useranswer == "3"){
      return(country.final)
    }else{
      print("invalid entry")
    }
  }
  return(country.final)
}
y<-mergecsv("countryY")
x<-mergecsv("countryX")
z<-mergecsv("countryX","countryY")