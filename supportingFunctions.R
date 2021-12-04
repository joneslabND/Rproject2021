
## define a function to convert all .txt monitoring files in Y country into comma-separated .csv files and save with the name dataY
txt2csv <- function(dir){
  #put all the txt files into filesY
  filesY <- list.files(path =dir, pattern = "txt", full.names = TRUE)
  #use for loop to change the file type and file name
  for (f in filesY){
    #read data
    dataY <- read.table(file=f,header=TRUE,sep=" ",stringsAsFactors=FALSE)
    #replace ".txt" in the name of f with ".csv"
    fout <- gsub('.txt','.csv',f)
    # get the monitor date number of each f file
    print(substr(f,nchar(f)-6,nchar(f)-4))
    #save every file in csv type without the row names
    write.csv(x = dataY, file = fout,row.names = FALSE)
  }
}
##running txt2csv to change file name and file type of the files in the directory of country Y
#txt2csv('countryY')



##set a function to upload and combine all data of Y and X country into a new table and add two columns of country_name and
##monitoring date of each people and its corresponding content into new generate countryX.csv and countryY.csv, give parameter
##of warningNA and removeNA for users to choose give warning and remove or not
##NA files or not if there is any NA present in the table
all2one <-  function(dir,country_name,warningNA=TRUE,removeNA=TRUE){
  #set start file number to be 0
  i=0
  #get the list of the files
  files <- list.files(path =dir, pattern = "csv", full.names = TRUE)
  
  #use for loop to upload the all csv file into one table 
  for (f in files){
    #read the data
    data <- read.table(file=f,header=TRUE,sep=",",stringsAsFactors=FALSE)
    ##if the file contained NA, give a warning, remove the NA and use other number for calculating
    if (is.na(data)){
      #input TRUE to give a warning 
      if (warningNA==TRUE){
        warning("file contains NA")
      }
      #input TRUE to remove the row which contains NA
      if (removeNA==TRUE){
        data <- na.omit(data)
      }
    }
    #for the first csv files
    if(i==0){
      #create a table and put every corresponding country name and monitor date into this table
      sub_table <- data.frame(Country=character(0),dayofYear=integer(0))
      
      for (j in 1:nrow(data)){
        
         sub_table <- rbind(sub_table,c(country_name,as.integer(substr(f,nchar(f)-6,nchar(f)-4))))
      }
      #name the two column in the sub_table
      colnames(sub_table) <- c("Country", "dayofYear")
      
      #combine original table and sub_table into a new table
      table_all <- cbind(data,sub_table)
    }
    #for the second and later csv files, repeat the adding column and corresponding content procure like the 1st file above
    #add every new file into table_all by it's order
    else{
      sub_table <- data.frame(Country=character(0),dayofYear=integer(0))
      
      for (j in 1:nrow(data)){
        sub_table <- rbind(sub_table,c(country_name,as.integer(substr(f,nchar(f)-6,nchar(f)-4))))
      }
      colnames(sub_table) <- c("Country", "dayofYear")
      table_next <- cbind(data,sub_table)
      table_all <- rbind(table_all,table_next)
    }
    i <- i+1
  }
  #save the final table_all in csv type
  write.csv(x = table_all, file = paste('Country',country_name,'.csv',sep = ""),row.names = FALSE,append = T)
}
##running all2one function to get the final table of country Y and country X
# all2one('countryY','Y')
# all2one('countryX','X')

##set a function to combine the data of country X and Y into a table and save as csv file
combine2csv <- function(file1,file2,fileout){
  data1 <- read.table(file=file1,header=TRUE,sep=",",stringsAsFactors=FALSE)
  data2 <- read.table(file=file2,header=TRUE,sep=",",stringsAsFactors=FALSE)
  table_all <- rbind(data1,data2)
  write.csv(x = table_all, file = fileout, row.names = FALSE,append = T)
}
##running the function combined2csv to get the final table
#combine2csv('countryX.csv','countryY.csv','alldata.csv')




##set a function to get daily patient number and percent in two country
patient_day_figure <- function(){
  library(gridExtra)
  library(ggplot2)
  #read the data
  dataScreen <- read.csv(file="alldata.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
  #get the first and last monitor dates
  minday <- min(dataScreen$dayofYear)
  maxday <- max(dataScreen$dayofYear)

  #set a table to get monitoring people number, patients number and patients percent in two countries separately  
  table1 <- data.frame(dayofYear=integer(0),people_X=integer(0),patients_X=integer(0),
                       percent_of_patients_X=numeric(0),people_Y=integer(0),
                       patients_Y=integer(0),percent_of_patients_Y=numeric(0))
  for (i in minday:maxday){
    table1 <- rbind(table1,c(i,0,0,0,0,0,0))
    
  } 
  #name the column name
  colnames(table1) <- c("dayofYear","people_X","patients_X","percent_of_patients_X",
                        "people_Y","patients_Y","percent_of_patients_Y")
  
  #use for loop to upload daily monitoring people and patient number of two country into table1
  for (i in 1:nrow(dataScreen)){
    totalmarker <- 0  
    dayofYear <- dataScreen$dayofYear[i]
    country <- dataScreen$Country[i]
    rowintable1 <- dayofYear-minday+1  #row number of each monitoring day in table1
    for (j in 3:12){
      #calculate the the sum of labeled marker number
      totalmarker <- totalmarker+dataScreen[i,j]
    }
    
    if (country=='X'){
      #calculate monitoring people number in X country
      table1$people_X[rowintable1] <- table1$people_X[rowintable1]+1
      #calculate monitoring patient number in X country
      if (totalmarker>0){
        table1$patients_X[rowintable1] <- table1$patients_X[rowintable1]+1
      }
      #calculate monitoring people and patient number in Y country
      }else{
      table1$people_Y[rowintable1] <- table1$people_Y[rowintable1]+1
      
      if (totalmarker>0){
        table1$patients_Y[rowintable1] <- table1$patients_Y[rowintable1]+1
      }
    }
  }
  #use for loop to calculate daily percent of patients in two country separately 
  for (i in 1:nrow(table1)){
    table1$percent_of_patients_X[i] <- table1$patients_X[i]/table1$people_X[i]
    table1$percent_of_patients_Y[i] <- table1$patients_Y[i]/table1$people_Y[i]
  }
  
  print(table1)
  
  #depict the line graph showing the change of daily patient number and percent
  #use blue color to show the result of X country, red to show Y country
  plot_patients_number <- ggplot(data = table1) +
    geom_line(aes(x = dayofYear, y = patients_X),color="blue") +
    geom_line(aes(x = dayofYear, y = patients_Y),color="red") +
    xlab("date")+
    ylab("patient number")+
    ggtitle("daily patient number in X and Y country")+
    theme_classic()
  plot_percent_of_patients <- ggplot(data = table1) +
    geom_line(aes(x = dayofYear, y = percent_of_patients_X),color="blue") +
    geom_line(aes(x = dayofYear, y = percent_of_patients_Y),color="red") +
    xlab("date")+
    ylab("patient number")+
    ggtitle("daily percent of patient in X and Y country")+
    theme_classic()
  # showing the line graph
  grid.arrange(plot_patients_number,plot_percent_of_patients, ncol=2, heights=c(1,1))
}
##running the function to get daily patient number and percent in two countries
#patient_day_figure()



##set a function to get daily female patient number and percent in X, Y country and two countries 
##and present these female patient percent in line and bar chart
gender_day_figure <- function(){
    library(gridExtra)
    library(ggplot2)
    #read the data and get the first and last monitoring dates
    dataScreen <- read.csv(file="alldata.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
    minday <- min(dataScreen$dayofYear)
    maxday <- max(dataScreen$dayofYear)
    
    #set a table to get gender number and percent of female patients in two country separately 
    table2 <- data.frame(dayofYear=integer(0),patients_X=integer(0),female_patients_X=integer(0),
                         percent_of_Fpatients_X=numeric(0),patients_Y=integer(0),
                         female_patients_Y=integer(0),percent_of_Fpatients_Y=numeric(0),
                         percent_of_Fpatients=numeric(0))
    for (i in minday:maxday){
      table2 <- rbind(table2,c(i,0,0,0,0,0,0,0))
      
    } 
    
    #name the column name
    colnames(table2) <- c("dayofYear","patients_X","female_patients_X","percent_of_Fpatients_X",
                          "patients_Y","female_patients_Y","percent_of_Fpatients_Y",
                          "percent_of_Fpatients")
  
    #use for loop to upload gender number of patients in two countries into table2
    for (i in 1:nrow(dataScreen)){
      totalmaker <- 0
      dayofYear <- dataScreen$dayofYear[i]
      country <- dataScreen$Country[i]
      gender <- dataScreen$gender[i]
      rowintable2 <- dayofYear-minday+1#row number of each monitoring day in table1
      for (j in 3:12){
        #calculate the the sum of labeled marker number
        totalmaker <- totalmaker+dataScreen[i,j]
      }
      
      #if sum of marker bigger than 1, judge the country and gender, calculate the female patient
      #number in X and Y country
      if (totalmaker>0){
        if (country=='X'){
          table2$patients_X[rowintable2] <- table2$patients_X[rowintable2]+1
          if(gender=='female'){
            table2$female_patients_X[rowintable2] <- table2$female_patients_X[rowintable2]+1
          }
        }
        }else{
        table2$patients_Y[rowintable2] <- table2$patients_Y[rowintable2]+1
          if(gender=='female'){
            table2$female_patients_Y[rowintable2] <- table2$female_patients_Y[rowintable2]+1
          }
        }
      }
    
    #use for loop th get percent of female patient in X, Y and two countries
    for (i in 1:nrow(table2)){
      #calculate percent of female patients in X country, Y country and two countries
      table2$percent_of_Fpatients_X[i] <- table2$female_patients_X[i]/table2$patients_X[i]
      
      table2$percent_of_Fpatients_Y[i] <- table2$female_patients_Y[i]/table2$patients_Y[i]
      
      table2$percent_of_Fpatients[i] <- (table2$female_patients_X[i]+table2$female_patients_Y[i])/
        (table2$patients_X[i]+table2$patients_Y[i])
    }
    
    print(table2)
    
    #depict line graph to show the female patients in X country, Y country and two countries
    plot_percent_of_female_patients_X <- ggplot(data = table2,aes(x = dayofYear, y = percent_of_Fpatients_X)) +
      geom_line() +
      ggtitle("percent of female patient in X country")+
      theme_classic()

    plot_percent_of_female_patients_Y <- ggplot(data = table2,aes(x = dayofYear, y = percent_of_Fpatients_Y)) +
      geom_line() +
      ggtitle("percent of female patient in Y country")+
      theme_classic()

    plot_percent_of_female_patients <- ggplot(data = table2,aes(x = dayofYear, y = percent_of_Fpatients)) +
      geom_line()  +
      ggtitle("percent of female patient in two countries")+
      theme_classic()
    
    
    #set a table to get female and male patients number in two country separately 
    table2_2 <- data.frame(gender=character(0),countryX=numeric(0),countryY=numeric(0),two_country=numeric(0))
    #use for loop to upload female and male number in country X, country Y and both two countries
    for (i in 1:2){
      table2_2 <- rbind(table2_2,c(0,0,0,0))
    }
    
    table2_2[1,1] <- 'female'
    table2_2[2,1] <- 'male'

    table2_2[1,2] <- sum(table2$female_patients_X)
    table2_2[2,2] <- sum(table2$patients_X)-sum(table2$female_patients_X)
    
    table2_2[1,3] <- sum(table2$female_patients_Y)
    table2_2[2,3] <- sum(table2$patients_Y)-sum(table2$female_patients_Y)
    
    table2_2[1,4] <- sum(table2$female_patients_X)+sum(table2$female_patients_Y)
    table2_2[2,4] <- sum(table2$patients_X)+sum(table2$patients_Y)-sum(table2$female_patients_X)-sum(table2$female_patients_Y)
    
    #name the column name of table2_2
    colnames(table2_2) <- c("gender","countryX","countryY","two_country")
    
    print(table2_2)
    
    #build a new black theme
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
    #depict the bar graph to show the composition of male vs female in country X, country Y and both two countries
    #use blue to present the male, and use light blue to present female
    pie_X<- ggplot(data = table2_2, aes(x="", y=countryX, fill=gender))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      ggtitle("Distribution of gender in X country")+
      scale_fill_brewer("gender") + blank_theme
    pie_Y<- ggplot(data = table2_2, aes(x="", y=countryY, fill=gender))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      ggtitle("Distribution of gender in Y country")+
      scale_fill_brewer("gender") + blank_theme
    pie_2<- ggplot(data = table2_2, aes(x="", y=two_country, fill=gender))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      ggtitle("Distribution of gender in two countries")+
      scale_fill_brewer("gender") + blank_theme
    
    #showing the line and bar graph
    grid.arrange(plot_percent_of_female_patients_X,plot_percent_of_female_patients_Y, plot_percent_of_female_patients,
                 pie_X,pie_Y,pie_2,ncol=3)
  }
##running the function to get daily daily female patient number and percent in X, Y country and two countries 
#gender_day_figure()

  
##set a function to get 1)the number of of all age patients and present every age distribution to the disease in country X, 
##country Y, both the two countries in line chart; 2）the number of every ten years age patients and present the age period
##distribution to the disease in country X, country Y, the two countries in line and chart graph
age_figure <- function(){
    library(gridExtra)
    library(ggplot2)
  #read the data  
  dataScreen <- read.csv(file="alldata.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
    #get the age of oldest patient
    maxage <- max(dataScreen[,2])

    #set a table to record patient age in X, Y country and two countries
    table3 <- data.frame(age=integer(0),countryX=integer(0),countryY=integer(0),two_country=integer(0))
    for (i in 1:maxage){
      table3 <- rbind(table3,c(i,0,0,0))
    }
    colnames(table3) <- c("age","countryX","countryY","two_country")
    #calculate patients number of every age in X, Y country and two countries
    for (j in 1:nrow(dataScreen)){
      totalmaker <- 0
      
      for (k in 3:12){
        totalmaker <- totalmaker+dataScreen[j,k]
      }
        if (totalmaker>0){
          if ((dataScreen[j,13])=='X'){
            table3[(dataScreen[j,2]),2] <-  table3[(dataScreen[j,2]),2]+1
          }else{
            table3[(dataScreen[j,2]),3] <-  table3[(dataScreen[j,2]),3]+1
          }
        }
        table3[(dataScreen[j,2]),4] <- table3[(dataScreen[j,2]),2]+table3[(dataScreen[j,2]),3]
    }
    
    print(table3)
    
    #depict line graph to show every age's distribution to the disease in X,Y country and two countries
    plot_age_X <- ggplot(data = table3,aes(x=age,y=countryX)) +
      geom_bar(stat='identity',fill="black") +
      xlab("age") +
      ylab('number of patients')+
      ggtitle("Distribution of age in X country")+
      theme_classic()
    plot_age_y <- ggplot(data = table3,aes(x=age,y=countryY)) +
      geom_bar(stat='identity',fill="black") +
      xlab("age") +
      ylab('number of patients')+
      ggtitle("Distribution of age in Y country")+
      theme_classic()
    plot_age_two_country <- ggplot(data = table3,aes(x=age,y=two_country)) +
      geom_bar(stat='identity',fill="black") +
      xlab("age") +
      ylab('number of patients')+
      ggtitle("Distribution of age in two country")+
      theme_classic()
    
    #set a table to record patient number of every 10 years old in X,Y country and two countries
    #there are 6 group of people, 0-9 years old, 10-19 years old, 21-29 years old, 31-39 years old, 
    #41-49 years old, 50 and above 50 years old
    table3_2 <- data.frame(age=character(0),countryX=integer(0),countryY=integer(0),two_country=integer(0))
    
    #add every age period into different rows in first column
    for (i in 1:6){
      table3_2 <- rbind(table3_2,c(i,0,0,0))
    }
    
    for (i in 1:5){
      table3_2[i,1] <- paste(as.character((i-1)*10),'-',as.character(i*10-1),sep = "")
    }
    table3_2[6,1] <- '50+'
    
    #add column's name
    colnames(table3_2) <- c("age","countryX","countryY","two_country")

    #calculate patients number of every age period and save in table3_2
    for (i in 1:nrow(table3)){
      if (i>=50){
        agerange <- 6
      }
      else{
        agerange <- (i %/% 10) +1
      }

      table3_2[agerange,2] <- table3_2[agerange,2]+ table3[i,2]
      table3_2[agerange,3] <- table3_2[agerange,3]+ table3[i,3]
      table3_2[agerange,4] <- table3_2[agerange,4]+ table3[i,4]
    }
    
    print(table3_2)
   
    #depict line graph to show every age periods' distribution to the disease in X,Y country and two countries
    plot_age_range_X <- ggplot(data = table3_2,aes(x=age,y=countryX)) +
      geom_bar(stat='identity',fill="black") +
      xlab("age") +
      ylab('number of patients')+
      ggtitle("Distribution of age in X country")+
      theme_classic()
    plot_age_range_Y <- ggplot(data = table3_2,aes(x=age,y=countryY)) +
      geom_bar(stat='identity',fill="black") +
      xlab("age") +
      ylab('number of patients')+
      ggtitle("Distribution of age in Y country")+
      theme_classic()
    plot_age_range_two_country <- ggplot(data = table3_2,aes(x=age,y=two_country)) +
      geom_bar(stat='identity',fill="black") +
      xlab("age") +
      ylab('number of patients')+
      ggtitle("Distribution of age in two country")+
      theme_classic()
    
    #build a new black theme
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
    #depict bar graph to show every age periods' distribution to the disease in X,Y country and two countries
    pie_X<- ggplot(data = table3_2, aes(x="", y=countryX, fill=age))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      ggtitle("Distribution of age in X country")+
      scale_fill_brewer("age") + blank_theme
    
    pie_Y<- ggplot(data = table3_2, aes(x="", y=countryY, fill=age))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      ggtitle("Distribution of age in Y country")+
      scale_fill_brewer("age") + blank_theme
    
    pie_2<- ggplot(data = table3_2, aes(x="", y=two_country, fill=age))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start=0)+
      ggtitle("Distribution of age in two countries")+
      scale_fill_brewer("age") + blank_theme

    #showing the line and bar graph
    grid.arrange(plot_age_X,plot_age_y,plot_age_two_country,
                 plot_age_range_X,plot_age_range_Y,plot_age_range_two_country,
                 pie_X,pie_Y,pie_2,
                 ncol=3,nrow=3)
}
##running the function to get the distribution of age to X, Y country and two countries
#age_figure()
  
  
  
##set the function to get the number of patients labeled with every marker of the ten disease related markers in X and Y country  
marker_figure <- function(){
    #read the data
    dataScreen <- read.csv(file="alldata.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
    #set a table to record the number of patients labeled with each marker and first appearance dates of each marker in X and Y country  
    table4 <- data.frame(dayofYear=integer(0),countryX=integer(0),countryY=integer(0),
                         first_day_X=integer(0),first_day_Y=integer(0))
    for (i in 1:10){
      table4 <- rbind(table4,c(i,0,0,0,0))
    } 
    #add column's name
    colnames(table4) <- c("marker","countryX","countryY",'first_day_X','first_day_Y')
    #calculate the number of patients labeled with each marker and first appearance dates of each marker
    for (i in 1:nrow(dataScreen)){
      totalmarker <- 0
      country <- dataScreen$Country[i]
      dayofYear <- dataScreen$dayofYear[i]
      for (j in 3:12){
        if (dataScreen[i,j]==1){
          if (country=='X'){
            table4$countryX[j-2] <- table4$countryX[j-2]+1
            if (table4$first_day_X[j-2]==0){
              table4$first_day_X[j-2] <- dayofYear
            }
          }
          else{
            table4$countryY[j-2] <- table4$countryY[j-2]+1
            if (table4$first_day_Y[j-2]==0){
              table4$first_day_Y[j-2] <- dayofYear
            }
          }
        }
      }
    }
    print(table4)
    
    #get RColorBrewer and choose color set to fill bar graph
    library(RColorBrewer)
    my.cols <-  brewer.pal(10,"Set3")

    #depict bar graph to show the distribution of markers in X and Y country
    plot_marker_X <- ggplot(data = table4,aes(x=marker,y=countryX)) +
      geom_bar(stat='identity',fill=my.cols) +
      xlab("Marker") +
      ylab("number of patient")+
      #label the scale of x axis 
      scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"))+
      ggtitle("Distribution of markers in X country")+
      theme_classic()

    plot_marker_Y <- ggplot(data = table4,aes(x=marker,y=countryY)) +
      geom_bar(stat='identity',fill=my.cols) +
      xlab("Marker") +
      ylab("number of patient")+
      #label the scale of x axis 
      scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"))+
      ggtitle("Distribution of markers in Y country")+
      theme_classic()
    
    #showing the bar graph
    grid.arrange(plot_marker_X,plot_marker_Y,ncol=2)
}
##running the function to get the number of patients labeled with every marker to X, Y country and two countries
#marker_figure()

