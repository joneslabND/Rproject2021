diry = readline(prompt = "enter your directory which holds Country Y's files: ")
dirx = readline(prompt = "enter your directory which holds Country X's files: ")
maindir = readline(prompt = "enter the directory where you want to store the compiled dataset: ")

source("supportingFunctions.R")
txt_to_csv(diry)
Mergefile(diry,dirx,maindir)
Summary(maindir)

newDataset = read.csv("theData.csv")
countx = 0
county = 0
for (i in 1: nrow(newDataset)){
  for (j in 3:12)
    if (newDataset[i,j]==1 && newDataset$Country[i] == "X"){
      countx = countx +1
      break
    }else if (newDataset[i,j]==1 && newDataset$Country[i] == "Y"){
      county = county +1
      break
    }else{
      
    }
  
}

print("In which country (X or Y) did the disease outbreak likely begin? ")

print(paste0("Country X infected patients =  ",countx))
print(paste0("Country y infected patients =  ",county))
difference = countx - county
print(paste0("It's Country X because it has ", difference ," more infected patients than Country Y "))


####If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
## to make more generic, ask user for number of markers and timeframe they would like to see for
## number of infections. This data could be inserted below where "length = 10" for number of markers
## and "n" to specify date range

Markers_x = numeric(length = 10)
Markers_y = numeric(length = 10)

n = 165

for (i in 1: length(Markers_x)){
  for (j in 1:nrow(newDataset))
    if (newDataset[j,i+2]==1 && newDataset$Country[j] == "X"&& newDataset$DayofYear[j] >=n ){
      Markers_x[i] = Markers_x[i] +1
      
    }else if (newDataset[j,i+2]==1 && newDataset$Country[j] == "Y" && newDataset$DayofYear[j] >=n){
      Markers_y[i] = Markers_y[i] +1
      
    }else{
      
    }
  
}
print("For Country X, the number of patients infected recently are (by each Marker 1-10): ")
print(Markers_x)

print("For Country Y, the number of patients infected recently are (by each Marker 1-10): ")
print(Markers_y)

percentY = 0

for (i in 1:length(Markers_y)){
  percentY[i] = (Markers_y[i]/sum(Markers_y)) * 100
  percentY[i] = round(percentY[i], digits=3)
  print(paste0("Marker ",i, " was found in ", percentY[i] ,"% of the number of recenlty infected patients in Country Y"))
}

print("We assume that Country Y will make a vaccine for any markers found in greater than 10% of the infected patients.") 
print("Based on the results above we believe that Country Y's vaccine would not be effective for Country X.") 
print("This is because Country X's infections are more concentrated with markers that will not be used to develop Country Y's vaccine.")
