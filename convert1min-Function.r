#all the paths are relative to the location of this script
#make a folder named inputfiles and put all the files need to be converted in this folder
#make a folder named outputfiles, which will hold all the converted files
#make sure you have the csv file with the right time sequence, e.g. time sequence nov. 2017
#then click Source, the script will run. You will get the results!
#
# Rongling Li 09.2018

inputpath<- "U:/Data from sensors (Sundmolen)/10.43.3.146/test"
outputpath<- "U:/Data from sensors (Sundmolen)/treated data/test"
timefile<- "time sequence 1 2018.csv"
mypattern<- ".csv$"


## convert inputfiles to time sequence of the timefile.
ConvertToMinuteFile <- function(inputfile, outputfile, timefile) {
  ## (0) read csv file
  df <- read.csv (inputfile)
  ##keep only two columns: timestamp and value
  db <- subset(df, select = c(1,2))
  #check the column of date, delete duplicated datetime, extract unique elements
  db <- db[!duplicated(db[1]),]
 
  ## (1) change date format from m-d-y to y-m-d in order to sort it by date
  db$newdate <- strptime(as.character(db$timestamp),"%m/%d/%Y %H:%M" )
  db$txtdate <- format(db$newdate, "%Y-%m-%d %H:%M")
  newdb <- db[, c("txtdate", "value")]
  
  ## (2) load full time sequence. 
  ts <- read.csv (timefile)
  
  ## (3) using the full time sequence (keytime) to merge db, so the db file contains the full time sequence of keytime
  fulldb <- merge(x=ts,y=newdb,by = "txtdate", all = TRUE)
  keepdb <- subset(fulldb, select = -2) 
  
  ## (4) use the na.locf() function from the zoo package to carry the last observation forward to replace NA values
  library(zoo)
  finaldb<- na.locf(keepdb)
  #fill the first a few NAs, using fromLast=TRUE to carry next observation backward
  nnfinaldb <- na.locf(finaldb, fromLast=TRUE)
  #check the column of date, delete duplicated datetime, extract unique elements
  unidb <- nnfinaldb[!duplicated(nnfinaldb[1]),]
 
  ## (5) output the 1-min interval data as.csv file
  write.csv(unidb, outputfile)
  
}


## get all file names
filelist<- list.files(inputpath, pattern = mypattern)
#Convert file by file 
for(filename in filelist) {
  #use paste function to join part of the original file name with postfix as the name of the output file
  outputfile<- paste(substring(filename,1,nchar(filename)-4), "_regtime.csv", sep = "") 
  #use paste function to define the paths of input file and output file
  ConvertToMinuteFile(paste(inputpath,filename,sep = "/"), paste(outputpath,outputfile,sep = "/"), timefile)
}

