rm(list=ls())

apt_no <- "A31"
#df <- read.csv (paste("2018_",i,"_",sensor$code[j],"_data.csv",sep=""))

#load metadata into dataframe from csv file
df <- read.csv (paste(apt, "meta data fil.csv"), sep =";", na.strings=c("", "NA"))
df_clean <- na.omit(df)

# Include only those where the unit is given i.e. we are dealing with a measurement / data point
df_clean$adr <- gsub("/", "_", df_clean$X)


#build dataframe with all sensor (used for file names), units and sensor names
sensor = data.frame("code" = df_clean$adr,
                  "unit" = df_clean$X.3,
                  "name" = df_clean$Energylab.Nordhavnen.KNX.Sundmolen)

#subsets can be selected, e.g. based on unit or address space
sensor[sensor$unit == "degC" ,]
sensor[sensor$code == "" ,]

