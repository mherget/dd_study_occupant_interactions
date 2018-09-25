  rm(list=ls())
  dev.off()
  library(zoo)
  library(ggplot2)
  
  
    ### INPUTS
    # Note1: In order for the axes to be displayed properly,the sensors should be ordered by unit in the "sensor" dataframe
    #(for example, avoid to put a temperature signal, then a RH, then another temperature in the "sensor" dataframe) 
    # Note 2: This code can only handle two different units since there can only be two y axes. But it can for example handle 5 temperature
    #signals and 3 power signals on the same graph

  
    ## apt: the code of the apartments you are working with, in case your data is organised in folders by apartment
    ## sensor: a matrix with the code of your sensors (or name if you have changed the file name), unit and name for the graph's legend
    ## start_date and end_date: time period where you want to visualize data
    ## base_path: folder where your apartment folders are (or loose sensor files)
  
  
  
  apt=c("10.43.3.10"
       ,"10.43.3.66"
       ,"10.43.3.42"
       #,"10.43.3.18"
       #,"10.43.3.34"
       #,"10.43.3.58"
        )
  sensor=data.frame(
    "code" = c("3_0_1"
               #,"2_2_1"
               ),
    
    "unit" = c("degC"
               #, "degC"
               ), 
    
    "name" = c("LR temperature (degC)"
               #,"LR temp setpoint (degC)" 
               ))
  
  start_date="2018-06-01 00:00"
  end_date="2018-08-31 00:00"
  base_path="U:/Data from sensors (Sundmolen)"
  
  
  
  #### Convert start and end date into readable formats
    
      new_sd <- strptime(as.character(start_date),"%Y-%m-%d %H:%M")
      new_ed <- strptime(as.character(end_date),"%Y-%m-%d %H:%M")
  
  
    
  #### Create usable weather dataframe
    
      weather_path <- "C:/Users/lucjsar/OneDrive/Documents/DTU-St-Gobain PhD/Nordhavn data analysis - BYG+Elektro/Weather data/ClimateStationData DTU.csv"
    
      weather <- read.csv(weather_path)
    
      ##keep only two columns: time and air_temperature
      out_temp <- subset(weather, select = c(1,12),colClasses=c("POSIXct", "numeric"))
    
      ##transform timestamp into a readable format
      out_temp$timestamp <- strptime(out_temp$timestamp,"%Y-%m-%d %H:%M" )
      
      ##select only the weather data on the chosen time period
      out_temp <- subset(out_temp, timestamp>=new_sd & timestamp<=new_ed)
  
  
  #### Create usable time stamp dataframe
      
      timestamp_path <- "C:/Users/lucjsar/OneDrive/Documents/DTU-St-Gobain PhD/Nordhavn data analysis - BYG+Elektro/time sequence 1-8 2018.csv"
      
      time_data <- read.csv(timestamp_path)
      
      ##transform txtdate into a readable format
      time_data$txtdate <- strptime(time_data$txtdate,"%Y-%m-%d %H:%M" )
      
      ##select only the data on the chosen time period
      time_data <- subset(time_data, txtdate>=new_sd & txtdate<=new_ed)
  
  
  #### Limits for the final plot's axes
  
      ylim=NULL #c(18,32)
      xlim=NULL #c(-5,5)
  
  
  #### This function converts an irregular time series into a time series with 1-minute time step (from Rongling). A time file is needed (see on Drive)  
  ConvertToMinuteFile <- function(df, time_data) {
        ## (0) read csv file
        
        ##keep only two columns: timestamp and value
        db <- subset(df, select = c(1,2))
        #check the column of date, delete duplicated datetime, extract unique elements
        #db <- db[!duplicated(db[1]),]
        
        ## (1) change date format from m-d-y to y-m-d in order to sort it by date
        db$newdate <- strptime(as.character(db$timestamp),"%Y-%m-%d %H:%M%OS" )
        db$txtdate <- format(db$newdate,"%Y-%m-%d %H:%M" )
        newdb <- db[, c("txtdate", "value")]
        
        ## (2) load full time sequence. 
        ts <- time_data
        
        
        ## (3) using the full time sequence (keytime) to merge db, so the db file contains the full time sequence of keytime
        fulldb <- merge(x=ts,y=newdb,by="txtdate", all = TRUE)
        keepdb <- subset(fulldb, select = -2) 
        
        ## (4) use the na.locf() function from the zoo package to carry the last observation forward to replace NA values
        library(zoo)
        finaldb<- na.locf(keepdb)
        #fill the first a few NAs, using fromLast=TRUE to carry next observation backward
        nnfinaldb <- na.locf(finaldb, fromLast=TRUE)
        #check the column of date, delete duplicated datetime, extract unique elements
        unidb <- nnfinaldb[!duplicated(nnfinaldb[1]),]
        
        
        
        return(unidb)
  }  
  
  
  
  #### This function takes the above inputs and plots the probability density function of the given variables on the given time period
  
  Multiple_Plot_density <-function(apt,sensor,start_date,end_date,base_path){
  
    dev.off()
    
    
    windows(height = 20, width = 45)
    
    
     
       
    #loop over the different sensors
    for (j in 1:length(sensor$code)){
      #print(paste("j=",j))
      par(new = T,mar = c(5,5,2,10))
      
      #loop over the different apartments
      for (a in 1:length(apt)){
        
        #print(paste("a=",a))
        
        
        #this line is because I have organised the files by apartment like on OwnCloud. If all files to be read are 
        #directly in base_path, then replace by:
        #setwd(base_path)
        setwd(paste(base_path,"/", apt[a], sep=""))
        getwd() 
        
        #create an empty data frame where we will store the data for the given sensor in the chosen time period
        db <- data.frame(timestamp=as.Date(character()),
                         value=character(), 
                         stringsAsFactors=FALSE) 
        
        first_value <- data.frame(
          "timestamp" = new_sd, 
          "value" = NA
          )
        
        last_value <- data.frame(
          "timestamp" = new_sd, 
          "value" = NA
          )
        
        
        
        #loop over the months we are interested in
        for (i in 1:8){
          #print(paste("month=",i))
          
          month_file_path <- paste("2018_",i,"_",sensor$code[j],"_data.csv",sep="")
          
          if(file.exists(month_file_path)){
            
            df <- read.csv (month_file_path)
            
            ##keep only two columns: timestamp and value
            lastdb <- subset(df, select = c(1,2),colClasses=c("POSIXct", "numeric"))
            
            ##transform timestamp into a readable format
            lastdb$timestamp <- strptime(df$timestamp,"%Y-%m-%d %H:%M:%OS" )
            
            
            ##retrieve the data for initial and last value
            before_start <- subset(lastdb, timestamp<=new_sd )
            after_end <- subset(lastdb, timestamp>=new_ed )
            
            
            ##select only the data on the chosen time period
            lastdb <- subset(lastdb, timestamp>=new_sd & timestamp<=new_ed)
            
            ##add to the data from the previous month
            db <- rbind(db,lastdb)
            
            
            ##retrieve the initial value (it is the last value before the start of the studied time period.
            ##It is mainly useful when studying setpoints, since there can be no data for several days)
            if (nrow(before_start)>0){
              first_value <- data.frame(
                "timestamp" = new_sd, 
                "value" = before_start$value [length(before_start$value)]
              )
            }
            
            
            ##retrieve the last value (the first value after the end of the studied time period).
            ##This will be used only in case there is no data at all in the studied time period and before (it happens with the sensor for heat setpoint display)
            if (nrow(after_end)>0){
              last_value <- data.frame(
                "timestamp" = new_sd, 
                "value" = after_end$value[1]
              )
            }
            
           
          }
          
          # when we arrive at the end of the studied time period, stop the loop if there is data in the studied time period or before
          if (i>=(new_ed$mon +1) && which( !is.na(first_value$value[1]) | nrow(db)>0 )){
            break
            
          }else if (!is.na(last_value$value[1])){
            db <-last_value
            print(paste("Warning: there is no data for the selected period for sensor ", sensor$code[j], " for apartment ",apt[a],", estimate might be inaccurate"))
            break
          }
          
        }  
        # add first value to dataset, in order to be sure not to have an empty dataframe when picking the last value from previous
        db <- rbind(first_value,db)
        
        ##this is the actual last data point in the studied time period
        real_last_value <- data.frame(
          "timestamp" = new_ed, 
          "value" = db$value[length(db$value)])
        
        ## append that last value to the dataset
        db <-rbind(db, real_last_value)

        
        
        
        
        #create a first plot with the first sensor then add the data from the other sensors on top, creating a new axis when unit changes
        #Note: here my first plot is a step plot (setpoints), hence type="s". For regular line plot, type="l"
        
        #block y axis for temperatures to 10 to 30 degrees (can be changed obviously)
        #ylim=if (sensor$unit[j]=="degC"){c(10,30)}  #else{c(0,50)}
        
        
        
        
        if (a==1){
          
          #par(mar = c(5,5,2,10))
          
          
          
          plot(ecdf(db$value),xlab=sensor$name[1], lty=j,col=1,main=""
               #,xlim=c(12,27)
               )
          
          
        } else{
          
          
          lines(ecdf(db$value),col=a,lty=j)
          
        }
        
        
        
      }
      
      
    }
    legend("bottomright",
           legend=paste(rep(apt, each = length(sensor$name)), sensor$name, sep = " - "),
           lty=rep(seq_len(length(sensor$name)), times=length(apt)), col=rep(seq_len(length(apt)), each=length(sensor$name)),
           bg="white")
    
    #Title can be changed
    title(main=paste("Apartment comparison - ", type_graph, " - ",start_date, " to ", end_date))
  }
  
  
  
  #### This function takes the above inputs and returns a time series plot of all the given variables on the given time period
  
  Multiple_Plot_time_series <-function(apt,sensor,start_date,end_date,base_path){
    
    dev.off()
    
    windows(height = 20, width = 45)
    
    
    
    
    #loop over the different sensors
    for (j in 1:length(sensor$code)){
      #print(paste("j=",j))
      par(new = T,mar = c(5,5,2,10))
      
      #loop over the different apartments
      for (a in 1:length(apt)){
        
        #print(paste("a=",a))
        
        
        #this line is because I have organised the files by apartment like on OwnCloud. If all files to be read are 
        #directly in base_path, then replace by:
        #setwd(base_path)
        setwd(paste(base_path,"/", apt[a], sep=""))
        getwd() 
        
        #create an empty data frame where we will store the data for the given sensor in the chosen time period
        db <- data.frame(timestamp=as.Date(character()),
                         value=character(), 
                         stringsAsFactors=FALSE) 
        
        first_value <- data.frame(
          "timestamp" = new_sd, 
          "value" = NA
        )
        
        last_value <- data.frame(
          "timestamp" = new_sd, 
          "value" = NA
        )
        
        
        
        #loop over the months we are interested in
        for (i in 1:8){
          #print(paste("month=",i))
          
          month_file_path <- paste("2018_",i,"_",sensor$code[j],"_data.csv",sep="")
          
          if(file.exists(month_file_path)){
            
            df <- read.csv (month_file_path)
            
            ##keep only two columns: timestamp and value
            lastdb <- subset(df, select = c(1,2),colClasses=c("POSIXct", "numeric"))
            
            ##transform timestamp into a readable format
            lastdb$timestamp <- strptime(df$timestamp,"%Y-%m-%d %H:%M:%OS" )
            
            
            ##retrieve the data for initial and last value
            before_start <- subset(lastdb, timestamp<=new_sd )
            after_end <- subset(lastdb, timestamp>=new_ed )
            
            
            ##select only the data on the chosen time period
            lastdb <- subset(lastdb, timestamp>=new_sd & timestamp<=new_ed)
            
            ##add to the data from the previous month
            db <- rbind(db,lastdb)
            
            
            ##retrieve the initial value (it is the last value before the start of the studied time period.
            ##It is mainly useful when studying setpoints, since there can be no data for several days)
            if (nrow(before_start)>0){
              first_value <- data.frame(
                "timestamp" = new_sd, 
                "value" = before_start$value [length(before_start$value)]
              )
            }
            
            
            ##retrieve the last value (the first value after the end of the studied time period).
            ##This will be used only in case there is no data at all in the studied time period and before (it happens with the sensor for heat setpoint display)
            if (nrow(after_end)>0){
              last_value <- data.frame(
                "timestamp" = new_sd, 
                "value" = after_end$value[1]
              )
            }
            
            
          }
          
          # when we arrive at the end of the studied time period, stop the loop if there is data in the studied time period or before
          if (i>=(new_ed$mon +1) && which( !is.na(first_value$value[1]) | nrow(db)>0 )){
            break
            
          }else if (!is.na(last_value$value[1])){
            db <-last_value
            print(paste("Warning: there is no data for the selected period for sensor ", sensor$code[j], " for apartment ",apt[a],", estimate might be inaccurate"))
            break
          }
          
        }  
        # add first value to dataset, in order to be sure not to have an empty dataframe when picking the last value from previous
        db <- rbind(first_value,db)
        
        ##this is the actual last data point in the studied time period
        real_last_value <- data.frame(
          "timestamp" = new_ed, 
          "value" = db$value[length(db$value)])
        
        ## append that last value to the dataset
        db <-rbind(db, real_last_value)
        
        
        
        
        
        #create a first plot with the first sensor then add the data from the other sensors on top, creating a new axis when unit changes
        #Note: here my first plot is a step plot (setpoints), hence type="s". For regular line plot, type="l"
        
        #block y axis for temperatures to 10 to 30 degrees (can be changed obviously)
        #ylim=if (sensor$unit[j]=="degC"){c(10,30)}  #else{c(0,50)}
        
        if (a==1){
          
          #par(mar = c(5,5,2,10))
          
          
          with(db, plot(db$timestamp, db$value, type="s", ylim=ylim,xaxt="n", xlab="time", ylab=sensor$name[1],lty=j,col=1))
          if (j==1){
            axis.POSIXct(1, db$timestamp, format="%A %m-%d",line=1,lty=0)
            axis.POSIXct(1, db$timestamp, format="%H:%M:%S")}
            
          
          
        } else{
          
          
          lines(db, col = a, type="s",lty=j)
            
          
        }
        
        
        
      }
      
      
      if (j!=1){
        axis(side = 4,line=(j-2)*5 )    #line=(j-2)*5
        mtext(side = 4,  line = (j-2)*5+3, sensor$name[j])  #line = (j-2)*5+3,
      }
      
    }
    legend("bottomright",
           legend=paste(rep(apt, each = length(sensor$name)), sensor$name, sep = " - "),
           lty=rep(seq_len(length(sensor$name)), times=length(apt)), col=rep(seq_len(length(apt)), each=length(sensor$name)),
           bg="white")
    
    #Title can be changed
    title(main=paste("Apartment comparison - ", type_graph, " - ",start_date, " to ", end_date))
  }
  
  
  
  #### This function takes the above inputs and plots them against the outdoor temperature on the given time period
  
  Multiple_Plot_XY_weather <-function(apt,sensor,start_date,end_date,base_path){
    
    
    windows(height = 20, width = 45)
    
    
    
    
    #loop over the different sensors
    for (j in 1:length(sensor$code)){
      #print(paste("j=",j))
      par(new = T,mar = c(5,5,2,10))
      #par(mfrow=c(1,3))
      #loop over the different apartments
      for (a in 1:length(apt)){
        
        #print(paste("a=",a))
        
        
        #this line is because I have organised the files by apartment like on OwnCloud. If all files to be read are 
        #directly in base_path, then replace by:
        #setwd(base_path)
        setwd(paste(base_path,"/", apt[a], sep=""))
        getwd() 
        
        #create an empty data frame where we will store the data for the given sensor in the chosen time period
        db <- data.frame(timestamp=as.Date(character()),
                         value=character(), 
                         stringsAsFactors=FALSE) 
        
        first_value <- data.frame(
          "timestamp" = new_sd, 
          "value" = NA
        )
        
        last_value <- data.frame(
          "timestamp" = new_sd, 
          "value" = NA
        )
        
        
        
        #loop over the months we are interested in
        for (i in 1:8){
          #print(paste("month=",i))
          
          month_file_path <- paste("2018_",i,"_",sensor$code[j],"_data.csv",sep="")
          
          
          if(file.exists(month_file_path)){
            
            
            df <- read.csv (month_file_path)
            
            ##keep only two columns: timestamp and value
            lastdb <- subset(df, select = c(1,2),colClasses=c("POSIXct", "numeric"))
            
            ##transform timestamp into a readable format
            lastdb$timestamp <- strptime(df$timestamp,"%Y-%m-%d %H:%M:%OS" )
            
            
            ##retrieve the data for initial and last value
            before_start <- subset(lastdb, timestamp<=new_sd )
            after_end <- subset(lastdb, timestamp>=new_ed )
            
            
            ##select only the data on the chosen time period
            lastdb <- subset(lastdb, timestamp>=new_sd & timestamp<=new_ed)
            
            ##add to the data from the previous month
            db <- rbind(db,lastdb)
            
            
            ##retrieve the initial value (it is the last value before the start of the studied time period.
            ##It is mainly useful when studying setpoints, since there can be no data for several days)
            if (nrow(before_start)>0){
              first_value <- data.frame(
                "timestamp" = new_sd, 
                "value" = before_start$value [length(before_start$value)]
              )
            }
            
            
            ##retrieve the last value (the first value after the end of the studied time period).
            ##This will be used only in case there is no data at all in the studied time period and before (it happens with the sensor for heat setpoint display)
            if (nrow(after_end)>0){
              last_value <- data.frame(
                "timestamp" = new_sd, 
                "value" = after_end$value[1]
              )
            }
            
            
          }
          
          # when we arrive at the end of the studied time period, stop the loop if there is data in the studied time period or before
          if (i>=(new_ed$mon +1) && which( !is.na(first_value$value[1]) | nrow(db)>0 )){
            break
            
          }else if (!is.na(last_value$value[1])){
            db <-last_value
            print(paste("Warning: there is no data for the selected period for sensor ", sensor$code[j], " for apartment ",apt[a],", estimate might be inaccurate"))
            break
          }
          
        }  
        # add first value to dataset, in order to be sure not to have an empty dataframe when picking the last value from previous
        db <- rbind(first_value,db)
        
        ##this is the actual last data point in the studied time period
        real_last_value <- data.frame(
          "timestamp" = new_ed, 
          "value" = db$value[length(db$value)])
        
        ## append that last value to the dataset
        db <-rbind(db, real_last_value)
        
        
        db <- ConvertToMinuteFile(db,time_data)
        #print(nrow(db))
        
        #create a first plot with the first sensor then add the data from the other sensors on top, creating a new axis when unit changes
        #Note: here my first plot is a step plot (setpoints), hence type="s". For regular line plot, type="l"
        
        #block y axis for temperatures to 10 to 30 degrees (can be changed obviously)
        #ylim=if (sensor$unit[j]=="degC"){c(10,30)}  #else{c(0,50)}
        
        
        if (a==1){
          
          
          
          
          with(db, 
          plot(out_temp$air_temperature, db$value, type="p", ylim=ylim,xlim=xlim, xlab="outdoor temperature (degC)", ylab=sensor$name[1],lty=j,col=a)
          )
          # if (j==1){
          #   axis(1, out_temp$air_temperature, line=1,lty=0)
          #   }
          
          
          
        } else{
           
           
          points(out_temp$air_temperature, db$value, col = a,lty=j)
           
           
        }
        
        
        
      }
      
      
      if (j!=1){
        axis(side = 4,line=(j-2)*5 )    #line=(j-2)*5
        mtext(side = 4,  line = (j-2)*5+3, sensor$name[j])  #line = (j-2)*5+3,
      }
      
    }
    legend("bottomright",
           legend=paste(rep(apt, each = length(sensor$name)), sensor$name, sep = " - "),
           lty=rep(seq_len(length(sensor$name)), times=length(apt)), col=rep(seq_len(length(apt)), each=length(sensor$name)),
           bg="white")
    
    #Title can be changed
    title(main=paste("Apartment comparison - " ,start_date, " to ", end_date))
  }
  
  
  
  
  dev.off()
  Multiple_Plot_XY_weather(apt,sensor,start_date,end_date,base_path)
  
  
  
  
