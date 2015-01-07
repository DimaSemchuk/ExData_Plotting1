
## Function that gets required dataframe from txt file in working directory
## Or returns an error message
getHpcDataframe <- function(){
  
  if("household_power_consumption.txt" %in% list.files()){
    
    print("loading household_power_consumption.txt Dataset.......")
    hpc <- read.csv("household_power_consumption.txt", header = TRUE, sep = ";")
    print("household_power_consumption.txt Dataset is loaded")
    
    
    hpc <- hpc[hpc$Date == "1/2/2007" | hpc$Date == "2/2/2007", ]
    print("strip by date == 1/2/2007 and 2/2/2007, data is stripped")
    
    
    print("Adding new column of casted DateTime...")
    hpc$DateTime <- strptime(paste(hpc$Date,hpc$Time), "%d/%m/%Y %H:%M:%S")
    print("DateTime added")
    
    print("Removing date and time columns...")
    hpc$Date <- NULL
    hpc$Time <- NULL
    
    hpc <- hpc[,c(8,1,2,3,4,5,6,7)]
    
    print("returning desired dataframe")
    
    return(hpc)
  }else{
    print("No household_power_consumption.txt file in the working directory")
  }
}



## next function uses getHpcDataframe to load the data and construct the graph
makePlot2 <- function(){
  
  hpc <- getHpcDataframe()
  
  if(is.data.frame(hpc)){
    hpc$Global_active_power <- as.numeric(hpc$Global_active_power) /500
    png(width=480, height=480, file = "plot2.png")
    plot(hpc$DateTime,hpc$Global_active_power, type = 'l', ylab = 'Global Active Power (kilowatts)', xlab = '')
    dev.off()
    
    print("plot2.png is created")
  }else{
    print("no dataframe to use")
  }
}

makePlot2()
