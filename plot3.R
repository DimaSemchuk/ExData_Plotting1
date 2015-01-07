
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
makePlot3 <- function(){
  
  hpc <- getHpcDataframe()
  
  if(is.data.frame(hpc)){
    hpc$Global_active_power <- as.numeric(hpc$Global_active_power) /500
    hpc$Sub_metering_2 <- as.numeric(hpc$Sub_metering_2)
    hpc$Sub_metering_2 <- replace(hpc$Sub_metering_2, hpc$Sub_metering_2 == 2, 0)
    hpc$Sub_metering_2 <- replace(hpc$Sub_metering_2, hpc$Sub_metering_2 > 3, 4)
    png(width=480, height=480, file = "plot3.png")
    
    plot(hpc$DateTime,hpc$Sub_metering_1, type = 'l',col="black", xlab = "", ylab = "Energy sub metering")
    lines(hpc$DateTime,hpc$Sub_metering_2, type = 'l',col="red")
    lines(hpc$DateTime,hpc$Sub_metering_3, type = 'l',col="blue")
    legend("topright", pch = "_______", col = c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    dev.off()
    
    print("plot3.png is created")
  }else{
    print("no dataframe to use")
  }
}

makePlot3()
