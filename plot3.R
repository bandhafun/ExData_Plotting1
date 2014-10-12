plot3 <-function (){
  
  #..... LOAD .........
  
  #***** Convert ? to NA  
  con <- file("household_power_consumption.txt", open ="r")
  lines <- readLines(con)
  close(con)
  lines <- gsub("\\?","NA",lines)
  #***** Ger data between 2007-02-01 and 2007-02-02
  newlines <- lines[1]
  for(i in 1: length(lines)){
    if(grepl("^1/2/2007",lines[i]) || grepl("^2/2/2007",lines[i]))
      newlines <- c(newlines , lines[i])
  }
  
  #convert to character in  date format
  newlines <- gsub("^1/2/2007" ,"07/02/01",newlines) 
  newlines <- gsub("^2/2/2007" ,"07/02/02",newlines)
  
  
  #** Write required data into a new file
  con <- file("household_power_consumption1.txt", open ="w")
  writeLines(newlines,con)
  close(con)
  
  #**** Load dataframe & check memory
  data <- read.table("household_power_consumption1.txt", header = TRUE,sep = ";",
                     colClasses = c("character","character","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric"))
  
  # Date and Time corrected
  i <- paste(data$Date,data$Time ,sep=" ")
  data$Time <-  strptime(i,"%y/%m/%d%H:%M:%S")
  data$Date <- as.Date(data$Date,"%y/%m/%d")
    
  # Line plot submetering 1 2 and 3 across date and time
  png(file = "plot3.png") # default is 480 by 480 pixels
  par(mar = c(5.1,4.1,4.1,2.1))
  par(pch=22, col="black")
  plot (data$Time , data$Sub_metering_1, type = "n", xlab = "" , 
        ylab = "Energy sub metering ")
  lines(data$Time , data$Sub_metering_1, type = "l")  
  
 par(pch=22, col="red")
  points(data$Time , data$Sub_metering_2, type = "n")
  lines(data$Time , data$Sub_metering_2, type = "l")
 
 par(pch=22, col="blue")
 points(data$Time , data$Sub_metering_3, type = "n")
 lines(data$Time , data$Sub_metering_3, type = "l")
 legend("topright",pch = c(NA,NA,NA),lty = c(1,1,1), col = c("black","red","blue"),
        legend = c ("Sub_metering_1","Sub_metering_2","Sub_metering_3"),)
 dev.off()
}
  