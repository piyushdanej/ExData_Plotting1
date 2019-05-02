plot4 <- function(){
        #load the data
        electricdata <- read.table("household_power_consumption.txt" , header = TRUE , sep = ";" , stringsAsFactors = FALSE)
        electricdata <- mutate(electricdata , newdates = as.Date(electricdata$Date , "%d/%m/%Y") )
        
        
        
        #extract the 2 dates
        smalldata <- filter(electricdata , newdates == "2007-02-01" | newdates =="2007-02-02")
        
        #make a new column for date and time combined
        smalldata <- mutate(smalldata , dateNtime = paste(newdates ,Time , sep = " ") )
        
        #change Sub_meterings to numeric from character
        smalldata <- mutate(smalldata , Sub_metering_1 = as.numeric(Sub_metering_1) ,
                                            Sub_metering_2 = as.numeric(Sub_metering_2) ,
                                            Sub_metering_3 = as.numeric(Sub_metering_3) ,
                                            Global_reactive_power = as.numeric(Global_reactive_power) , 
                                            Global_active_power = as.numeric(Global_active_power) , 
                                                Voltage = as.numeric(Voltage)
                           )
        #set layout for the graphs
        par(mfrow = c(2,2))
        
        #FIrst plot
        with(smalldata , plot(as.POSIXct(dateNtime) , Global_active_power , type="n" , xlab = "" , ylab="Global Active Power") )
        lines(as.POSIXct(smalldata$dateNtime) , smalldata$Global_active_power)
        
        #second plot
        with(smalldata , plot(as.POSIXct(dateNtime) , Voltage  , type="n" , xlab = "datetime"  , ylab= "Voltage") )
        lines(as.POSIXct(smalldata$dateNtime) , smalldata$Voltage )
        
        #Third plot
        with(smalldata , plot( as.POSIXct(dateNtime)  , Sub_metering_1 , type="n" , xlab=" " , ylab = "Energy sub metering") )
        
        #Now throw in the lines.
        lines(x = as.POSIXct(smalldata$dateNtime) , y = smalldata$Sub_metering_1 , col="black")
        lines(x = as.POSIXct(smalldata$dateNtime) , y = smalldata$Sub_metering_2 , col="red")
        lines(x = as.POSIXct(smalldata$dateNtime) , y = smalldata$Sub_metering_3 , col="blue")
        
        #And now the legend
        #Giving extra right spacing on legend since the lables are cut in png.
        legend("topright" ,
               col = c("black" , "red" , "blue") ,
               legend = c("Sub_metering_1               " , "Sub_metering_2             " , "Sub_metering_3             ") ,
               x.intersp = 1.5 , 
               y.intersp = 1.5 , 
               lty = 1 ,
               bty = "n") #bty = "n" specified no boundaries in legend box
        
        #Fourth plot
        with(smalldata , plot(as.POSIXct(dateNtime) , Global_reactive_power , type="n" , xlab="datetime"))
        lines(as.POSIXct(smalldata$dateNtime) , smalldata$Global_reactive_power)
        dev.copy(png , "plot4.png")
        dev.off()
        
}