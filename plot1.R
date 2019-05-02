library(dplyr)

plot1 <- function(){
        #load the data
        electricdata <- read.table("household_power_consumption.txt" , header = TRUE , sep = ";" , stringsAsFactors = FALSE)
        electricdata <- mutate(electricdata , newdates = as.Date(electricdata$Date , "%d/%m/%Y") )
               
       
        
        #extract the 2 dates
        smalldata <- filter(electricdata , newdates == "2007-02-01" | newdates =="2007-02-02")
        
        smalldata <- mutate(smalldata , Global_active_power = as.numeric(Global_active_power))
        
       
        #creating the plot in normal dev mode
        with(smalldata , hist(Global_active_power ,
                              col = "red" ,
                              xlab = "Global Active Power (kilowatts)" ,
                              ylab = "Frequency" ,
                              main = "Global Active Power"))
        
        #Copy the histogram into png file
        dev.copy(png , file = "plot1.png")
        dev.off()
             
}
