plot2 <- function(){
        #load the data
        electricdata <- read.table("household_power_consumption.txt" , header = TRUE , sep = ";" , stringsAsFactors = FALSE)
        electricdata <- mutate(electricdata , newdates = as.Date(electricdata$Date , "%d/%m/%Y") )
        
        
        
        #extract the 2 dates
        smalldata <- filter(electricdata , newdates == "2007-02-01" | newdates =="2007-02-02")
        
        #make a new column for date and time combined
        smalldata <- mutate(smalldata , dateNtime = paste(newdates ,Time , sep = " ") )
        
        
        #Now pull up and empty chart with labels and all.
        with(smalldata , plot( as.POSIXct(dateNtime)  , 
                               Global_active_power , 
                               type="n" , 
                               xlab=" " ,
                               ylab = "Global Active Power (kilowatts)") 
             )
        
        #Fill the plot with lines
        lines(as.POSIXct(smalldata$dateNtime) , smalldata$Global_active_power)
        
        dev.copy(png , "plot2.png")
        dev.off()
}