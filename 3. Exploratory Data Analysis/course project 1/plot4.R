library("data.table")
plot4 = function() {
    ## read required data
    data = fread("household_power_consumption.txt", sep=';', skip = 66637,
                na.strings="?",  nrows = 2880)
    ## set column name
    setnames(data, c(1:9), c("Date", "Time", "Global_active_power",
                             "Global_reactive_power", "Voltage", 
                             "Global_intensity","Sub_metering_1",
                             "Sub_metering_2", "Sub_metering_3"))
    ## convert data, create new column "POSIXTime"
    data$Date <- as.Date(data$Date, format="%d/%m/%Y") 
    data$POSIXTime = as.POSIXct(paste(data$Date, data$Time))
    # plot data
    png(filename = "plot4.png", width = 480, height = 480)
    par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
    with(data, {
        plot(Global_active_power~POSIXTime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        plot(Voltage~POSIXTime, type="l", 
             ylab="Voltage (volt)", xlab="")
        plot(Sub_metering_1~POSIXTime, type="l", 
             ylab="Energy sub metering (watt-hour of active energy)", xlab="")
        lines(Sub_metering_2~POSIXTime,col='Red')
        lines(Sub_metering_3~POSIXTime,col='Blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~POSIXTime, type="l", 
             ylab="Global Rective Power (kilowatts)",xlab="")
    })
    
    dev.off()
}
