library("data.table")
plot3 = function() {
    ## read required data
    data = fread("household_power_consumption.txt", sep=';', skip = 66637,
                 select = c(1:2,7,8,9), na.strings="?",  nrows = 2880)
    ## set column name
    setnames(data, c(1:5), c("Date","Time","Sub_metering_1",
                             "Sub_metering_2", "Sub_metering_3"))
    ## convert data, create new column "POSIXTime"
    data$Date <- as.Date(data$Date, format="%d/%m/%Y") 
    data$POSIXTime = as.POSIXct(paste(data$Date, data$Time))
    # plot data
    png(filename = "plot3.png", width = 480, height = 480)
        plot(data$Sub_metering_1~data$POSIXTime, type="l",
             ylab="Energy sub metering(watt-hour of active energy)", xlab="")
        lines(data$Sub_metering_2~data$POSIXTime,col='Red')
        lines(data$Sub_metering_3~data$POSIXTime,col='Blue')
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    dev.off()
}
