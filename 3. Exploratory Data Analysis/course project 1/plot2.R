library("data.table")
plot2 = function() {
    ## read required data
    data = fread("household_power_consumption.txt", sep=';', select = c(1:3),
                 na.strings="?", skip = 66637, nrows = 2880)
    ## set column name
    setnames(data, c(1:3), c("Date","Time","Global_active_power"))
    ## convert data, create new column "POSIXTime"
    data$Date <- as.Date(data$Date, format="%d/%m/%Y") 
    data$POSIXTime = as.POSIXct(paste(data$Date, data$Time))
    # plot data
    png(filename = "plot2.png")
    plot(data$Global_active_power~data$POSIXTime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    dev.off()
}
