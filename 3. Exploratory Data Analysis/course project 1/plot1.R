library("data.table")
plot1 = function () {
    ## read required data
    data = fread("household_power_consumption.txt", sep=';', select = c(3),
                 na.strings="?", skip = 66637, nrows = 2880)
    ## set column name
    setnames(data, c(1), c("Global_active_power"))
    ## plot data
    png(filename = "plot1.png")
    hist(as.numeric(data$Global_active_power), main="Global Active Power", 
         xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")  
    dev.off()
}
