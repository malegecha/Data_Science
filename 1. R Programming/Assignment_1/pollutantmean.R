pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    all_files = list.files(directory, pattern="*.csv")
    required_files = vapply(all_files[id], 
                            function(elem) paste(directory,elem, sep="/"), 
                            character(1))
    
    if (pollutant == "sulfate") {
        col = "2"   
    } else {
        col = "3"
    }

    df= data.frame()
    for(i in required_files) {
        temp = na.omit(read.csv(
            pipe(paste("cut -f", col, " -d ',' ",  i, sep=""))))
        df = rbind(values, temp)
    }
    
    if (pollutant == "sulfate"){
        mean(df$sulfate)  
    } else {
        mean(df$nitrate)
    }
    
}