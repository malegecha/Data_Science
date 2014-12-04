corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    all_files = list.files(directory, pattern="*.csv")
    file_names = vapply(all_files, 
                        function(elem) paste(directory,elem, sep="/"), 
                        character(1))
    result = vector('numeric')
    for(i in file_names) {
        d = na.omit(read.csv(pipe(paste("cut -f2,3 -d ',' ",  i, sep=""))))
        if (nrow(d) > threshold) {
            result = c(result, cor(d$sulfate, d$nitrate))
        }
    }
    result
}