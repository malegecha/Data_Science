complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    all_files = list.files(directory, pattern="*.csv")
    required_files = vapply(all_files[id], 
                            function(elem) paste(directory,elem, sep="/"), 
                            character(1))
    nobs = vapply(required_files, 
                  function(elem) NROW(na.omit(read.csv(elem))), 
                  integer(1))
    result <- data.frame(id,nobs, row.names = NULL)
}