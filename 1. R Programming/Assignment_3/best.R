library(data.table)
best <- function(state, outcome) {  
    ## Read outcome data to data.table
    data = fread("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    ## Subseting columns, remove useless columns
    if (!state %in% unique(data$State)) {
        stop("invalid state", call. = T)
    }
    if (outcome == "heart attack") {
        data = subset(data, select = c(2, 7, 11))
    } else if (outcome == "heart failure") {
        data = subset(data, select = c(2, 7, 17))
    } else if (outcome == "pneumonia"){
        data = subset(data, select = c(2, 7, 23))
    } else {
        stop("invalid outcome", call. = T)
    }
    ## rename columns    
    setnames(data, c(1,3), c("Name", "Outcome"))
    ## subsetting columns
    data = data[data$State == state & data$Outcome != "Not Available", ]
    ## convert character to numeric
    data$Outcome = as.numeric(data$Outcome)
    ## set key to data.table, it will sort data.table automatically
    setkeyv(data, c(Outcome = "Outcome", Name = "Name"))
    ## Return hospital name in that state with lowest 30-day death rate
    head(data, 1)$Name   
}