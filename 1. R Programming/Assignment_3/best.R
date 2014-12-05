best <- function(state, outcome) {
    
    ## Read outcome data
    data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    data = subset(data, select = c(2, 7, 11, 17, 23))
    if (!state %in% unique(data$State)) {
        stop("invalid state", call. = T)
    }
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% outcomes) {
        stop("invalid outcome", call.= T)
    }
    setnames(data, 3:5, outcomes)
    data = data[data$State == state & data[, outcome] != "Not Available", ]
    
    ## Return hospital name in that state with lowest 30-day death rate
    result = data[which.min(data[, outcome]),1]
}