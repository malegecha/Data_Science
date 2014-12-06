library(data.table)
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data to data.table
    ## Check that state, num, and outcome are valid 
    if (outcome == "heart attack") {
        data = fread("outcome-of-care-measures.csv", select = c(2, 7, 11), 
                     colClasses = "character")
    } else if (outcome == "heart failure") {
        data = fread("outcome-of-care-measures.csv", select = c(2, 7, 17), 
                     colClasses = "character")
    } else if (outcome == "pneumonia"){
        data = fread("outcome-of-care-measures.csv", select = c(2, 7, 23), 
                     colClasses = "character")
    } else {
        stop("invalid outcome", call. = T)
    }
    state_set = unique(data$State)
    if (!state %in% state_set) {
        stop("invalid state", call. = T)
    }
    if (is.numeric(num)) {
        if (!num %% 1 == 0){
            stop("invalid num", call. = T)
        }
        if (length(state_set) < num) {
            return(NA)
        }
    } else if (!num %in% c("best", "worst")) {
        stop("invalid num", call. = T)
    }
    ## rename columns    
    setnames(data, c(1, 3), c("Name", "Outcome"))
    ## subsetting columns
    data = data[data$State == state & data$Outcome != "Not Available", ]
    ## convert character to numeric
    data$Outcome = as.numeric(data$Outcome)
    ## set key to data.table, it will sort data.table automatically
    setkeyv(data, c(Outcome = "Outcome", Name = "Name"))
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best") {
        head(data, 1)$Name
    } else if (num == "worst") {
        tail(data, 1)$Name
    } else {
        data[, Name[num]]
    }
}
