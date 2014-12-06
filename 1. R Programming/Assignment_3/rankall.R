library("data.table")
rankall <- function(outcome, num = "best") {  
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
    if (is.numeric(num)) {
        if (!num %% 1 == 0){
            stop("invalid num", call. = T)
        }
    } else if (!num %in% c("best", "worst")) {
        stop("invalid num", call. = T)
    }
    
    ## rename columns    
    setnames(data, c(1:3), c("hospital", "state", "outcome"))
    ## subsetting columns
    data = data[data$outcome != "Not Available", ]
    ## convert character to numeric
    data$outcome = as.numeric(data$outcome)
    ## set key to data.table, it will sort data.table automatically
    setkeyv(data, c(outcome = "outcome", hospital = "hospital"))
  
    get_best = function(d, s) {
        d = d[d$state == s]
        if (nrow(d) < 1) {
            return(NA)
        } 
        head(d, 1)$hospital
    }
    get_worst = function(d, s) {
        d = d[d$state == s]
        if (nrow(d) < 1) {
            return(NA)
        }
        tail(d, 1)$hospital
    }
    get_rank = function (d, s) {
        d = d[d$state == s]
        if (nrow(d) < num) {
            return(NA)
        }
        d[, hospital[num]]
    } 
    state_set = sort(unique(data$state))
    result = data.table(hospital="1", state=state_set)
    ## the purpose of using three distinct get_XXX functions is avoiding
    ## redundant "if statements" checking for large datasets, 
    ## i.e. only check "num" once
    if (num == "best") {
        for (s in 1:length(state_set)){
            set(result, i= s, j = 1L, value=get_best(data, state_set[s]))
        }
    } else if (num == "worst") {
        for (s in 1:length(state_set)){
            set(result, i= s, j = 1L, value=get_worst(data, state_set[s]))  
        }
    } else {
        for (s in 1:length(state_set)){
            set(result, i= s, j = 1L, value=get_rank(data, state_set[s]))    
        }    
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result   
}

