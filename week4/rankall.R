rankall <- function(outcome, num = "best")
{
    ## Read outcome data
    csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (outcome == "heart attack")
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    else if (outcome == "heart failure")
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    else if (outcome == "pneumonia")
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    else
        stop("invalid outcome")
    
    csv <- csv[, c("Hospital.Name", "State", colName)] # filter the columns we want
    csv[, colName] <- as.numeric(csv[, colName]) # convert to numeric
    csv <- split(csv, csv$State) # split by state
    
    rank <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE) # create a new variable to put the hospital for each state
    
    for (state in names(csv))
    {
        csv[[state]] <- csv[[state]][order(csv[[state]][,3], csv[[state]][,1], na.last = NA), ] # Sort the min values and remove NAs
        stateHospitals <- csv[[state]]
        
        if (num == "best")
            hospitalName <- stateHospitals[1, "Hospital.Name"]
        else if (num == "worst")
            hospitalName <- tail(stateHospitals$Hospital.Name, n=1)
        else if (num >= 1 & num <= length(stateHospitals$Hospital.Name))
            hospitalName <- stateHospitals[num, "Hospital.Name"]
        else
            hospitalName <- NA
        
        rank[nrow(rank) + 1, ] <- c(hospitalName, state)
    }
    
    return(rank)
}