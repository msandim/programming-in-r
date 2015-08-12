rankhospital <- function(state, outcome, num = "best")
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
    
    csv <- csv[csv$State == state, ] # only the rows refering to the state we want
    
    if (nrow(csv) == 0)
        stop("invalid state")
    
    # csv <- csv[csv[, colName] == min(csv[, colName], na.rm = TRUE), ] # Get the min values
    csv <- csv[order(csv[,3], csv[,1], na.last = NA), ] # Sort the min values and remove NAs
    
    if (num == "best")
        csv[1, "Hospital.Name"]
    else if (num == "worst")
        tail(csv$Hospital.Name, n=1)
    else if (num >= 1 & num <= length(csv$Hospital.Name))
        csv[num, "Hospital.Name"]
    else
        NA
}