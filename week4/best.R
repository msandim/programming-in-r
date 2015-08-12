best <- function(state, outcome)
{
    ## Read outcome data
    csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (outcome == "heart attack")
        colName <- "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    else if (outcome == "heart failure")
        colName <- "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    else if (outcome == "pneumonia")
        colName <- "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    else
        stop("invalid outcome")
    
    csv <- csv[, c("Hospital.Name", "State", colName)] # filter the columns we want
    csv[, colName] <- as.numeric(csv[, colName]) # convert to numeric
    
    csv <- csv[csv$State == state, ] # only the rows refering to the state we want
    
    if (nrow(csv) == 0)
        stop("invalid state")

    csv <- csv[csv[, colName] == min(csv[, colName], na.rm = TRUE), ] # Get the min values
    csv <- csv[order(csv[,1], csv[,3]), ] # Sort the min values
    
    return (csv[1,1])
}