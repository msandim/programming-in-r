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
    
    # create structure and fill header:
    idFreq <- data.frame(rep(NA, length(id)), 2)
    colnames(idFreq) <- c("id", "nobs")
    
    for (i in seq_along(id))
    {
        # Create file path:
        filePath <- file.path("specdata", paste(sprintf("%03d", id[i]), ".csv", sep=""))
        
        # Load into data frame:
        frame <- read.csv(filePath)
        
        # Introduce a new row for this id:
        idFreq[i,] = c(id[i], sum(!is.na(frame$sulfate) & !is.na(frame$nitrate)))
    }
    
    return(idFreq)
}

