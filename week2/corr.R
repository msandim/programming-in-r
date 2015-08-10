corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    # Get filenames and build corr return structure:
    files <- list.files(directory)
    corr <- rep(NA, length(files));
    
    for (i in seq_along(files))
    {
        # Load into data frame:
        frame <- read.csv(file.path(directory, files[i]))
        
        # Get the data that is not incomplete:
        frame <- frame[!is.na(frame$sulfate) & !is.na(frame$nitrate), ]
        
        # Only calculate cor if we want this one:
        if (nrow(frame) > threshold)
            corr[i] = cor(frame$sulfate, frame$nitrate)
    }
    
    corr <- corr[!is.na(corr)]
    return(corr)
}

