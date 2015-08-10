x <- read.csv('hw1_data.csv')

# Question 18

ozoneNull <- is.na(x[,'Ozone'])
tempNull <- is.na(x[,'Temp']);

filter <- x[,'Ozone'] > 31 & x[,'Temp'] > 90 & !is.na(x[,'Ozone']) & !is.na(x[,'Ozone'])
filtered <- x[filter,'Solar.R']
meanValue <- mean(filtered, na.rm = TRUE)

# Question 19
filter <- x[,'Month'] == 6
filtered <- x[filter,'Temp']
meanValue2 <- mean(filtered, na.rm = TRUE)

# Question 20
filter <- x[,'Month'] == 5
filtered <- x[filter,'Ozone']
maxValue <- max(filtered, na.rm = TRUE)