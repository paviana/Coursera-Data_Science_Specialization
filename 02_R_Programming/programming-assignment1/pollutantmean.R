## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {
    if (is.null(id) || length(id) == 0) id <- 1:332
    files <- list.files(directory, full.names = TRUE)
    data <- data.frame()
    for (i in id) 
        data <- rbind(data, read.csv(files[i]))
    data <- data[, pollutant] 
    m <- mean(data, na.rm=TRUE)
    m
}