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
complete <- function(directory, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        comp <- data.frame()
        for (i in id) {
            d <- read.csv(files[i])
            sulfate <- d$sulfate
            nitrate <- d$nitrate
            s <- sum(complete.cases(sulfate, nitrate))
            s <- sum(complete.cases(d))
            comp <- rbind(comp, data.frame(id = i, nobs = s))
        }
        comp
}