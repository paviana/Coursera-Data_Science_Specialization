## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) {
    if (threshold < 0) threshold <- 0
    files <- list.files(directory, full.names = TRUE)
    v <- vector(mode='numeric')
    for (f in files) {
        d <- read.csv(f)
        s <- sum(complete.cases(d))
        if (s > threshold) 
            v <- c(v, cor(d$sulfate,d$nitrate, use='pairwise.complete.obs'))
    }
    v
}
