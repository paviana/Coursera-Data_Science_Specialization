MakeVector <- function(x = numeric()) {
    m <- NULL
    Set <- function(y) {
        x <<- y
        m <<- NULL
    }
    Get <- function() x
    SetMean <- function(mean) m <<- mean
    GetMean <- function() m
    invisible(list(set = Set, 
                   get = Get, 
                   set.mean = SetMean, 
                   get.mean = GetMean))
}

CacheMean <- function(x, ...) {
    m <- x$get.mean()
    if (!is.null(m)) {
        message('Getting caching data')
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$set.mean(m)
    m
}