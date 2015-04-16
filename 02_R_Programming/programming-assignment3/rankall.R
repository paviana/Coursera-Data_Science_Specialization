data <- read.csv('data/outcome-of-care-measures.csv', comment.char='', 
                 colClasses='character')[c(2,7,11,17,23)]
colnames(data) <- c('name', 'state', 'heart.attack', 'heart.failure', 'pneumonia')
data[3:5] <- suppressWarnings(sapply(data[3:5], as.numeric))

rankall <- function(outcome, num = 'best') {
    outcome <- make.names(outcome)
    if (!(outcome %in% colnames(data))) stop('invalid outcome')
    
    states <- split(data, data$state)
    f <- function(df) {
        ord <- order(df[outcome], df['name'], na.last=NA)
        df <- df[ord, 'name']      
        
        name <- NULL
        len <- length(df) 
        if (num == 'best') name <- df[1]
        else if (num == 'worst') name <- df[len]
        else if (num > 0 && num <= len) name <- df[num]
        else name <- NA
        name
    }
    
    hosps <- lapply(states, f)
    data.frame(hospital=unlist(hosps), state=names(hosps))
}
