
data <- read.csv('data/outcome-of-care-measures.csv', comment.char='', 
                 colClasses='character')[c(2,7,11,17,23)]
colnames(data) <- c('name', 'state', 'heart.attack', 'heart.failure', 'pneumonia')
data[3:5] <- suppressWarnings(sapply(data[3:5], as.numeric))

rankhospital <- function(state, outcome, num = 'best') {
    df <- data[data$state == state, ]
    if (nrow(df) == 0) stop('invalid state')
    
    outcome <- make.names(outcome)
    if (!(outcome %in% colnames(data))) stop('invalid outcome')
    
    df <- df[order(df[outcome], df['name'], na.last=NA), c('name', outcome)]      
    
    len <- nrow(df) 
    if (num == 'best') return(df[['name']][1])
    else if (num == 'worst') return(df[['name']][len])
    else if (num > 0 && num <= len) return(df[['name']][num])
    else return(NA)
}