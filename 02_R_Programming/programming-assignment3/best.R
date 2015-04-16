data <- read.csv('data/outcome-of-care-measures.csv', comment.char='', 
                 colClasses='character')[c(2,7,11,17,23)]
colnames(data) <- c('name', 'state', 'heart.attack', 'heart.failure', 'pneumonia')
data[3:5] <- suppressWarnings(sapply(data[3:5], as.numeric))

best <- function(state, outcome) {
    st <- data[data$state == state, ]
    if (nrow(st)) stop('invalid state')
    
    outcome <- make.names(outcome)
    if (!(outcome %in% colnames(data))) stop('invalid outcome')
    
    ord <- order(st[outcome], st['name'], na.last=NA)
    st <- st[ord, c('name', outcome)]      
    
    st[['name']][1]
}




