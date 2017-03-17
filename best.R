## find the best hospital with lowest 30-day mortality for specified
## outcome in specified state

best <- function(state, outcome) {
        options(warn=-1)
        ## read in the outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[, c(11, 17, 23)] <- sapply(data[, c(11, 17, 23)], as.numeric)
        
        ## Check that state and outcome are valid
        stateList <- unique(data[, 7])
        if(sum(stateList == state) == 0) {stop("invalid state")}
        
        if(outcome == "heart attack") {colNum <- 11}
        else if(outcome == "heart failure")  {colNum <- 17}
        else if(outcome == "pneumonia")  {colNum <- 23}
        else {stop("invalid outcome")}
        
        ## Return hospital name in that state with lowest 30-day death rate
        fullStateData <- data[data[, 7] == state, ]
        noNAStateData <- fullStateData[!is.na(fullStateData[, colNum]), ]
        minStateData <- noNAStateData[noNAStateData[, colNum] == min(noNAStateData[, colNum]), ]
        sortMinStateData <- minStateData[order(minStateData[, 2]), ]
        hospitalName <- as.character(sortMinStateData[1,2])
        hospitalName
}