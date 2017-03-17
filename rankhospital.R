rankhospital <- function(state, outcome, num = "best") {
        options(warn = -1)
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[, c(11, 17, 23)] <- sapply(data[, c(11, 17, 23)], as.numeric)
        
        ## Check that state and outcome are valid
        stateList <- unique(data[, 7])
        if(sum(stateList == state) == 0) {stop("invalid state")}
        
        if(outcome == "heart attack") {colNum <- 11}
        else if(outcome == "heart failure") {colNum <- 17}
        else if(outcome == "pneumonia") {colNum <- 23}
        else {stop("invalid outcome")}
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        fullStateData <- data[data[, 7]==state, ]
        noNAStateData <- fullStateData[!is.na(fullStateData[, colNum]), ]
        sortStateData <- noNAStateData[order(noNAStateData[, colNum], noNAStateData[, 2]), ]

        if(num == "best") {
                nameOutput <- as.character(sortStateData[1, 2]) 
        }       
        else if(num == "worst") {
                nameOutput <- as.character(sortStateData[nrow(sortStateData), 2])
        }
        else if(num <= nrow(sortStateData)) {
                nameOutput <- as.character(sortStateData[num, 2])
        }
        else return(NA)
        nameOutput
}