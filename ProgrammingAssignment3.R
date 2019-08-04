## Read files
fileRead <- function(x) {
        read.csv(x, na.strings = "NA", stringsAsFactors = FALSE, colClasses = "character")
}

getOutcomes <- function() {
        outcomes <- c("heart attack" = 11,"heart failure" = 17, "pneumonia" = 23)
}

getOutcomePtr <- function(x){
        outcomelist <- getOutcomes()
        for(i in 1:length(outcomelist)) {
                if(x == names(outcomelist[i])) {
                        return(outcomelist[i])
                }
        }
        return(NA)
}
getName <- function(x) {
        names <- c("hostpital", "state", "outcome")
        names[x]
}

plotHeartAttack30DayMortality <- function(wd = "") {
        if(wd != "") {
                setwd(wd)
        }
        FileName <- "outcome-of-care-measures.csv"
        outcomes <- fileRead(FileName)
        outcomes[, 11] <- as.integer(outcomes[, 11])
        hist(outcomes[, 11])
}

best <- function(state, outcome) {
        ## 1. Read outcome data
        ## 2. Check that state and outcome are valid
        ## 3. Return hospital name in that state with lowest 30-day death rate
 
        outcome.state <- data.frame()
        FileName <- "outcome-of-care-measures.csv"
        pointer <- getOutcomes()
        if(!state %in% state.abb) {
                stop(gettext("invalid state"))
        } else if(!outcome %in% names(pointer)) {
                stop(gettext("invalid outcome"))
        }
        outcome.data <- fileRead(FileName)
        outcome.state <- subset(outcome.data, outcome.data$State == state)
        outcome.ptr <- getOutcomePtr(outcome)
        outcome.state[, outcome.ptr] <- as.integer(outcome.state[, outcome.ptr])
        outcome.final <- outcome.state[order(outcome.state[, outcome.ptr]), ]
        outcome.final[1, 2]
}