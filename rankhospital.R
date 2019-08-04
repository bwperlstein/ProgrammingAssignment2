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

addRank <- function(frame) {
        newcolptr <- ncol(frame) + 1
        for(i in 1:nrow(frame)) {
                frame[i, newcolptr] <- i
                colnames(frame) [newcolptr] <- "Rank"
        }
        return(frame)
}

rankhospital <- function(state, outcome, num = "best") {
        ## 1. Read outcome data
        ## 2. Check that state, outcome and num are valid
        ## 3. Return hospital name, state, outcome and rank for outcome
 
        pointer <- getOutcomePtr(outcome)
        FileName <- "outcome-of-care-measures.csv"
        if(!state %in% state.abb) {
                stop(gettext("invalid state"))
        } else if(!outcome %in% names(pointer)) {
                stop(gettext("invalid outcome"))
        }
        if(!num %in% c("best", "worst")) {
                num <- as.integer(num)
                if(is.na(num)) {
                        stop(gettext("invalid rank"))
                }
        }
        outcome.data <- fileRead(FileName)
        outcome.state <- subset(outcome.data, outcome.data$State == state)
        outcome.ptr <- getOutcomePtr(outcome)
        colnames(outcome.state) [outcome.ptr] <- "Rate"
        outcome.state[, outcome.ptr] <- as.numeric(outcome.state[, outcome.ptr])
        outcome.state <- outcome.state[order(outcome.state[, outcome.ptr], outcome.state[, 2]), ]
        outcome.complete <- complete.cases(outcome.state[, outcome.ptr])
        outcome.final <- outcome.state[outcome.complete, ]
        outcome.final <- addRank(outcome.final)
        lastcol <- ncol(outcome.final)
        if(num == "best") {
                outcome.final[1, c(2, 7, outcome.ptr, lastcol)]
        } else if(num == "worst") {
                outcome.final[nrow(outcome.final), c(2, 7, outcome.ptr, lastcol)]
        } else {
                outcome.final[num, c(2, 7, outcome.ptr, lastcol)]
        }
}