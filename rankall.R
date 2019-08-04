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

makeResultNumeric <- function(x, resultcol) {
        for (i in 1:nrow(x)) {
                x[i, resultcol]<- as.numeric(x[i, resultcol])
                browser()
        }
        return(x)
}

returnRank <- function(x) {
        if (x == "best") {
                y <- 1L
                return(y)
        } else if(x != "worst") {
                y <- as.integer(x)
                if (is.na(y)) {
                        stop(gettext("invalid rank"))
                } else {
                        return(y)
                }
        } else {
                return(NULL)
        }
}

rankall <- function(outcome, num = "best") {
        ## 1. Read outcome data
        ## 2. Check that state, outcome and num are valid
        ## 3. Return hospital name, state, outcome and rank for outcome
 
        pointer <- getOutcomePtr(outcome)
        FileName <- "outcome-of-care-measures.csv"
        if(!outcome %in% names(pointer)) {
                stop(gettext("invalid outcome"))
        }
        outcome.data <- fileRead(FileName)
        outcome.ptr <- getOutcomePtr(outcome)
        colnames(outcome.data) [2] <- "Hospital"
        colnames(outcome.data) [7] <- "State"
        outcome.rank <- returnRank(num)
        browser()
        outcome.data <- transform(outcome.data,
                                  outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack =as.numeric(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                                  outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure =
                        as.numeric(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                                outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia =
                        as.numeric(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
        outcome.data <- outcome.data[order(outcome.data[, 7], outcome.data[, outcome.ptr],
                                           outcome.data[, 2]), ]
        browser()
        outcome.complete <- complete.cases(outcome.data[, outcome.ptr])
        outcome.data <- outcome.data[outcome.complete, ]
        outcome.bystate <- split(outcome.data, outcome.data$State)
        browser()
        outcome.list <- lapply(outcome.bystate, function(x) {
                if(!is.null(outcome.rank)) {
                        x[outcome.rank, c(2, 7)]
                } else {
                        x[nrow(x), c (2,7)]
                }
                })
        outcome.list
}