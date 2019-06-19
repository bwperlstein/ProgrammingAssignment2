complete <- function(directory = "", id = 1:332) {
        if(directory != "") {
                setwd(directory)                
        }
        i <- 1
        nobs <- vector(mode = "integer", length = 332)
        observer <- vector(mode = "integer", length = 332)
        output <- matrix(NA, 332, 2)
        colnames(output) <- c("observer", "nobs")
        complete <- matrix(NA, 332, 2)
        colnames(complete) <- c("observer", "nobs")
        data <- data.frame()
        while(i <= length(id)) {
                charid <- as.character(id[i], length = 3)
                a <- as.integer(id[i] / 10)
                b <- as.integer(id[i] / 100)
                if(a < 1) {
                        charid <- paste("00", charid, sep = "")
                } else if(a >= 1 & b < 1) {
                        charid <- paste("0", charid, sep = "")
                }
                data <- read.csv(paste(charid, ".csv", sep = ""), header = TRUE)
                nobs[i] <- sum(complete.cases(data$sulfate, data$nitrate))
                observer[i] <- id[i]
                i <- i + 1
        }
        output[1:length(observer),"observer"] <- observer[1:length(observer)]
        output$nobs <- nobs[1:length(nobs)]
        complete <- output
}