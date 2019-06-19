pollutantmean <- function(dir = "", pollutant, id = 1:332) {
        data <- data.frame()
        df <- vector(mode = "numeric", length = 0)
        dflen <- 0
        if (dir != "") {
                setwd <- dir          
        }
        i <- 1
        while (i <= length(id)) {
                charid <- as.character(id[i], length = 3)
                a <- as.integer(id[i] / 10)
                b <- as.integer(id[i] / 100)
                if(a < 1) {
                        charid <- paste("00", charid, sep = "")
                } else if(b < 1) {
                        charid <- paste("0", charid, sep = "")        
                }
                data <- read.csv(paste(charid, ".csv", sep = ""), header = TRUE)                       
                j <- 1
                k <- length (df)
                while(j <= nrow(data)) {
                        df[k + j] <- data[j,pollutant]
                        j <- j + 1
                }
                i <- i + 1
        }
        a <- !is.na(df)
        pm <- round(mean(df[a]), digits = 3)
        pm
}