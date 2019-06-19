corr <- function(directory = "", threshold = 0) {
        id <- 1:332
        i <- 1
        z <- 1
        x <- vector(mode = "integer", length = 0)
        y <- vector(mode = "integer", length = 0)
        correlation <- vector(mode = "numeric", length = 0)
        data <- data.frame()
        if(directory != "") {
                setwd(directory)
        }
        while(i <= length(id)) {
                fi <- paste(as.character(id[i], length = 3), ".csv", sep = "")
                a = as.integer(id[i] / 10)
                b = as.integer(id[i] / 100)              
                if(a < 1) {
                        fi <- paste("00", fi, sep="")
                } else if(b < 1) {
                        fi <- paste("0", fi, sep="")
                }
                data <- read.csv(fi, header = TRUE)
                goodptr <- complete.cases(data$sulfate, data$nitrate)
                goodrows <- sum(goodptr)
                m <- 1
                r <- 1
                x <- NULL
                y <- NULL
                if(goodrows > 0 & goodrows >= threshold) {
                        while(m <= nrow(data)) {
                                if(goodptr[m]) {
                                        x[r] <- data[m, "sulfate"]
                                        y[r] <- data[m, "nitrate"]
                                        r <- r + 1
                                }
                                m <- m + 1
                        }
                        if(i == 275 | i == 276 | i == 277) { browser() }
                        correlation[z] <- cor(x, y)
                        z <- z + 1
                }
                i <- i + 1
        }
        return(correlation)
}