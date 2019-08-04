chars <- function()
j <- 1
vect <- vector(mode = "integer")
for(i in 1:length(html)) {
        if(i == 10 | i == 20 | i == 30 | i == 100) {
                browser()
                vect[j] <- nchar(html[i])
                j <- j + 1
        }
}
vect