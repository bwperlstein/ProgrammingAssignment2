time <- function()
Rprof ( tf <- "log.log",  memory.profiling = TRUE )
DT[,mean(pwgtp15),by=SEX]
Rprof ( NULL ) ; print ( summaryRprof ( tf )  )

Rprof ( tf <- "log.log",  memory.profiling = TRUE )
sapply(split(DT$pwgtp15,DT$SEX),mean)
Rprof ( NULL ) ; print ( summaryRprof ( tf )  )

Rprof ( tf <- "log.log",  memory.profiling = TRUE )
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
Rprof ( NULL ) ; print ( summaryRprof ( tf )  )

Rprof ( tf <- "log.log",  memory.profiling = TRUE )
tapply(DT$pwgtp15,DT$SEX,mean)
Rprof ( NULL ) ; print ( summaryRprof ( tf )  )