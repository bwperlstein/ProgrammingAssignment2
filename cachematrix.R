## Put comments here that give an overall description of what your
## functions do

## Get cached (built) inverse matrix; build inverse matrix nd cache,
## store in cahe

makeCacheMatrix <- function(y = matrix()) {
        orig_x <<- matrix(data = y, nrow = nrow(y), ncol = ncol(y))
        invert_cols <- nrow(y)
        invert_rows <- ncol(y)
        invert_x <<- matrix(nrow = invert_rows, ncol = invert_cols)
        invert_x <<- NULL
        set <- function(y) {
                orig_x <<- y
                invert_x <<- NULL
        }
        get <- function() {
                return(orig_x)
        }
        setinvert <- function(z) {
                invert <- matrix(nrow = ncol(z), ncol = nrow(z))
                i <- 1
                while (i <= nrow(z)) {
                        invert[1:ncol(z),i] <- z[i,1:ncol(z)]
                        i <- i + 1
                }
                invert_x <<- invert
                orig_x <<- z
                return(invert)
        }
        getinvert <- function() {
                invert_x
                orig_x
                }
        list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## Write a short comment describing this function

## Return cached inverse matrix if available. Otherwise calculate inverse matrix and return.

cacheSolve <- function(t, ...) {
        t$getinvert()                           ## Retrieve cached values for matrices
        a <- t$get()                            ## Retieve current matrix
        if(!is.null(invert_x[1, 1])) {          ## if inverse calculated, do not recalc
                message("Getting cached data")
                return(invert_x)
        }
        invert_x <- t$setinvert(a)              ## A. Create inverse matrix AND B.cache matrices
        return(invert_x)
}