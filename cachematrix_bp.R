## Put comments here that give an overall description of what your
## functions do

## Functions neeed to: a Get cached (built) inverse matrix or b. cache the inverse matrix

makeCacheMatrix <- function(y = matrix()) {
        invert_x <<- matrix(nrow = nrow(y), ncol = ncol(y))
        invert_x <<- NULL
        set <- function(y) {                    ## Cache matrix and initialize inverse
                x <<- y
                invert_x <<- NULL
        }
        get <- function() {                     ## Get cached value of matrix
                return(x)
        }
        setinvert <- function(z) {              ## Cache inverse matrix
                invert_x <<- z
                return(invert_x)
        }
        getinvert <- function() {               ## Get cached value of inverse matrix
                invert_x
                }
        list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## Write a short comment describing this function

## Return cached inverse matrix if available. Otherwise calculate inverse matrix and cache.

cacheSolve <- function(t, ...) {
        t$getinvert()                           ## Retrieve cached values for matrices
        a <- t$get()                            ## Retieve current matrix
        if(!is.null(invert_x[1, 1])) {          ## if inverse calculated, do not recalc
                message("Getting cached data")
                return(invert_x)
        }
        b <- solve(a)                           ## Create inverse
        invert_x <- t$setinvert(b)              ## A. Cache original and inverse matrices
        return(invert_x)
}