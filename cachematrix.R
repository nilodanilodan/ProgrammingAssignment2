## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    #1. set the matrix
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    #2. get the matrix
    get <- function() m
    #3. set the inverse
    setInv <- function(inverse) inv <<- inverse
    #4. get the inverse
    getInv <- function() inv
    #determine the list of all methods
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## Function computes the inverse matrix created by the previously function using the "solve" function as returns the inverse

cacheSolve <- function(m, ...) {
    inv <- m$getInv()
    if(!is.null(inv)) {
        # if the inverse is already calculated, gets from the cache and skips a new calculation
        message("getting information saved")
        return(inv)
    }
    #else
    data <- m$get()
    #calculates the inverse
    inv <- solve(data, ...)
    m$setInv(inv)
    #return the matrix
    inv
}
