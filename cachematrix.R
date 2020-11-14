## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() m
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## Function computes the inverse matrix created by the previously function using the "solve" function as returns the inverse

cacheSolve <- function(m, ...) {
    inv <- m$getInv()
    if(!is.null(inv)) {
        message("getting information saved")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setInv(inv)
    inv
}
