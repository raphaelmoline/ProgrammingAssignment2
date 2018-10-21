## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that has the the matrix, the inverted matrix and associated functions

makeCacheMatrix <- function(x = matrix()) {

    invMat <- matrix() # default value
    invMat <- NULL
    
    set <- function(y = matrix()) { #setter matrix
        x <<- y
        invMat <<- NULL
    }
    
    get <- function() x # getter matrix
    
    setInvMat <- function(z = matrix()) invMat <<- z # setter invMat
    
    getInvMat <- function() invMat # getter invMat
    
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
    
}


## caching the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInvMat()
    
    if (!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    
    data <- x$get()
    invMat <- solve(x$get()) # argument is the matrix
    x$setInvMat(invMat)
    invMat
    
}
