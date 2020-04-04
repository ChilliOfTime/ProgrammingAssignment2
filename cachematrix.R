
############################################################
## Coursera Data Science Specialization
## R Programming 
## Week 3 Peer-Graded Assignment
############################################################

## Put comments here that give an overall description of what your
## functions do

## The following function does:
## Creates a special "matrix" object that can cache its inverse.

makeCacheMatric <- function(x = matrix()) {    
    invMatrix <- NULL                       
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invMatrix <<- inverse
    getinv <- function() invMatrix
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function does:
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getinv()
    if (!is.null(invMatrix)) {
        message("Getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setinv(invMatrix)
    invMatrix
}
