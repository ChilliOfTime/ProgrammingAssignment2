
############################################################
## Coursera Data Science Specialization
## R Programming 
## Week 3 Peer-Graded Assignment
############################################################

## Put comments here that give an overall description of what your
## functions do

## The following function does:
## Creates a special "matrix" object that can cache its inverse.

makeCacheMatric <- function(x = matrix()) {    ## define the function with an input of a matrix as default
    invMatrix <- NULL                          ## The invMatrix variable holds the value of the inverse of the matrix, set to NULL
    set <- function(y) {                       ## define the "set" function to assign new 
        x <<- y                                ## value of matrix in parent environment
        invMatrix <<- NULL                     ## if there is a new matrix, reset invMatrix to NULL
    }
    get <- function() x                        ## define the get functio - returns value of the matrix and inverse of the matrix
    setinv <- function(inverse) invMatrix <<- inverse  ## assigns value of invMatrix in parent environment
    getinv <- function() invMatrix                     ## gets the value of invMatrix where the function is called
    list(set = set,                            ## return a list that contains matrix and invMatrix
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function does:
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getinv()              ## return the inverse of matrix value
    if (!is.null(invMatrix)) {           ## if the invMatrix already exists,
        message("Getting cached data")   ## get the cached data.
        return(invMatrix)
    }
    data <- x$get()                      ## if the invMatrix does not exist,
    invMatrix <- solve(data, ...)        ## calculate the inverse of matrix using the "solve" function
    x$setinv(invMatrix)                  
    invMatrix                            ## Return the value
}

## -------------------Testing the Program------------------- ##

## 2 * 2 matrix
    m <- matrix(rnorm(4), 2,2)
    m
    m1 <- makeCacheMatric(m)
    m2 <- cacheSolve(m1)
    m2
    m %*% m2

## 4 * 4 matrix
    m <- matrix(rnorm(16), 4,4)
    m
    m1 <- makeCacheMatric(m)
    m2 <- cacheSolve(m1)
    m2
    m %*% m2

