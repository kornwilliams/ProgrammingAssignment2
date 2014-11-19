## This is the second programming assignment
## for R Programming course on Coursera.
## This file contains 2 functions:
## 1. 
## makeCacheMatrix() creates a special object that stores a matrix and caches its inverse
## along with serveral getter and setter functions for the matrix and the cached inverse
## 2.
## cacheSolve() returns the inverse the special matrix created by makeCacheMatrix()
## if the inverse is cached, cacheSolve() just returns it
## if not, cacheSolve() calculates the inverse, cache it and then return it.
## Note:
## For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix takes in a matrix, and returns a special matrix based on the submitted matrix,
## the special matrix has a cache for the inverse of the submitted matrix,
## and a list of getter and setter functions for the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## cache for the inverse
    cachedInverse <- NULL
    
    ## setter of the matrix
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    ## getter of the matrix
    get <- function() x
    
    ## setter of the inverse
    setInverse <- function(inverseToBeSet) cachedInverse <<- inverseToBeSet
    
    ## getter of the inverse
    getInverse <- function() cachedInverse
    
    ## list of the setters and getters
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes in the special matrix created by makeCacheMatrix()
## and returns the inverse the matrix. 
## If the inverse is cached, cacheSolve just returns the cached result.
## If not, cacheSolve calculates the inverse matrix, cache it and then return it.
cacheSolve <- function(x, ...) {
    ## check the cached data
    returnedInverse <- x$getInverse()
    if (!is.null(returnedInverse)) {
        ## the inverse is cached before, just return it
        message("getting cached inverse matrix")
        return(returnedInverse)
    }
    
    ## returnedInverse == NULL
    ## the inverse is not cached before
    ## calculate it, set it and return it
    data <- x$get()
    returnedInverse <- solve(data)
    x$setInverse(returnedInverse)
    returnedInverse
}
