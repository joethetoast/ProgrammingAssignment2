## This file defines functions for creating a matrix-like object which is able
## cache its inverse, and a function that can be used to retreive the 
## cached inverse, or calculate and cache the inverse, for a given matrix.


## The makeCacheMatrix function creates an object which can store a matrix
## along with its cached inverse. It has several methods:
##  - get: returns the matrix
##  - set: sets a new matrix, invalidating any cached inverse result
##  - setInverse: once the inverse of the matrix is calculated, this method can
##      be used to cache it.
##  - getInverse: returns the cached inverse, or NULL if no inverse has been calculated.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## The cacheSolve function takes a matrix-like object x which was created with the
## makeCacheMatrix function. It checks to see if the matrix has a cached inverse
## associated with it. If so, it will return the cached inverse. If not, it will
## calculate the inverse and cache it.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message("Returning cached inverse")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setInverse(i)
    i
}
