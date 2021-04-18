## This module defines two functions and describes a method to cache data structures to avoid redundand computations
## The first function defines the object that handles data caching and associated get/set functions.
## the second function uses the above object to check if a cached solution exists before calculating the inverse of the matrix. 

# This function creates an object that defines a structure consisting of a matrix and the following associated functions:
## set() sets the data matrix.
## get() returns the data matrix.
## setsolve() sets the inverse of the matrix. Should not be called directly, but only from the cacheSolve() function.
## getsolve() retuns de inverse of the matrix.
# Its purpose is to cache results from potentially time-consuming computatations
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(inv) i <<- inv
        getsolve <- function() i
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# This function calculates the inverse of a matrix, but first checks if a cached value exists.
# Its argument is an object created by the makeCacheMatrix() function.
# Example:
## o <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
## o$get()
## cacheSolve(o)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getsolve()
        if (!is.null(sol)) {
                message("Using cached matrix.")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data)
        x$setsolve(sol)
        sol
}
