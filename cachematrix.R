## This file contains a pair of functions to cache inverses of square matrices.
## It contains the following functions:
##
##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##   cacheSolve: This function computes the inverse of the special "matrix" returned by
##     makeCacheMatrix above. If the inverse has already been calculated (and the matrix
##     has not changed), then cacheSolve retrieves the inverse from the cache.

## The function makeCacheMatrix creates a special "matrix" object,
## which is really a list containing 4 "methods"" to
##
##    set - set the value of the matrix
##    get - get the value of the matrix
##    setinverse - set the value of the inverse
##    getinverse - get the value of the inverse
##
## A "method" is really a function closure that "knows" to which matrix it belongs.

makeCacheMatrix <- function(x = matrix()) {
        # initialize the cache to "empty"
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        # Return a list with the 4 functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve calculates the inverse of a special
## "matrix" object, using a cached value if it is available.
## If not in cache, the calculated value is put in the cache.

cacheSolve <- function(x, ...) {
        # Check if cached value is present
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) # Return cached value
        }
        # If not, calculate inverse, put in cache, and return it.
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
