## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "matrix" object,
## which is really a list containing 4 "methods"" to
##
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse
##
## A "method" is really a function closure that "knows" to which matrix it belongs.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
