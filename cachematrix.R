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

## The function cacheSolve calculates the inverse of a special
## "matrix"" object, using a cached value if it is available.
## If not in cache, the calculated value is put in the cache.

cacheSolve <- function(x, ...) {
        # Check if cached value is present
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # If not, calculate inverse and put in cache
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}

## Unit testing function
## This is not really part of the assignment requirements,
## but I wrote it to test that everything is working OK.
## For everything that doesn't work correctly, a warning is printed
## If there are no problems, it prints "Everything OK!!"

unittest <- function() {

     # Matrices used for the test
     mat1 <- matrix(c(2, 0, 0, 2), nrow = 2)
     mat1inv <- solve(mat1)
     mat2 <- matrix(c(1, 2, 3, 4), nrow = 2)
     mat2inv <- solve(mat2)
     errors <- 0

     # Check that makeCacheMatrix works
     mat <- makeCacheMatrix(mat1)
     if (!identical(mat1, mat$get())) {
          warning("Get doesn't return proper value")
          errors <- errors + 1
     }
     mat$set(mat2)
     if (!identical(mat2, mat$get())) {
          warning("Get doesn't return the set value")
          errors <- errors + 1
     }
     mat$setinverse(mat1inv)
     if (!identical(mat1inv, mat$getinverse())) {
          warning("Getinverse doesn't return the setinverse value")
          errors <- errors + 1
     }

     # Check that cacheSolve works
     # First we make a new CacheMatrix object
     mat <- makeCacheMatrix(mat1)
     # Calculate the inverse
     matinv <- cacheSolve(mat)
     # Check it
     if (!identical(matinv, mat1inv)) {
          warning("cacheSolve does not return the inverse")
          errors <- errors + 1
     }
     # Now try it a second time and it should return the same
     matinv <- cacheSolve(mat)
     if (!identical(matinv, mat1inv)) {
          warning("cacheSolve does not return the cached inverse")
          errors <- errors + 1
     }
     # However we don't know if it did the caching properly
     # (Except for a message)
     # So we are now testing the caching by misleading the cache
     mat$setinverse(mat2inv)
     matinv <- cacheSolve(mat)
     if (!identical(matinv, mat2inv)) {
          warning("cacheSolve does not use cached value")
          errors <- errors + 1
     }
     # reset the cache and try again
     mat$setinverse(NULL)
     matinv <- cacheSolve(mat)
     if (!identical(matinv, mat1inv)) {
          warning("cacheSolve does not calculate the inverse")
          errors <- errors + 1
     }
     # Now we try to use set to change the matrix and see if it gives the proper inverse
     mat$set(mat2)
     matinv <- cacheSolve(mat)
     if (!identical(matinv, mat2inv)) {
          warning("cacheSolve does not calculate the inverse after set")
          errors <- errors + 1
     }
     # Try once more to get the cached value
     matinv <- cacheSolve(mat)
     if (!identical(matinv, mat2inv)) {
          warning("cacheSolve does not calculate the inverse after set")
          errors <- errors + 1
     }
     
     if (errors == 0)
          message("Everything OK!!")
     else
          warning(sprintf("%d errors detected", errors))
}
