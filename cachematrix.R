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
     if (errors == 0)
          message("Everything OK!!")
     else
          warning(sprintf("%d errors detected", errors))
     
}
