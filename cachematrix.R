##########################################
##                                      ##
## Anand Hariharan's submission for     ##
## Programming Assignment 2 of          ##
## R Programming course in Coursera.org ##
##                                      ##
##########################################

## This R source file contains two functions viz.,
##
##   *   makeCacheMatrix
##   *   cacheSolve
##
## They are described as comments just before their implementation.
##
## Test them like so -
##
## > source("ProgrammingAssignment2//cachematrix.R")
##
## > b = matrix(c(2, 1, 3, 2), nrow = 2, ncol = 2)
##
## > b
##      [,1] [,2]
## [1,]    2    3
## [2,]    1    2
##
## > x <- makeCacheMatrix(b)
##
## > x$get()
##      [,1] [,2]
## [1,]    2    3
## [2,]    1    2
##
## > x$set(b)
## Input same as cached. 'set' won't do anything
##
## > cacheSolve(x)
##      [,1] [,2]
## [1,]    2   -3
## [2,]   -1    2
##
## > x$getInverse()
##      [,1] [,2]
## [1,]    2   -3
## [2,]   -1    2
##
## > x$get() %*% x$getInverse()
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## > 
## 

## The function makeCacheMatrix returns a list of functions to -
##
##   *   set the matrix whose inverse is being cached
##   *   get the matrix whose inverse is being cached
##   *   set the matrix's inverse
##   *   get the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {

    cached_inverse <- NULL

    # The implementation of 'set' is slightly different from the example in
    # the web-page in that, it checks to see if the input is the same as the
    # one being cached.  If so, the inverse is retained and not discarded.
    set <- function(y) {

        if ( ! identical(x, y) ) {
            x <<- y
            cached_inverse <<- NULL
        }
        else {
            message("Input same as cached. 'set' won't do anything")
        }
    }

    get <- function() x

    setInverse <- function(inputInverse) cached_inverse <<- inputInverse

    getInverse <- function() cached_inverse

    list( set        = set,
          get        = get,
          setInverse = setInverse,
          getInverse = getInverse )

}


## The function cacheSolve first checks to see if the inverse of the input
## matrix has already been computed and cached.
##
## If so, it gets the inverse from the cache and skips the computation.
##
## Otherwise, it calculates the inverse of the matrix (by calling 'solve') and
## sets the value of the inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {

    inv <- x$getInverse()

    if ( ! is.null(inv) ) {
        message("getting cached inverse")
        return(inv)
    }

    matrix_for_inverse <- x$get()

    inv <- solve(matrix_for_inverse, ...)

    x$setInverse(inv)

    inv
}

