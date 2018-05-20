## This script calculates the inverse of a matrix and cahches the result. If the
##  calculation has already been run, the the value in the cahche will be
##  returned rather than calculating the inverse of the matirx again.
##  Usage:  -Declare the two functions
##          -Call makeCacheMatrix with a matrix as arg and assign to a variable
##          -Call cacheSolve with this variable to create the inverted matrix
##          -Call cacheSolve again and it will return the cached inverted matrix

# makeCacheMatrix creates a matrix object that can cache its inverse. Returns a
#   list of four functions.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolved <- function(solved) s <<- solved
    getSolved <- function() s
    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved)
}

# cahceSolve computes the inverse of the matrix created by makeCacheMatrix. If 
#   the inverse has already been cached then cacheSolve will retrieve the 
#   inverse from the cache. In either case it returns an inverted matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolved()
    # check if the cached matrix already exists. If it does, return it and exit
    # the function
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    # if the cached matrix does not exist, calculate and return it.
    data <- x$get()
    s <- solve(data, ...)
    x$setSolved(s)
    s
}

# # Testing code
# x <- rnorm(16)
# dim(x) <- c(4,4)
# s <- solve(x)
