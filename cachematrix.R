## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##             If the inverse has already been calculated, then the function retrieves the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix
##    takes a square invertible matrix 'x' as input
##    and creates a special "matrix", which is really a list containing a function to
##          1. set the matrix 'x'
##          2. get the matrix 'x'
##          3. set the inverse of 'x'
##          4. get the inverse of 'x'

    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## cacheSolve
##    takes the special "matrix" of 'x' created by makeCacheMatrix() as input
##    and return a matrix that is the inverse of the initial 'x'
    
    m <- x$getinv()     ## attempt to retrieve the inverse matrix eventually cached
    ## if the inverse has already been calculated and cached
    if(!is.null(m)) {
        ## retrieve it from the cache and return it
        message("getting cached inverse matrix")
        return(m)
    }
    ## if not, calculates the inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)         ## and cache the result for potential future use
    m
}