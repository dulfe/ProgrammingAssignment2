## Generic Inverse Matrix Cache Provider, based on sample given by Instructor
## on Programming Assignment 2

## Data structure (Class???) that allows to cache the inverse of a matrix
## @param Matrix X: Source Matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Inverse Matrix cache
    m <- NULL
    ## Sets the initial values
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Gets the supplied Matrix
    get <- function() x
    ## Stored the Inverse of the Matrix (it does not calculate it)
    setinverse <- function(inverse) m <<- inverse
    ## Gets the Inverse of the Matrix from Cache (NULL if not available)
    getinverse <- function() m
    ## Sets the list of available public functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of cached matrix
## @param makeCacheMatrix X: Cache-able Matrix
## @return Inverse of the Matrix (from cache if possible)
cacheSolve <- function(x, ...) {
    ## Get Inverse Matrix from Cache
    m <- x$getinverse()
    ## If exists, return it from Cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## It does not exists, get the Source Matrix
    data <- x$get()
    ## Calculate the Inverse
    m <- solve(data, ...)
    ## Store Inverse in Cache
    x$setinverse(m)
    ## Return Inverse
    m
}
