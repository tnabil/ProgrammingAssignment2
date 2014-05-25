## The functions below are used to calculate the inverse of a matrix and
## cache the outcome so that it is not re-calculated unnecessarily as long as
## the underlying matrix has not changed

## Used to construct a special matrix that can cache its inverse to avoid
## unnecessary re-computation

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(theInverse) inverse <<- theInverse
    
    getInverse <- function() inverse
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the given special matrix
## It will check first if the inverse is already cached and return the cached
## value, otherwise it will calculate it and cache it before returning it

cacheSolve <- function(x, ...) {
    i = x$getInverse()
    
    if (!is.null(i)) {
        message('getting cached data')
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
