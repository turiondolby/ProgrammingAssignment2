## Caching the Inverse of a Matrix
##    Uses a helper function to make a cache object
##    and another to store and return the inverse
##    in the cache.


## Creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes and returns inverse of special "matrix"
## returned by makeCacheMatrix above.
## If inverse has already been calculated
## (and matrix has not changed), cachesolve
## retrieves inverse from cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
