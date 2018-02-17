## These two functions will help in caching an inverse matrix and retrieving it.
## Caching and retrieving an inverse matrix is useful because inversing a matrix
## is a computationally expensive operation.

## This function caches the original matrix and creates a list of containing functions
## that will be used to set and get inverse of the matrix as well as the original.
## makeCacheMatrix function must be called at least once for each distinct matrix
## before cacheSolve can be used for that matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }	  

	  set(x)

        get <- function() x
        setInverse <- function(inverseparam) i <<- inverseparam
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function first tries to get the inverse of a matrix from cache. If the inverse is
## found in the cache, it returns the inverse. Otherwise, it will create the inverse
## and sets it in the cache using setInverse function that was created by calling makeCacheMatrix
## function. makeCacheMatrix function must be called at least once for each distinct matrix
## before cacheSolve can be used for that matrix.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
