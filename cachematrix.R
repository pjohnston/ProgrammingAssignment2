# Matrix inversion can be an expensive calculation. This pair of functions
# creates a special matrix function which is capable of caching it's inverse 

## makeCacheMatrix is a function that creates a special "matrix".
#
## This is really a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}
        
## 
## cacheSolve computes the inverse of a "matrix" x created with makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache, otherwise,
## the inverse is calculated, cached and then returned
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        # If we have a cached inverse value, return it
        if(!is.null(i)) {
                message("getting cached inverse data")
                return(i)
        }
        # get the matrix
        data <- x$get()
        # calculate it's inverse using the "solve" function
        i <- solve(data, ...)
        # cache the calculated inverse
        x$setinverse(i)
        # return the inverse
        i
}
