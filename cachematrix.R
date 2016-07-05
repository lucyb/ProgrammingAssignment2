## Caching the inverse of a matrix
## These are a pair of functions that calculate and then cache the inverse
## of a matrix.

## Create a matrix object that is able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # The inverse matrix
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the matrix, given by x. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the inverse is retrieved from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
