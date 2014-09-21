## Compute the inverse of a matirx caching the value of calculations
## Example of using scope assignment operator "<<-"

## Create a special object to store the matrix and the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## If the value had been computed before, the cached inverse is returned
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## check if the inverse was cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## do the calculation
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}