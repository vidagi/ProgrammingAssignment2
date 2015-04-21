## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             ##while being described, makes the cache empty
        set <- function(y) {                    ##describes the 'set' method
                x <<- y                         ##      asigns the matrix
                inv <<- NULL                    ##      makes the cached inverse empty
        }
        get <- function() x                     ##describes the 'get' method
        setinv <- function(inverse) inv <<- inverse     ##describes 'setinv' method
        getinv <- function() inv                        ##describes 'getinv' method
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## instead of solving every time, first checks if there is stored value in the cache
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
