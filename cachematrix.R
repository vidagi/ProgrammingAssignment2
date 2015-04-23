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
             setinv = setinv,
             getinv = getinv)
}

## instead of solving every time, first checks if there is stored value in the cache
cacheSolve <- function(x, ...) {
        inv <- x$getinv()                     ##first tries to take cashed inversed matrix
        if(!is.null(inv)) {                   ##if it contains smthg 
                message("getting cached data")  
                return(inv)                   ##brings as final inversed matrix
        }                                     ## if not
        data <- x$get()                       ##takes the normal data of the matrix
        inv <- solve(data, ...)               ## solves, e.g. finds the inverse matrix
        x$setinv(inv)                         ## writes to the cashe
        inv                                   ##
        ## Return a matrix that is the inverse of 'x'
}
