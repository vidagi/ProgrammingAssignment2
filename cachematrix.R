## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             ## while being described, makes the cache empty
        set <- function(y) {                    ## describes the 'set' method
                x <<- y                         ##      asigns the matrix
                inv <<- NULL                    ##      makes the cached inverse empty
        }
        get <- function() x                     ## describes the 'get' method for the part 'x'
        setinv <- function(inv) inv <<- inv     ## describes 'setinv' method for the cached value
        getinv <- function() inv                ## describes 'getinv' method for the cached value
        list(set = set, get = get,              ## makes a list with corresponding names
             setinv = setinv,
             getinv = getinv)
}

## instead of solving every time, first checks if there is stored value in the cache
cacheSolve <- function(x, ...) {
        inv <- x$getinv()                     ## first tries to take the cashed value of the inversed matrix
        if(!is.null(inv)) {                   ## if it contains smthg 
  #             message("getting cached data")  ##announces usage of the cache - has not been specified in the assignment, was used by me for the debugging
                return(inv)                   ## brings as final inversed matrix from cache
        }                                     ## if not
        data <- x$get()                       ## takes the normal data of the matrix
        inv <- solve(data, ...)               ## solves, e.g. finds the inverse matrix
        x$setinv(inv)                         ## writes it to the cache
        inv                                   ## and returns the the calculated inverse matrix
        ## Return a matrix that is the inverse of 'x'
}
