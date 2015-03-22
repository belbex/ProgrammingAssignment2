makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## checks if the matrix is the same and if the inverse is casched
## if those conditions are true it returns the cached inverse 
## if not it computes the inverse and caches it

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv) && identical(data, x$get()))
    {
        message("Getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
