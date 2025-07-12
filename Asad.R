## Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize the inverse as NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL  # reset inverse when the matrix is changed
    }

    get <- function() x  # return the matrix

    setinverse <- function(inverse) inv <<- inverse  # store the inverse
    getinverse <- function() inv  # retrieve the stored inverse

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function to compute the inverse of the matrix or return it from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)  # return cached inverse
    }

    data <- x$get()         # get the original matrix
    inv <- solve(data, ...) # compute the inverse
    x$setinverse(inv)       # cache the inverse
    inv
}
