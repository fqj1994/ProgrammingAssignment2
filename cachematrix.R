## A cacheMatrix which caches the result of inverse
## recalculate when needed and data changed.

## generated a cacheMatrix from given matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL;
    set <- function(y) {
        x <<- y;
        m <<- NULL;
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get, getinverse=getinverse, setinverse=setinverse)
}


## calculate the inverse of a cacheMatrix with result cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data");
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}


# DEBUG ONLY

# mat = makeCacheMatrix(matrix(rnorm(25), 5, 5))
# print(cacheSolve(mat))
# print(cacheSolve(mat))
# print(cacheSolve(mat))
# mat$set(matrix(rnorm(25), 5, 5))
# print(cacheSolve(mat))
# print(cacheSolve(mat))
