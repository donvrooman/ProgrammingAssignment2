## cachSolve(x, ...) returns the inverse of a square matrix using 
## functions defined in makeCacheMatrix(x) to cache its inverse and
## improve performance following the first call. 

## makeCacheMatrix('x') initializes the cache to NULL and returns a list 
## containing functions to cache the inverse of square matrix 'x':
##  1. set() sets the cached inverse matrix to NULL
##  2. get() gets the value of the matrix to be inverted
##  3. setinverse(inverse) sets the value of cached inverse matrix to inverse
##  4. getinverse() gets the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL
    set <- function() m <<- NULL
    get <- function() x
    setinverse <- function(inverse = NULL) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve(x) on its first call uses solve to calulate, cache, and return the
## inverse of square matrix. On subsequent calls it returns the cached value of 
## the inverse of the matrix. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
