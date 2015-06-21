## cachSolve(x, ...) returns the inverse of a square matrix using 
## functions defined in makeCacheMatrix(x) to cache its inverse and
## improve performance following the first call. 

## makeCacheMatrix('x') initializes two cached matrices and returns a list 
## containing functions to set and get these cached values:
##  1. set(m) sets cached matrix to 'm', and sets cached inverse matrix to NULL
##  2. get() gets cached matrix to be inverted
##  3. setinverse(i) sets cached inverse matrix to 'i'
##  4. getinverse() gets cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    set <- function(m) {
        cachedi <<- NULL
        cachedm <<- m
    }
    get <- function() cachedm
    setinverse <- function(i) cachedi <<- i
    getinverse <- function() cachedi

    ## initialize cached matrices to avoid surprises
    set(x)
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve(x, ...) on its first call for a particular matrix uses solve to 
## calulate, cache, and return the inverse of the square matrix in cache. On 
## subsequent calls it returns the cached inverse matrix. 

cacheSolve <- function(x, ...) {

    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
