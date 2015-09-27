## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix
## The makeCacheMatrix function: This function creates a special "matrix" object that can cache its inverse.
## Basically it's a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
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

## The cacheSolve function:  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed) it gets the inverse from the cache and skips the rest of computation.
## Otherwise it caculates the inverse of the matrix and chaches it.
##
## Usage Example:
## 
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2,3,3)))
## > cacheSolve(m)
##

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
