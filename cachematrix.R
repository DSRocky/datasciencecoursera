## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## The first function, creates a list containing a function to
## set the value of the matrix, get the value of the matrix, set the value of the inverse of the matrix and get the value of the inverse of the matrix.
## Assumption: matrix is always invertible.

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


## Write a short comment describing this function
## This function gives you the inverse of a matrix. In case the inverse has been calculated already, it returns the result with a message "getting cached data".
## In case the cache is empty, the function calculates the inverse of the matrix and sets a new value to return.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
