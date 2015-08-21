## makeCacheMatrix and cacheSolve are functions for computing the inverse of a
## matrix and storing the matrix inverse in the cache.
##
## To run:
## 1.  Define a square matrix 'a'
##        a <- matrix(1:4,2,2)
## 2.  Define object 'b' that calls 'makeCacheMatrix' with matrix 'a' as input
##        b <- makeCacheMatrix(a)
## 3.  Call 'cacheSolve' with the object 'b' as input.
##        a_inverse <- cacheSolve(b)
## On the first execution, the program computes and returns the inverse of
## the matrix 'x', and stores the inverse in the cache.  On subsequent
## executions, the cached value of the matrix inverse is returned.


## makeCacheMatrix function creates an object that gets and caches the 
## values of the input matrix 'x' and the inverse of 'x'.  The inverse is 
## computed using the R 'solve' function.
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     ## 'set' function changes the matrix 'x' and sets value of 'm' to NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     ## 'get' function returns the matrix 'x' 
     get <- function() x
     ## 'setinverse' function uses the R 'solve' function to calculate 
     ## the matrix inverse and sets 'm' to the value of the matrix inverse   
     setinverse <- function(solve) {
          m <<- solve
     }
     ## 'getinverse' function returns 'm' (either NULL or the inverse of matrix 'x')
     getinverse <- function() m
     
     ## The functions 'set', 'get', 'setinverse' and 'getinverse' are stored in a list 
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve function returns the inverse of a matrix x
cacheSolve <- function(x, ...) {
     ## set m to the value returned by 'getinverse' in the 'makeCacheMatrix' function
     m <- x$getinverse()
     ## if the inverse has been previously calculated, the cached inverse matrix is returned
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     ## if the inverse has NOT been previously calculated, the 'get'
     ## function is called to return the value of the matrix, the inverse is 
     ## calculated using the R 'solve' function, and 'setinverse' is called to 
     ## cache the value of the inverse.  The inverse of the matrix is returned.
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
