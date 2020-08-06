## write a pair of functions, namely, "makeCacheMatrix", and 
##"cacheSolve", that cache the inverse of a matrix. While 
## makeCacheMatrix is a function which creates a special
## matrix object that can catche its inverse for the input
## (a square matrix)
##
  makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
        my_set <- function(y) {
               x <<- y
              inv <<- NULL
           }
        my_get <<- function() x
        my_setinv <- function(inverse)
        inv <<- inverse
        my_getinv <- function() inv
        list(my_set = my_set, my_get = my_get, my_setinv = my_setinv, my_getinv = my_getinv)
  }
  
   ##
     ## cacheSolve is a function which computes the inverse of the
     ## special matrix returned by makeCacheMatrix above. If the
     ## inverse has been calculated, the cacheSolve should retrieve
     ## the inverse from the cache.
     ##
     cacheSolve <- function(x, ...) {
           # retrieve a matrix that is the inverse of x
             inv <- x$my_getinv()
             if(!is.null(inv)) {
                   message("getting the cached data")
                   return(inv)
               }
             data <- x$my_get()
             inv <- solve(data, ...)
             x$my_setinv(inv)
             inv
         } 
     ##
       ## test the above function
       ##
       my_t <- matrix(rnorm(25), 5, 5)
       my_t1 <- makeCacheMatrix(my_t)
       cacheSolve(my_t1)