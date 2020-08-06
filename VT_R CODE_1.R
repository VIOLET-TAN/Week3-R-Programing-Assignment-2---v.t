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
[,1]       [,2]       [,3]       [,4]       [,5]
[1,] -0.7538667  1.1117058 -0.2383705  0.2959582  2.0116314
[2,] -1.0564341  0.6132577  0.9493080  0.1041939  1.9966202
[3,] -0.4853578  0.3816935  0.1296111  0.3736950  0.2989598
[4,] -0.9695556  0.5465854  1.0710022 -0.4219785  1.0737054
[5,]  0.3783780 -0.6358701 -1.1891647 -0.1635814 -0.4838225