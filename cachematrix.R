## matrix <- makeCacheMatrix( matrix(c(5,22,11,10), nrow = 2, ncol = 2) );
## cacheSolve(matrix)

## This function creates a special "matrix" object that can cache its inverse. 
## Contains the following functions:
## * setMatrix      set the value of a matrix
## * getMatrix      get the value of a matrix
## * cacheInverse   get the inverse of the matrix
## * getInverse     get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  getMatrix <- function() x
  cacheInverse <- function(solve) cache <<- solve
  getInverse <- function() cache
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       cacheInverse = cacheInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {

  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("cache value")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  inverse
}

