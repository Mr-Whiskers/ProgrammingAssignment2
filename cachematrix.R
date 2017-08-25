## Put comments here that give an overall description of what your
## functions do

## Returns a list of functions that allow users to retrive or store the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(input) inverse <<- input
  getInverse <- function() inverse
  list(set = set,
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Checks and returns the cached inversed matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
