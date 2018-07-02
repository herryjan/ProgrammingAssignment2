## Put comments here that give an overall description of what your
## functions do

## Create an invertible square matrix that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Compute the inverse based off of the 'makeCacheMatrix' function. This function will retrieve the inverse from the cache ASSUMING that the matrix has been solved already.

cacheSolve <- function(x, ...) {
	## Return a matrix the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached result...")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}