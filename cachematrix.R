## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly.

## Assuming the matrix created is invertible, these functions will allow you 
## to create a matrix, cache the inverse, retrieve that information later, and 
## recalculate the inverse should your matrix data have changed.

## First, create a matrix object. You will then cache the following:
## Set and get the values of the matrix.
## Set and get the values of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If the matrix values are unchanged, the following function will retrieve the 
## cached inverse. If the values have changed, the function will recalculate the
## inverse and store it in the cache for future use.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
