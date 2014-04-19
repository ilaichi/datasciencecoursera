## Matrix inversion is usually a costly computation.
## The functions in this file enable caching the inverse of a matrix 
## rather than computing it repeatedly. They need to be used in
## conjunction with each other.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) minv <<- inv
  
  getinverse <- function() minv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then it 
## retrieves the inverse from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if(!is.null(inv)) {
    message("getting cached data")

  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  }

  inv
}
