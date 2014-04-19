## Matrix inversion is usually a costly computation.
## The functions in this file enable caching the inverse of a matrix 
## rather than computing it repeatedly. They need to be used in
## conjunction with each other.

# EXAMPLE USAGE
#
# source('cachematrix.R')
# c=rbind(c(1, -1/4), c(-1/4, 1)) # create test matrix
# cm = makeCacheMatrix(c)         # cache matrix for c
# cacheSolve(cm)                  # solve and cache inverse
# cacheSolve(cm)                  # retrieve cached inverse
# cacheSolve(cm) %*% c            # test that inverse is correct, result should be Identiry matrix


## makeCacheMatrix
#  This function creates a special "matrix" object that can cache its inverse.
#
#  'x': invertible matrix
#
#  Returns: a list containing four functions for saving the matrix and its mean
#  This is a bit like object-oriented programming where these functions would be 
#  the methods.

makeCacheMatrix <- function(x = matrix()) {
  # initialize matrix inverse to NULL
  minv <- NULL
  
  # function to save the matrix
  # the superassignment is to use the parent environment of this function
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  # function to get the matrix
  get <- function() x
  
  # function to cache the inverse
  setinverse <- function(inv) minv <<- inv
  
  # function to retrieve the cached inverse
  getinverse <- function() minv
  
  # construct the list of the functions and return as this special 
  # "matrix" object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve
#  This function computes the inverse of the special "matrix" returned by 
#  `makeCacheMatrix` above. If the inverse has already been calculated 
#  (and the matrix has not changed), then it retrieves the inverse from the cache.
#
#  'x': invertible matrix
#  '...': any arguments for solve() 
#
#  Returns: a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  # get cached inverse first
  inv <- x$getinverse()

  if(!is.null(inv)) {
    # there is a cached inverse
    message("getting cached data")

  } else {
    # compute inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  }

  # return inverse
  inv
}
