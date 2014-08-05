# Title: Assignment 2
# Date: 2014-08-04
###############################################################################
# Purpose: Your assignment is to write a pair of functions that cache 
#          the inverse of a matrix.
#
# Write the following functions:
# 1. makeCacheMatrix: This function creates a special "matrix" object 
#    that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been 
#    calculated (and the matrix has not changed), then the cachesolve should 
#    retrieve the inverse from the cache.


# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y # <<- causes a search to made through parent environments 
            # for an existing definition of the variable being assigned
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m) ## Return a matrix that is the inverse of 'x'
}
