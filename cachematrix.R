## There are two functions: makeCacheMatrix and cacheSolve.
#   The purpose is to calculate the inverse of a matrix and cache it in a
#   variable. If the inverse has already been calculated, then call the
#   variable with the cached inverse matrix.

## makeCacheMatrix can do 4 things:
#   1: set the value of the matrix
#   2: get the value of the matrix
#   3: set the inverse of the matrix
#   4: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix from makeCacheMatrix.
#   It will check if the matrix has already been calculated. If so, it skips
#   the calculation and gets the cached inverse. Otherwise, it calculates
#   the inverse of the matrix and caches the value using the setinverse
#   function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
