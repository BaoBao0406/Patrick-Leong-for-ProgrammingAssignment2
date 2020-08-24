## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeVector <- function(x = matrix()) {
  ## Initialize inverse matrix
  m <- NULL
  ## set the matrix
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## set the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  ## get the inverse of the matrix
  getInverse <- function() m
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cachemean <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ##return the inverse if its already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  m <- solve(data, ...)
  ## Set the inverse to the object
  x$setInverse(m)
  ## Return matrix
  m
}