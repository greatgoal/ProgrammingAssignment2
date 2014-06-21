## This is to complete programming assignment 2. Thank you!
## The pair of functions below will cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## Set the matrix
## Get the matrix
## Set the inverse of the matrix
## Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Set the matrix
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the matrix
  getMatrix <- function() x
  ## Set the inverse of the matrix and cache it
  setInverse <- function(inverse) m <<- inverse
  ## Get the inverse of the matrix
  getInverse <- function() m
  ## Return the list for the special matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special matrix created with the above function
## It first checks to see if the inverse has already been computed
## If so, it gets inverse and skips the computation
## Otherwise, it computes the inverse of the matrix and set the inverse in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Return the cached inverse if it is not null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix of x
  matrix <- x$getMatrix()
  ## Compute the inverse of x
  m <- solve(matrix, ...)
  ## Set the inverse of x and cache it
  x$setInverse(m)
  m
}
