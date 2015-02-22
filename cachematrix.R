## users can create a matrix from scratch and then inverse it or retrieve
## a cached inversed matrix if it is available or calculate if it is not

## This function creates new matrix and adds several methods for getting
## and setting values (creating new matrix or caching inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
  # creating empty matrix
  m <- NULL
  # creating a setter for a matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # getter for a matrix
  get <- function() x
  # caching an inversed matrix
  setInverse <- function(inverse) m <<- inverse
  # getter for an inversed matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function retrieves a cached inverse matrix for the specified one
## or computes it if there is no cached version of it.

cacheSolve <- function(x, ...) {
  # getting inverse matrix
  m <- x$getInverse()
  # checking whether inverse matrix was computed before and returns it
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  # getting matrix 
  data <- x$get()
  # creating an inverse matrix using the matrix got in the previous action
  m <- solve(data)
  # sending to the storage the cached version of inversed matrix
  x$setInverse(m)
  ## Return a matrix that is the inverse of 'x'
  return(m)
}