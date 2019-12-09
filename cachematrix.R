
## This function "makeCacheMatrix" creates a special matrix, which stores
## the value of the input matrix and its inverse, using
## the get and set functions.

makeCacheMatrix <- function(x = matrix()) {
  
  ## For Inverse Matrix, which should be null initially.
  i <- NULL
  
  ## Set function for setting the value of the matrix and inverse.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get function for getting the value of the matrix.
  get <- function() x
  
  ## Set and Get function for Inverse of the matrix.
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  ## Special matrix as input argument to the cache solve function.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function checks whether the input matrix already has
## its inverse cached. If it is, it returns the value of inverse
## with a "Getting cached data" message. Otherwise, it calculates
## the inverse of input matrix, and returns its value.

cacheSolve <- function(x, ...) {
  
  ## Gets the value of inverse matrix from the makeCacheMatrix function.
  i <- x$getinverse()
  
  ## Checks if the inverse of the matrix is already solved.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Gets the data using the get function.
  data <- x$get()
  
  ## Inverse is calculated using the solve function.
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## i returns a matrix that is the inverse of 'x'
  i        
}
