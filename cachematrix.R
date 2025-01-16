## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is updated
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached data...")
    return(inv)  # Return cached inverse
  }
  
  # Get the matrix
  mat <- x$get()
  
  # Compute the inverse
  inv <- solve(mat, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
