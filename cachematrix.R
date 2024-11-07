makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse to NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when the matrix changes
  }
  
  get <- function() {
    ## Return the matrix
    x
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() {
    ## Return the cached inverse
    inv
  }
  
  ## Return a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(cacheMatrix) {
  ## Retrieve the cached inverse
  inv <- cacheMatrix$getInverse()
  
  if (!is.null(inv)) {
    ## Return the cached inverse if it is not NULL
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our cacheMatrix object
  data <- cacheMatrix$get()
  ## Calculate the inverse
  inv <- solve(data, tol = 1e-10)  # tol is the tolerance; adjusts the pivot elements in Gaussian elimination
  ## Cache the inverse
  cacheMatrix$setInverse(inv)
  
  inv  # Return the inverse
}
