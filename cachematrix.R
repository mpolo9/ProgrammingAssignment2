## This function is designed to create a matrix object that is able to cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
## First, values are set for the matrix, in an environment different from the current environment.
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
## Second, I get the values of the matrix
  get <- function() x
## Third, I set the values of the inverted matrix
  setInverse <- function(inverse) inv <<- inverse
## Finally, I get the matrix in the form of a list
  getInverse <- function() inv
  list(set = set,
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The following function computes the inverse of the matrix created above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
## If the inverse has already been calculated (and the 
  ## matrix has not changed), then it should return the inverse from the cache and skips computation.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## Otherwise, it calculates the inverse of the matriz and sets the resultant value in the cache via the setInverse function.
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
