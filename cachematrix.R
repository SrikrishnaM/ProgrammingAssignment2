## makeCacheMatrix creates a special matrix object that holds the actual matrix as a member variable and inverse
## of it as another member variable and provides getter/setter methods. Simply put, makeCacheMatrix is an object 
## encapsulating the matrix and the inverse.

## Parameter: x - Matrix to be encapsulated - the inverse of which should be cached.
## Return the special matrix that encapsulates 'x' and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setInverse = function(inverse) inv <<- inverse
  getInverse = function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix. It first checks if there is a cached 
## version of inverse of the matrix. If there exists a cached copy, it returns the same; Otherwise, it calculates
## the inverse and stores the inverse in the special matrix.

## Parameter: x - Matrix object obtained by makeCacheMatrix method
## Return a matrix that is the inverse of 'x'
cacheSolve = function(x, ...) {
  inv = x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setInverse(inv)
  inv
}
