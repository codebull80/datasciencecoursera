

## This function generates a special "matrix" object capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solveMatrix)  inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## The inverse of the "matrix" produced by makeCacheMatrix is computed with this function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
    
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv  ## Return a matrix that is the inverse of 'x'
  
  
}
       

