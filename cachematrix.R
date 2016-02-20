## Put comments here that give an overall description of what your
## functions do

## Returns a list of functions that hold lexically scoped variables for the matrix value 
## and potentially it's inverse. Setter/getters are created for this function to hold 
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  getInverse <- function() inv
  setInverse <- function(i) inv <<- i
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function() x
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## Given a cacheMatrix, solve for the inverse if it's null or return the cached value

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cache data")
        return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
