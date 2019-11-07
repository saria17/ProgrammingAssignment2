## Simple implementation for caching costly computation of finding inverse of a matrix

## this function creates a special matrix object
## which is a list of certain functions that sets the value of matrix, 
## gets its value, as well as caches its inverse and gets its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(i) {
    inverse <<- i
  }
  getinverse <- function() {
    inverse
  }
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## this function is for getting cached inverse of a special matrix object if exists
## otherwise it calculates its inverse and caches with function provided by matrix object (setinverse)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
