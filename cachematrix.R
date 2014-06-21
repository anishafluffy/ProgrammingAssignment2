## These functions take a square invertible matrix as an input
## and return the inverse of the matrix. 
## The inverse will be cached for quick retrieval. 

## This function creates four functions to 1. set the matrix, 
## 2. return the matrix, 3. calculate the inverse, 
## and 4. return the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function checks to see if the inverse of the matrix
## has already been calculated. If so, the function gets the 
## inverse from the cache. If not, the function calculates 
## and returns the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
