## These functions take a square invertible matrix as an input and return the inverse of the matrix. 
## The inverse will be cached for quick retrieval. 

## This function creates four functions to 1. set the matrix, 2. return the matrix, 
## 3. calculate the inverse, and 4. return the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## m is the inverse of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  } ## function to set value of matrix
  get <- function() x ## function to return value of matrix
  setinverse <- function(solve) m <<- solve ## function to compute inverse of matrix
  getinverse <- function() m ## function to return inverse of matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## list of four functions
}

## This function checks to see if the inverse of the matrix has already been calculated. 
## If so, the function gets the inverse from the cache. 
## If not, the function calculates and returns the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## returns inverse using functions in makeCacheMatrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } ## if there's no computed inverse already, print "getting cached data"
  data <- x$get() ## get the matrix from function in makeCacheMatrix
  m <- solve(data, ...) ## compute the inverse of the matrix
  x$setinverse(m) ## cache the inverse
  m ## return the inverse
}
