## The two functions below are used to create a special object that
##  caches the inverse of a matrix. It is assumed that the matrix is
##  always invertible.


## makeCacheMatrix: This function creates a special "matrix" object
##  that can cache its inverse.
##  1 - set the matrix
##  2 - get the matrix
##  3 - set the inverse of the matrix
##  4 - get the inverse of the matrix
## Result is the creation of a list which is used by cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  
  imat <- NULL
  
  set <- function(y) {
    x <<- y
    imat <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) imat <<- inverse
  
  getinverse <- function() imat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix"
##  created with the makeCacheMatrix function above.
##  It checks if the inverse has already been calculated (and that the
##  matrix has not changed).
##  If the inverse has already been calculated then cacheSolve will
##  retrieve the inverse from the cache.
##  If not, cacheSolve will calculate the inverse matrix.

cacheSolve <- function(x, ...) {
  
  imat <- x$getinverse()
  
  if(!is.null(imat)) {
    message("getting cached data")
    return(imat)
  }
 
  matrixdata <- x$get()
  imat <- solve(matrixdata, ...)
  
  x$setinverse(imat)
  
  imat
}
