## This function need to install the Package "MASS" first to get a more general inverse matrix function ginv()
library("MASS")
## makeCacheMatrix create a special object that stores a matrix and cache's its inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) inv <<- ginv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## cacheSolve calculates the inverse of the special "vector" created with the above function

cacheSolve <- function(x, ...) {
  
   inv <- x$getinv()
if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
}
data <- x$get()
inv <- ginv(data, ...)
x$setinv(inv)
inv
  ## Return a matrix that is the inverse of 'x'
}
