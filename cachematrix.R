
##Usually Matrix inversion is a costly computation and so rather than computing it
##over and over, there can be some benefit to caching the inverse of a matrix.

##these functions are used to create a special object that stores a matrix and caches
##its inverse.

## the makeCacheMatrix creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
}
  get <- function () x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function () inv
  list (set = set, get = get, setInverse = setInverse, 
        getInverse = getInverse)}


## this function takes what is produced by the 'makeCacheMatrix' function (a special 
## "matrix") and computes the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse ()
  if (!is.NULL(inv)) {
    message ("getting cached data")
    return (inv)
  }
  m <- x$get ()
  inv <- solve (m, ...)
  x$setInverse(inv)
}
