## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(nrow = 0, ncol = 0)
  set <- function(y) {
    x <<- y
    m <<- matrix(nrow = 0, ncol = 0)
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
   list (set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes inverse of the special "matrix" returned by makeCacheMatrix. 
## If inverse has been calculated and matrix is unchanged, then cachesolve should retrieve inverse from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(ncol(m)!=0) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
