## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The 
## following pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" (in fact, it is a list) object that 
## can cache its inverse. This list contains a function to set and get the value
## of the matrix and set and get the value of the its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {}
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. It first checks to see if the inverse matrix has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setmean function.
## In other words, if the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve retrieves the inverse from the cache.If it's
## not been calculated, the cachesolve calculates the inverse and stores it in 
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
