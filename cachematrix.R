## Solution for JH R programming assignment #2
##Contains two functions, one that creates a cache matrix and one that retrieves its inverse


## This creates a cache that holds the value of an inverse of an invertible matrix
## so that for a given matrix, it is only computed once and stored, rather than recomputed every time
## R uses the function solve(mtrx) to compute the inverse of an invertible matrix mtrx.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
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


## This retrieves the inverse of a matrix that has been created by the previous function
## If the inverse has already been computed, it is retrieved from the cache.
##  If the inverse has not yet been computed, it is computed and then stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
