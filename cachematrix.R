## The following two functions are used in the computation
## of a matrix's inverse or the retrieval of a previously
## computed inverse of the matrix.

## This function creates a matrix used to cache its inverse
## using the format of the provided example "makeVector".

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse the matrix created using
## the funtion "makeCacheMatrix" or retrieves the inverse from
## the inv variable if not null. This format was adopted from
## the "cachemean" example provided.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached inverse....")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
} 