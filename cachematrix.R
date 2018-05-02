## Put comments here that give an overall description of what your
## functions do
## Used for calculating the inverse f a square matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Calculates inverse of a matrix, but uses 
  ## the cached versionb if no change
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse has already been calculated 
        ## it uses the cashed version
    inv <- x$getinv()
    if (!is.null(inv)) {
      message("getting cached result")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
