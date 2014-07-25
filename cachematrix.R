## This is library containing caching matrix. It caches inversed
## matrix so it can be effectively accessed many times without need of
## inversing it each time.

## Creates matrix wrapper. Give a matrix as a parameter.
## You can access original matrix using obj$get()

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns inversion of a given matrix. Give a matrix created
## by makeCacheMatrix as a parameter.
## This function will compute inversion only once and next calls
## will simply return cached result.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
