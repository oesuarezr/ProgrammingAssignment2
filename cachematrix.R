## These two functions can help avoiding the costly computation process of matrix inversion
## by cathing its reverse instead of computing it

## This function can creates a matrix which can cache its inverse (invs)
## 
makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix created above
## If the inverse has been calculated, this function rerieve it from the cache.
cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if (!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}
