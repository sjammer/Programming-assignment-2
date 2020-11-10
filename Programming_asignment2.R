#First function is created to store a matrix and caches its inverse

CacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Computes the inverse matrix. 
#Inspired by the mean calculation example presented in the assignment.

## Returns a matrix that is the inverse of 'x'
cacheInverse <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}