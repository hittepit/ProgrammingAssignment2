## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInversed <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedInverse <<- inverse
  getinverse <- function() cachedInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("cache hit")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}