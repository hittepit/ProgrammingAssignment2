## These two functions may be use to efficiently compute the inverse of a matrix
## by using a system of caching


## makeCacheMatrix returns a list of functions written to manipulate a cached matrix
## Warning: makeCacheMatrix does not compute anything, it only manages caching
makeCacheMatrix <- function(x = matrix()) {
  #The cached matrix, NULL if nothing is cached
  cache <- NULL
  
  #Set function stores the base matrix and clears the cache
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  #get function returns the base matrix
  get <- function() x
  
  #setcache function assigns a matrix to the cache
  setcache <- function(c) cache <<- c
  
  #getcache function returns the cached matrix
  getcache <- function() cache
  
  #returns a list of functions
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## Function that computes the inverse of the base matrix wrapped by the makeCacheMatrix
## If the inverse has already been computed and cached, it returns a cached value
## else it computes the inverse and stores it in the cache before returning it
cacheSolve <- function(x, ...) {
  # Get the cached value (if any)
  m <- x$getcache()
  
  #If a cached value exists, returns it
  if(!is.null(m)){
    message("cache hit")
    return(m)
  }
  
  #If no cached matrix is found, retrieves the base matrix
  data <- x$get()
  m <- solve(data)
  x$setcache(m) #Caches the result
  m
}