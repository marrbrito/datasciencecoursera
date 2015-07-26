## Marcelo R. Brito: 26/07/2015

makeCacheMatrix <- function(x = matrix()) {

  # Overall, these functions create variables, some of which are defined as functions,
  # some as nested functions, and others as cached variables, and they use the functions 
  # to move these data in and out of the cache environment and to perform operations on these data.
  
  # makeCacheMatrix receives a matrix variable, and sets variables and functions in memory, 
  # and returns a list of functions nested within makeCacheMatrix.
  
  x <- NULL
  
  ## Create set function to store the matrix passed in the call as x and NULL as m, both in cache.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Create function to get/return the matrix passed in the command line call to '$set
  get <- function() x
  ## Create function to set the value of cache_i in cache to the value of local_i passed in the call to '$set_cache_i.
  setinverse <- function(inverse) i <<- inverse
  ## Create function to retrieve value of cache_i from cache and return cache_i to the caller so we can check it for NULL
  getinverse <- function() i
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
