## Assignment Week 2

## Puts the setter and getter for the Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL                                 # starts off the function by setting the cache to NULL.
  set <- function(y = matrix()) {               # set the matrix value.
    x <<- y
    cache <<- NULL  
  }
  
  get <- function() x                           # get the matrix value
  
  setInverse <- function(value) cache <<- value # set the inverse of the matrix to the cache
  
  getInverse <- function() cache                # get the value from the cache. If nothing has been set, the default value is NULL.
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #returns a list with names to call the functions

}


## Takes a matrix and inverses it. 

cacheSolve <- function(y = matrix(), ...) {
  
  cache <- y$getInverse()                       # Retrieves the cache value and returns it if it's not NULL
  if(!is.null(cache)){
    message("getting cached data")
    return(cache)
  }
  
  data <- y$get()                               # If cache = NULL, then find the inverse of the matrix and store it in the cache.
  cache <- solve(data, ...)
  y$setInverse(cache)
  cache
## Return a matrix that is the inverse of 'x'
}
