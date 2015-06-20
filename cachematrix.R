## Assignment Week 2

## Puts the setter and getter for the Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  set <- function(y = matrix()) {
    x <<- y
    cache <<- NULL  
  }
  
  get <- function() x
  
  setInverse <- function(value) cache <<- value
  
  getInverse <- function() cache
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Takes a matrix and inverses it. 

cacheSolve <- function(y = matrix(), ...) {
  
  cache <- y$getInverse()
  if(!is.null(cache)){
    message("getting cached data")
    return(cache)
  }
  
  data <- y$get()
  cache <- solve(data, ...)
  y$setInverse(cache)
  cache
## Return a matrix that is the inverse of 'x'
}
