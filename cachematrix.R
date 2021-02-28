## The Assigment is to write the following two functions makeCacheMatrix and cacheSolve
## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function () x
  setSolution <- function(Solution) z <<- Solution
  getSolution <- function() z
  list(set = set, get = get, 
       setSolution = setSolution,
       getSolution = getSolution)
}

## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getSolution()
  if(!is.null(z)){
    message("retrieve inversed data from the cache")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setSolution(z)
  z
}
