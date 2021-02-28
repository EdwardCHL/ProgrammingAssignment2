## The Assigment is to write the following two functions makeCacheMatrix and cacheSolve
## functions do

## makeCacheMatrix is a function that creates a Matrix that can cache its inverse for input.

makeCacheMatrix <- function(x = matrix()) {
 z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function () x
  setInverse <- function(Inverse) z <<- Inverse
  getInverse <- function() z
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
  if(!is.null(z)){
    message("retrieve inversed data from the cache")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setInverse(z)
  z
}
