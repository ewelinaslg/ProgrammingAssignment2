## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv_x <- NULL
  set <- function(y) {
    x <<- y # Sets the value
    inv_x <<- NULL # Clears the cache
  }
  get <- function() x
  setInverse<- function(inverse) inv_x <<-inverse
  getInverse <- function() inv_x
  # Returns a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv_x <- x$getInverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setInverse(inv_x)
    return(inv_x)
  }
}
