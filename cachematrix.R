## Defines two functions to implement caching for matrix inversion.
##

## Given a matrix 'x', create a cacheable matrix. Pass the returned object to 'cacheSolve' to get the inverse.
## The return object is a list of four(4) functions :
##  get() -> Return the matrix 'x'
##  set(y) -> Update 'x' to 'y' and clear the cache
##  getinverse() -> Return the cached inverse of 'x'
##  setinverse(i) -> Set the inverse of 'x' to 'y'. This function shall not be call outside of 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
}


## Takes a cacheable matrix created with 'makeCacheMatrix'. Return the inverse of the matrix 'x' either from the cache or after using 'solve'

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting result from cache")
    return(inverse)
  }
  real_matrix <- x$get()
  inverse <- solve(real_matrix, ...)
  x$setinverse(inverse)
  return(inverse)
}
