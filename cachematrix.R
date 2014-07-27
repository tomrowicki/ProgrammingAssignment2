## Put comments here that give an overall description of what your
## functions do

## The first function creates a cache matrix that can cache its inverse,
## in a way analogous to the makeVector function
## presented in the task's description. It makes it possible to
## set/get the matrix values and set/get the inverse values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The below function computes the inverse of the special matrix created above,
## checking first if the inverse is not already stored in the cache. It it isn't,
## the function attempts the calculation and sets the inverse appropriately.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        
}
