## Function makeCacheMatrix creates a special 'matrix' object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
##- get returns vector x stored.
##- set changes the vector stored.
##- setinv and getinv store input value m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function cacheSolve returnes the inverse of 'matrix' from cache if it is has already been calculated. 
## If not m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("got cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
