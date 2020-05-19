## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Functions for getting and setting cached inverse matrix value

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()               #matrix stored in data
  inverse <- solve(data, ...)   #Actual inverse done in this step
  x$setinverse(inverse) 
  inverse
}
