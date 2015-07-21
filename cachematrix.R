## A pair of functions that cache the inverse of a matrix

## This first function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initialize the inverse with NULL
  set <- function(y) {
    x <<- y  # cache the new matrix
    m <<- NULL  # the new inverse is not calculated yet, so null the old one
  }
  get <- function() x  # get the cached matrix
  setinverse <- function(inverse) m <<- inverse  # cache the inverse
  getinverse <- function() m # return the cached inverse
  # This function is really a list containing 4 functions to
  #   set the value of the matrix
  #   get the value of the matrix
  #   set the value of the inverse
  #   get the value of the inverse
  list(set = set, get = get,     
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get() # get the matrix
  m <- solve(data, ...) # calculate the inverse
  x$setinverse(m) # cache it
  m  # return it
}
