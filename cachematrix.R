## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  # Getter function to retrieve the matrix
  get <- function() {
    x
  }
  
  # Getter function to retrieve the cached inverse if available, otherwise compute and cache the inverse
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  # Getter function to retrieve the cached inverse
  getinv <- function() {
    inv
  }
  
  # Return a list of functions representing the special matrix object
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...){
  inv <- x$getInverse()  # Try to retrieve the cached inverse
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)  # Return cached inverse if available
  }
