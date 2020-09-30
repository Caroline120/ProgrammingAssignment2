## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( p = matrix() ) {
  
  ## Initialize the inverse property
  o <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    p <<- matrix
    o <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    p
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    o <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    o
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  p <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(p) ) {
    message("getting cached data")
    return(p)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  p <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(p)
  
  ## Return the matrix
  p
}

