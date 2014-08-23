## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that could cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property of matrix
  j <- NULL
  
  ##the method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    j <<- NULL
  }
  
  ## the method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## the method to set the inverse of the matrix
  setInverse <- function(inverse) {
    j <<- inverse
  }
  
  ## the method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    j
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the specific matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and also the matrix has not
## been changed), then the "cachesolve" should get the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}