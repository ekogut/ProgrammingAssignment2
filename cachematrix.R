## Functions to calculate the inverse of a Matrix using cache.

makeCacheMatrix <- function(x = matrix()) {
   
      ## Initialize the inverse 
      i <- NULL
      ## To set the matrix
      set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
      }
      ## To get the matrix
      get <- function() {
            ## Return the matrix
            m
      }
      ## Set the inverse of the matrix
      setInverse <- function(inverse) {
            i <<- inverse
      }
      ## To get the inverse of the matrix
      getInverse <- function() {
            i
      }
      ## Return methods
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of x
      m <- x$getInverse()
      ## Return the inverse if its already there
      if( !is.null(m) ) {
            message("getting cached data")
            return(m)
      }
      ## Get the matrix 
      data <- x$get()
      ## Calculate the inverse
      m <- solve(data) %*% data
      ## Set the inverse to the object
      x$setInverse(m)
      ## Return the result
      m
}
