## Put comments here that give an overall description of what your
## functions do

## This function, makeCacheMatrix encapsulate a matrix in a special structure, 
# which is really a list containing four functions 

# set to stablish the value of the matrix
# get to obtain value of the matrix
# setinv to set the value of the inverse of the matrix
# getinv to retrieve the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinv <- function(invers) inverse <<- invers
      getinv <- function() inverse
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
# via the setinv function.
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
      
      
        ## Return a matrix that is the inverse of 'x'
}
