## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      # Initialize the inverse to NULL during the first call to makeVector
      inverse <- NULL
      # Function to set a new value for the underlying vector
      # this invalidates the cached inverse
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      # getter function for underlying vector in R the
      # return value of a function is the last statement.
      get <- function() {
            x
      }
      # set the solve of the vector inverse.
      setsolve <- function(solve) {
            inverse <<- solve
      }
      # returns the inverse.
      getsolve <- function() {
            inverse
      }
      # return list of functions.
      list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
      # get the solve of the vector defined inside inverse.
      inverse <- x$getsolve()
      # if we've already computed the solve and stored it via setsolve(),
      # and have not invalidated the cache by calling set(), return the cached
      # version of x.
      if(!is.null(inverse)) {
            message("getting cached data.")
            return(inverse)
      }
      # call get() to get the underlying vector.
      data <- x$get()
      # calculate the solve of the underlying vector, passing with it
      # any varargs passed to cacheSolve
      inverse <- solve(data, ...)
      # now set the solve in x so we cache it and
      # dont need to needlessly recompute it
      x$setsolve(inverse)
      # return the caching vector
      inverse
}