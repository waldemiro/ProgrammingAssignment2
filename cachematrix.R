## Function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) inverse <<- solve
      getsolve <- function() inverse
      list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

cacheSolve <- function(x, ...) {
      inverse <- x$getsolve()
      if(!is.null(inverse)) {
            message("getting cached data.")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setsolve(inverse)
      inverse
}