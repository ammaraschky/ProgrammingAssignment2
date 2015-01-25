
# 'makeCacheMatrix()' is a function that returns a list of functions.
# Its purpose is to store a matrix and a cached value of the inverse of the matrix. 
#
# It contains the following functions:
# - setMatrix      set the value of a matrix
# - getMatrix      get the value of a matrix
# - cacheInverse   set the cached value (inverse of the matrix)
# - getInverse     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
      
      # reset cache, set it to NULL
      cache <- NULL
      set <- function(y) {
            x <<- y
            # since the matrix is assigned a new value, clear the cache
            cache <<- NULL
      }
      # returns the stored matrix
      get <- function() x
      
      # cache the given argument 
      setInverse <- function(solve) {
            cache <<- solve
      }
      # get the cached value
      getInverse <- function() {
            cache
      }
      
      # return a list where each named element of the list is a function
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# The next function calculates the inverse of the matrix created with makeCacheMatrix
# but it only performs this task once, making it quickly repeatable. 

cacheSolve <- function(x, ...) {
      # get the cached value
      inverse <- x$getInverse()
      # if a cached value exists return it
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      # else get the matrix; caclulate the inverse and store it in the cache
      
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
