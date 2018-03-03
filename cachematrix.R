## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# # The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 
# set the values of the matrix
# get the values of the matrix
# set the values of the inverse
# get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
            
      }
      get <- function() x
      setsolve <- function(solve) inv <<- solve
      getsolve <- function() inv
      list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
      
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsikve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getsolve()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setsolve(inv)
      inv
}

